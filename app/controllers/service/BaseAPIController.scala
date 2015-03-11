package controllers.service

import com.wordnik.swagger.core.util.ScalaJsonUtil
import scala.language.postfixOps
import play.api.http._
import play.api.libs.json._
import play.api.mvc._
import java.io.StringWriter
import controllers.service.CommonKeys._
import controllers.service.ConfigurationSetup._
import scala.concurrent.Future
import controllers.jobs.LogActor
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import reactivemongo.api.Cursor
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Await
import scala.concurrent.duration._
import models.auth.SecurityKey
import models.beans.EnumTableList.PASS_COLLECTOR
import play.Logger

//For cache
import play.api.Play.current
import play.api.cache.Cache

class BaseApiController extends Controller with MongoController{
  
  def getOptions(path: String) = Action {
    implicit request =>  
      JsonResponse(Ok(Json.obj()), 3600)
  }
  
  // Define json response with all the needed headers (checked by X scripting)
  protected def JsonResponse(result: Result):Result = {
    result.withHeaders(
         
              //play.api.http.HeaderNames.ORIGIN
        
        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS))
  }
  
  protected def JsonResponse(result: Result, age: Long):Result = {
    JsonResponse(result).withHeaders(CACHE_CONTROL -> ("max-age:"+age))
  }
  
    
  /**
   * Specific method created only to validate if the cookie is correct. This needs to be enhanced to do complex algorithm.
   */
  def AuthorizeUser[A] (bp: BodyParser[A], authLevel:Int = AUTH_DEFAULT_LVL)(f:Request[A] => Result) :Action[A] = {
    Action(bp) { request =>
    	try{
    		val userId = request.session(USER_ID)
    		val oType = request.session(OTYPE)
    		val cpId = request.session(CP_ID)
    		val publicKey = request.session(PUBLIC_KEY)
    		if(validateUser(userId, oType, publicKey, cpId, authLevel))
    			f(request)
    		else
    		  JsonResponse(Unauthorized(views.html.help("You are not authorized.")))
    	}catch{
    	  case e:Exception => {
    	    LogActor.logActor ! "Error Code :" + e.getMessage() 
    	    JsonResponse(Unauthorized(views.html.help("You are not authorized.")))
    	  }
    	}
    }
  }
  
  /**
   * Specific method created only to validate if the cookie is correct. This needs to be enhanced to do complex algorithm.
   */
  def AuthorizeAsyncUser[A] (bp: BodyParser[A], authLevel:Int = AUTH_DEFAULT_LVL)(f:Request[A] => Future[Result]) :Action[A] = {
    Action.async(bp) { request =>
    	try{
    		val userId = request.session(USER_ID)
    		val oType = request.session(OTYPE)
    		val cpId = request.session(CP_ID)
    		val publicKey = request.session(PUBLIC_KEY)
    		if(validateUser(userId, oType, publicKey, cpId, authLevel))
    			f(request)
    		else
    		  Future.successful(JsonResponse(Unauthorized(views.html.help("You are not authorized."))))
    	}catch{
    	  case e:Exception => {
    	    LogActor.logActor ! "Error Code :" + e.getMessage() 
    	    Future.successful(JsonResponse(Unauthorized(views.html.help("You are not authorized."))))
    	  }
    	}
    }
  }
  
  /**
   * Validate Username and password validity.
   */
  private def passCollection: JSONCollection = db.collection[JSONCollection](PASS_COLLECTOR.toString())
  
  case class PasswdCollCursor(userId:String, privateKey:String, cpId:Option[String], authLevel:Option[Int])
  
  implicit val passwdCollRWZ = Json.format[PasswdCollCursor]
  
  private def validateUser(userName:String, oType:String, publicKey:String, cpId:String, authLevel:Int) = {
    val userId = userIDCombination(oType,userName,false);
    
    val loginResult = Cache.getOrElse(userId, 3600)({
	    val query = Json.obj("userId" -> userId)
	    val cursor:Cursor[PasswdCollCursor] = passCollection.find(query).cursor[PasswdCollCursor]
	    val futureList: Future[List[PasswdCollCursor]] = cursor.collect[List](1)
	    
	    val result = Await.result(futureList, 3 seconds)
	    
	    result
    })
    
    val validated = if(loginResult.length == 0){
				        false
				      }else{
				        val loginSelected = loginResult(0)
				        
/*Same public keys*/    (validatePassword(publicKey,userId, loginSelected.privateKey) &&
						validateAuthLevel(authLevel, loginSelected.authLevel) &&
						validateCpId(cpId, loginSelected.cpId.getOrElse(EMPTY_CPID)))
				      }
    
      
    validated
  }
  
  private def validatePassword(publicKey:String, userId:String, privateKey:String) = {
    publicKey == SecurityKey.encode(userId, privateKey)
  }
  
  private def validateCpId(cpId:String, resultCP:String) = {
    cpId == resultCP
  }
  
  /**
   * Validate base on bitwise operation. first 1 is always true.
   * Make sure hack with negative is filtered as & comparator is used.
   */
  private def validateAuthLevel(authLevel:Int, resultLevel:Option[Int]) = {
    val authLvlFromResult = resultLevel.getOrElse(AUTH_DEFAULT_LVL)
    
    if(authLvlFromResult > 0) 
    	((authLevel & authLvlFromResult) != 0)
    else
    	false
  }
  
  def insertPassIntoDB(userId:String, privateKey:String, cpId:String, authLevel:Int){
    import reactivemongo.core.commands.GetLastError
    
    Cache.remove(userId)
    passCollection.update(Json.obj( "userId" -> userId), (Json.obj( "userId" -> userId, "privateKey" -> privateKey, "cpId" -> cpId, "authLevel" -> authLevel)), GetLastError(), upsert = true, multi = false)
    //If it fails let it be, job is done trying.
  }
  
  def removePassFromDB(userId:String){
    import reactivemongo.core.commands.GetLastError;
    
    Cache.remove(userId)
    passCollection.remove(Json.obj( "userId" -> userId), firstMatchOnly = true)
    //If it fails let it be, job is done trying.
  }
  
  /**
   * User combination.
   */
  def userIDCombination(oType:String, userId:String):String = {
    getBaseOType(oType)+userId
  }
  
  def userIDCombination(oType:String, userId:String, filterBase:Boolean):String = {
    if(filterBase == false)
      oType+userId
    else
      getBaseOType(oType)+userId
  }
  
  def getBaseOType(oType:String):String = {
    if(oType.startsWith(DEVICE_APP)){
      oType.substring(DEVICE_APP_LENGTH)
    }else{
      oType
    }
  }
}

object BaseApiController{}
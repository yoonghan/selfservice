package controllers.service.rest

import play.api._
import play.api.mvc._
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import play.modules.reactivemongo.json.collection.JSONCollection
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import models.beans.SubscriptionModel._
import play.api.Play.current
import play.api.libs.functional.syntax._
import scala.concurrent.Future
import reactivemongo.api._
import controllers.service.CommonKeys._
import com.wordnik.swagger.annotations.Api
import com.wordnik.swagger.annotations.ApiOperation
import com.wordnik.swagger.annotations.ApiResponses
import com.wordnik.swagger.annotations.ApiResponse
import scala.concurrent.duration._
import reactivemongo.core.commands.GetLastError
import play.api.data.Forms._
import reactivemongo.core.commands.Aggregate
import reactivemongo.core.commands.Match
import reactivemongo.core.commands.Project
import reactivemongo.bson.{BSONInteger,BSONArray,BSONBoolean,BSONDocument,BSONObjectID}
import reactivemongo.core.commands.RawCommand
import models.beans.EnumTableList.{SUBSCRIPTION,USER } 
import controllers.jobs.LogActor

@Api(value = "/subscription", description = "Subscription Information")
object SubscriptionController extends BaseApiController {
  
  def subscriptionCollection: JSONCollection = db.collection[JSONCollection](SUBSCRIPTION.toString())
  def userCollection: JSONCollection = db.collection[JSONCollection](USER.toString())
	
  /**
	* Query all subscription list.
  */
  @ApiOperation(
    nickname = "getSubscriptionList", 
    value = "SubscriptionList", 
    notes = "Returns available subscriptionList", 
    response = classOf[SubscriptionHost],
    responseContainer = "List",
    httpMethod = "GET"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def hostlist = AuthorizeAsyncUser(BodyParsers.parse.anyContent){request =>    
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)
	val query = Json.obj("del" -> false);
	val userAuth = userIDCombination(oType, userId)
	
	/**HALTED - This is only working in Mongo 2.6 onwards**/
//	val command = Aggregate ("subscription", Seq(
//	    Match(BSONDocument("status" -> 1)),
//	    
//    	Project(
//    	    ("id",BSONInteger(1)),
//    	    ("cName",BSONInteger(1)),
//    	    ("cDesc",BSONInteger(1)),
//    	    ("subscribed",
//    	        BSONDocument("$setIsSubset"->BSONArray(
//    	            BSONArray(userAuth), "$subs")))
//    	    )
//    	 ))
//    /**
//     * --Query Execution
//     * db.subscription.aggregate([{$match:{status:1}},{$project:{"userId":1, "cId":1,"subs":1, discount:{$setIsSubset:[["G114852108498604797792"],"$subs"]}}}])
//	**/	    
//    	    
//    import play.modules.reactivemongo.json.BSONFormats._
//	val result = db.command(command)
//	    
//    val futureSubscriptionList = result.map { value => {
//        value.toSeq map (Json.toJson(_).as[SubscriptionReg])
//      }}
//
//    futureSubscriptionList.map { subscriptionList =>
//      subscriptionList.size match {
//        case 0 => JsonResponse(NotFound(Json.obj()))
//        case _ => {
//          JsonResponse(Ok(Json.toJson(subscriptionList)))
//        }
//      }
//    }
    /**HALTED - This is only working in Mongo 2.6 onwards**/
	/**REPLACEMENT until Mongo 2.6 works[S]**/
	val cursor: Cursor[SubscriptionHost] = subscriptionCollection.find(Json.obj()).cursor[SubscriptionHost]

    val futureSubscriptionReg: Future[List[SubscriptionHost]] = cursor.collect[List]()
	
    futureSubscriptionReg.map { subscriptionList =>
      subscriptionList.size match {
        case 0 => JsonResponse(NotFound(Json.obj()))
        case _ => {
		      val returnedList = subscriptionList.map( subList => {
		        
		        val isSubscribed =  subList.subs.getOrElse(Nil).contains(userAuth)
		        
		        SubscriptionReg(
		            subList.id ,
		            subList.cName ,
		            subList.cDesc ,
		            isSubscribed
		            )
		      })
		      JsonResponse(Ok(Json.toJson(returnedList)))
        }
      }
      /**REPLACEMENT until Mongo 2.6 works[E]**/
    }
    

  }
  
	
	/**
	 * Update subscription list
	 */
  @ApiOperation(
    nickname = "updSubscriptionList", 
    value = "JSONResponse", 
    notes = "Update user subscribed on list subscribed", 
    response = classOf[String],
    httpMethod = "POST"
    )
  @ApiResponses(Array(new ApiResponse(code = 401, message = "User had no authorities to access"))) 
  def updateHostList = AuthorizeAsyncUser(BodyParsers.parse.json){request =>    
    
	val userId = request.session(USER_ID)
	val oType = request.session(OTYPE)
	val query = Json.obj("del" -> false);
	val userAuth = userIDCombination(oType, userId)
	
	val subscribedIds = request.body.validate[SubscribedIds]
	
	subscribedIds.fold(
		errors => {
		          Logger.info(errors.toString)
		        	Future.successful(JsonResponse(BadRequest("Unexpected Request, what have you sent?")));
		        },
		subId => {
	/**HALTED - This is only working in Mongo 2.6[S] onwards**/	  
//		  val remapSub = subId.subs.map( x  => BSONObjectID(x))
//		  val remapUnSub = subId.unsubs.map( x  => BSONObjectID(x))
//		  
//		  val ins_query = BSONDocument("_id" -> BSONDocument("$in"-> (remapSub)))
//		  val del_query = BSONDocument("_id" -> BSONDocument("$in"-> (remapUnSub)))
//		  
//		  val ins_conJsonObj = BSONDocument("$addToSet" -> BSONDocument("subs" -> userIDCombination(oType,userId)))
//		  val del_conJsonObj = BSONDocument("$pull" -> BSONDocument("subs" -> userIDCombination(oType,userId)))
//		  
//		  val commandDoc =
//	        BSONDocument(
//	          "update" -> subscription_CollectionName,
//	          "updates" -> BSONArray(
//	            BSONDocument("q" -> ins_query, "u" -> ins_conJsonObj, "upsert"->false, "multi"->true),
//	            BSONDocument("q" -> del_query, "u" -> del_conJsonObj, "upsert"->false, "multi"->true)
//	          )
//	        )
//	        
//	        /**
//	         * Example query
//	         * db.runCommand({update:"subscription", updates: [ 
//					{q:{cId:{$in:[]}}, u: {$addToSet: {subs: "G114852108498604797792"}}},
//					{q:{cId:{$in:["Id1", "Id2", "Id1", "Id1", "Id1", "Id11"]}}, u: {$pull: {subs: "G114852108498604797792"}},upsert:false, multi:true}
//				]})
//	         */
//	        
//	        val updateRec = db.command(RawCommand(commandDoc))
//	        updateRec.onFailure{
//	            case f => {
//	            	//manually handle exception bug https://groups.google.com/forum/#!topic/reactivemongo/xKWO8aXbYnE
//	            	if(f.getMessage().contains("writeError")){
//	            	  f.printStackTrace()
//	            	}      
//	            }
//			  }
//				
//			
//		
	/**HALTED - This is only working in Mongo 2.6[E] onwards**/	
	/**REPLACEMENT until Mongo 2.6 works[S]**/
		  
		  val remapSub = subId.subs.map( x  => Json.obj("$oid"-> x))
		  val remapUnSub = subId.unsubs.map( x  => Json.obj("$oid"-> x))
		  
		  val insQuery = Json.obj("_id" -> Json.obj("$in" -> remapSub ))
		  val delQuery = Json.obj("_id" -> Json.obj("$in" -> remapUnSub ))
		  val ins_conJsonObj = Json.obj("$addToSet" -> Json.obj("subs" -> userIDCombination(oType,userId)))
		  val del_conJsonObj = Json.obj("$pull" -> Json.obj("subs" -> userIDCombination(oType,userId)))
		  val futureIns = subscriptionCollection.update(insQuery, ins_conJsonObj, GetLastError(), upsert = false, multi = true)
		  val futureDel = subscriptionCollection.update(delQuery, del_conJsonObj, GetLastError(), upsert = false, multi = true)
		  
		  futureIns.onFailure{
		    case f => {
		      LogActor.logActor ! "Error insertion on subscription for: "+userId+" >> "+f.getMessage()  
		      f.printStackTrace();
		    }
		  }
		  futureDel.onFailure{
		    case f => {
		      LogActor.logActor ! "Error deleting subscription for: "+userId+" >> "+f.getMessage()  
		      f.printStackTrace();
		    }
		  }
	/**REPLACEMENT until Mongo 2.6 works[E]**/
		  Future.successful(JsonResponse(Created(Json.obj("success"->"ok"))))
	})
  }
  
  def createSubscription(userId:String, oType:String, authLvl:Int, sub:Subscription):Future[Boolean]={
    
    val userAuth = userIDCombination(oType, userId);
    
    val query = Json.obj("$or"->Json.arr(
        Json.obj("userId" -> userAuth),Json.obj("cName" -> sub.cName.trim())
        )) 
    val cursor: Cursor[SubUserIdMapCpId] = subscriptionCollection.find(query).cursor[SubUserIdMapCpId]

    val futureQuerySubscription: Future[List[SubUserIdMapCpId]] = cursor.collect[List](1)
    futureQuerySubscription.flatMap{ queryRec =>
      if(queryRec.size==0){
        
        val bsonId = BSONObjectID.generate.stringify 
        val createSub = Subscription_Creation(bsonId, sub.cName , sub.cDesc , sub.cWebsite ,sub.cCtcNo,sub.cEmail , List(userAuth), userAuth, 2, 0)
	    val futureInsSubscription = subscriptionCollection.insert(createSub)
	    futureInsSubscription.flatMap(
	        status => {
	          if(status.ok){
	            val query = Json.obj("id" -> userId, "otype" -> oType)
	            val updateVal = Json.obj("$set"->Json.obj("cpId" -> bsonId,"authLevel" -> ( AUTH_CAL_CREATE_LVL | authLvl )))
	            val futureUpdateUser = userCollection.update(query, updateVal, GetLastError(), false, false)
	            futureUpdateUser.map{
	              status =>
	              if(status.updated == 1){
	                val query = Json.obj("_id" -> Json.obj("$oid" -> bsonId))
	                val updateVal = Json.obj("$set"->Json.obj("status" -> 1))
	                subscriptionCollection.update(query, updateVal, GetLastError(), false, false)
	                true
	              }else{
	                false
	              }
	            }
	          }else
	            Future.successful(false)
	        }
	    )
      }else{
        Future.successful(false)
      }
    }
    
    
    
  }
}


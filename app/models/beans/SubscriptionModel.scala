package models.beans

import play.api.libs.json._
import play.api.data.Forms.{mapping, nonEmptyText, text, optional, email, number}
import controllers.service.FormValidator

object SubscriptionModel {
  
  private val wordExpr = """^([A-Z|a-z|0-9]+\s{0,1}[A-Z|a-z|0-9]*)*"""
  private val contactNoExpr = """^\+?[0-9]+$"""
  private val urlExpr = """^(https?:\/\/)?([\da-z\.-]+)\.([a-z\.]{2,6})([\/\w \.-]*)*\/?$"""
  
  case class Subscription_Edit(
      cDesc:String,
      cWebsite:Option[String],
      cCtcNo:Option[String],
      cEmail:Option[String],
      ver:Int
      )
      
  case class Subscription_Creation(
      id:String,
      cName:String,
      cDesc:String,
      cWebsite:Option[String],
      cCtcNo:Option[String],
      cEmail:Option[String],
      subs:List[String],
      userId:String,
      status:Int,
      ver:Int
      )
      
  case class Subscription(
      cName:String,
      cDesc:String,
      cWebsite:Option[String],
      cCtcNo:Option[String],
      cEmail:Option[String],
      ver:Int
      )
  
  case class SubscriptionHost(
      userId:String,
      id:String,      
      cName:String,
      cDesc:String,
      subs:Option[List[String]],
      status:Int)
      
  case class SubscriptionReg(
      id:String,
      cName:String,
      cDesc:String,
      subscribed:Boolean)
      
  case class SubUserIdMapCpId(
      id:String
      )
      
  case class SubscribedIds(
      subs:List[String],
      unsubs:List[String])
      
  def mongoReads[T](r: Reads[T]) = {
    __.json.update((__ \ 'id).json.copyFrom((__ \ '_id \ '$oid).json.pick[JsString] )) andThen r
  }
 
  def mongoWrites[T](w : Writes[T]) = {
    w.transform( js => js.as[JsObject] - "id"  ++ Json.obj("_id" -> Json.obj("$oid" -> js \ "id")) )
  }
  
  implicit val subFormat = Json.format[Subscription]
  implicit val subEditFormat = Json.format[Subscription_Edit]
  implicit val subCreateWrites:Writes[Subscription_Creation] = mongoWrites[Subscription_Creation](Json.writes[Subscription_Creation])
  
  implicit val subListRead:Reads[SubscriptionHost] = mongoReads[SubscriptionHost](Json.reads[SubscriptionHost])
  implicit val subListWrites:Writes[SubscriptionHost] = mongoWrites[SubscriptionHost](Json.writes[SubscriptionHost])
  
  implicit val subRegRead:Reads[SubscriptionReg] = mongoReads[SubscriptionReg](Json.reads[SubscriptionReg])
  implicit val subRegWrites:Writes[SubscriptionReg] = mongoWrites[SubscriptionReg](Json.writes[SubscriptionReg])
  
  implicit val subIdsFormat = Json.format[SubscribedIds]
  
  implicit val subUserIdMapCpIdRead:Reads[SubUserIdMapCpId] = mongoReads[SubUserIdMapCpId](Json.reads[SubUserIdMapCpId])
  implicit val subUserIdMapCpIdWrite:Writes[SubUserIdMapCpId] = mongoWrites[SubUserIdMapCpId](Json.writes[SubUserIdMapCpId])
  
  private val cmp_cName = ("cName" -> nonEmptyText(3,100).verifying("must only contain words", _.matches(wordExpr)))
  private val cmp_cDesc = ("cDesc" -> nonEmptyText(3,300).verifying("must only contain words", _.matches(wordExpr)))
  private val cmp_cWebsite = ("cWebsite" -> optional(text(3)).verifying("is not a valid web URL", _.getOrElse("www.sample.com").trim().matches(urlExpr)))
  private val cmp_cCtcNo = ("cCtcNo" -> optional(text(3,13)).verifying("is not a valid contact no", _.getOrElse("0123456").trim().matches(contactNoExpr)));
  private val cmp_cEmail = ("cEmail" -> optional(email))
  private val cmp_ver = ("ver" -> number)
  
  private val validateSubscription_Edit = mapping(
      cmp_cDesc,
      cmp_cWebsite,
      cmp_cCtcNo,
      cmp_cEmail,
      cmp_ver
  )(Subscription_Edit.apply)(Subscription_Edit.unapply)
  
  val validateSubscription = mapping(
	     cmp_cName,
	     cmp_cDesc,
	     cmp_cWebsite,
	     cmp_cCtcNo,
	     cmp_cEmail,
	     cmp_ver
	  )(Subscription.apply)(Subscription.unapply)
  
  def matchField(field:String):String = {
     field match {
       case "cDesc" => "Description"
       case "cWebsite" => "Website"
       case "cCtcNo" => "Contact No"
       case "cEmail" => "Email"
       case _ => field
     }
  }
  
  def validateSubscription_Edit(subscription: Subscription_Edit):List[String] = {      
    val validForm = validateSubscription_Edit.bind(
        Map(
    	"cDesc" -> subscription.cDesc,
    	"cWebsite" -> subscription.cWebsite.getOrElse(""),
    	"cCtcNo" -> subscription.cCtcNo.getOrElse(""),
    	"cEmail" -> subscription.cEmail.getOrElse(""),
    	"ver" -> subscription.ver.toString
	 ))
      
     FormValidator.validForm(validForm, matchField);
  }
  
  def toSubscriptionError(errorList:List[String]) = {
    val errorArray = errorList.foldLeft(JsArray())((acc, x) => acc ++ Json.arr(x))
    Json.obj(
      "errors" -> errorArray
    )
  }
}
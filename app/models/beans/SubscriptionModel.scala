package models.beans

import play.api.libs.json._

object SubscriptionModel {
  
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
 
  implicit val subListRead:Reads[SubscriptionHost] = mongoReads[SubscriptionHost](Json.reads[SubscriptionHost])
  implicit val subListWrites:Writes[SubscriptionHost] = mongoWrites[SubscriptionHost](Json.writes[SubscriptionHost])
  
  implicit val subRegRead:Reads[SubscriptionReg] = mongoReads[SubscriptionReg](Json.reads[SubscriptionReg])
  implicit val subRegWrites:Writes[SubscriptionReg] = mongoWrites[SubscriptionReg](Json.writes[SubscriptionReg])
  
  implicit val subIdsFormat = Json.format[SubscribedIds]
  
  implicit val subUserIdMapCpIdRead:Reads[SubUserIdMapCpId] = mongoReads[SubUserIdMapCpId](Json.reads[SubUserIdMapCpId])
  implicit val subUserIdMapCpIdWrite:Writes[SubUserIdMapCpId] = mongoWrites[SubUserIdMapCpId](Json.writes[SubUserIdMapCpId])
  
}
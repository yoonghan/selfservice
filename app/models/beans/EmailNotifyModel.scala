package models.beans

import play.api.libs.json._

object EmailNotifyModel{
  
  case class EmailValidate(
      _id:String,
      email:String,
      key:String,
      date:Long,
      valid:Boolean
      )
  
  case class EmailNotify(
     _id:String,
     userId:String,
     date:String,
     emailDist:String,
     message:String,
     completed:Boolean
      )  
      
  case class Notify(
     date:String,
     period:Int
      )
  
  implicit val validFormat = Json.format[EmailValidate]
  implicit val notifyFormat = Json.format[Notify]
  implicit val emailNotifyFormat = Json.format[EmailNotify]
}
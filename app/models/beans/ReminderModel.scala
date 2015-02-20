package models.beans

import play.api.libs.json._
import play.api.data.Form
import play.api.data.Forms.{mapping, number, nonEmptyText, text, boolean, list, optional, email}
import play.api.data.FormError
import controllers.service.FormValidator

object ReminderModel {
  
  private val contactNoExpr = """^\+?[0-9]*$"""
  
  case class ReminderSetting(
      reminderDays:List[Int],
      alertEmail:Option[String],
      validEmail:Option[Boolean],
      alertSMS:Option[String],
      validSMS:Option[Boolean],
      allowCreation:Boolean
      )
    
  case class ReminderSetting_Edit(
      reminderDays:List[Int],
      alertEmail:Option[String],
      alertSMS:Option[String],
      allowCreation:Boolean
      )
      
  implicit val reminderSettingFormat = Json.format[ReminderSetting]
  implicit val reminderSettingEditFormat = Json.format[ReminderSetting_Edit]
  
  def toReminderSettingError(errorList:List[String]) = {
    val errorArray = errorList.foldLeft(JsArray())((acc, x) => acc ++ Json.arr(x))
    Json.obj(
      "errors" -> errorArray
    )
  }
  
  private val validateForm = mapping(
      "reminderDays" -> list(number(1,20)),
      "alertEmail" -> optional(email),
      "alertSMS" -> optional(text(7, 13)).verifying("is invalid only numbers allowed", _.getOrElse("").trim().matches(contactNoExpr)),
      "allowCreation" -> boolean
  )(ReminderSetting_Edit.apply)(ReminderSetting_Edit.unapply)
  
  def matchField(field:String):String = {
     field match {
       case "reminderDays" => "Reminder Day"
       case "alertEmail" => "Alert via Email"
       case "alertSMS" => "Alert via SMS"
       case "allowCreation" => "Allow Creation"
       case _ => field
     }
  }
    
  def validateInput(reminderSetting: ReminderSetting_Edit):List[String] = {
    
    val reminderDays = for( l <- 0 until reminderSetting.reminderDays.length) yield {
      "reminderDays["+l+"]" -> String.valueOf(reminderSetting.reminderDays(l))
    }
    
    val  map = Map(
    	"alertEmail" -> reminderSetting.alertEmail.getOrElse(""),
    	"alertSMS" -> reminderSetting.alertSMS.getOrElse(""),
    	"allowCreation" -> String.valueOf(reminderSetting.allowCreation )
	 )
      
	 val fullmap = map ++ reminderDays.toMap
	 val validForm = validateForm.bind(
        fullmap
     )
     FormValidator.validForm(validForm, matchField);
  }
  
  

}
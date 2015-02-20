package models.beans

import play.api.libs.json._
import play.api.data.Form
import play.api.data.Forms.{mapping, number, nonEmptyText, text, boolean, list, optional, email}
import play.api.data.FormError
import controllers.service.FormValidator

object ReportModel {
	case class ReportNotify(
	    _id:String, 
	    status:String,
	    time:Long
	    )
	    
	 case class ReportNotifyWithoutId(
	    status:String,
	    time:Long
	    )
	    
	 case class ReportSetting(
	     email:String
	     )

	private val validateForm = mapping(
      "email" -> email
    )(ReportSetting.apply)(ReportSetting.unapply)
  
    def matchField(field:String):String = {
		field match {
			case "email" => "Email"
			case _ => field
		}
	}
    
	def validateInput(reportSetting: ReportSetting):List[String] = {
		val validForm = validateForm.bind(
		Map(
			"email" -> reportSetting.email 
		))
		FormValidator.validForm(validForm, matchField);
	}
	
	def toReportSettingError(errorList:List[String]) = {
		val errorArray = errorList.foldLeft(JsArray())((acc, x) => acc ++ Json.arr(x))
		Json.obj(
				"errors" -> errorArray
		)
	}
	     
	implicit val reportSettingFormat = Json.format[ReportSetting]
	implicit val notifyFormat = Json.format[ReportNotify]
	implicit val notifyWithoutIdFormat = Json.format[ReportNotifyWithoutId]
}
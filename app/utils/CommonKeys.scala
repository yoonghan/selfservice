package utils

import play.api.Logger
import play.api.Play.current
import play.api.libs.json._

object CommonKeys{
  
	val OTYPE      = "otype"
	val USER_ID    = "userId"
	val PUBLIC_KEY = "pubKey"
	val ACCESS_LVL = "accLvl"
	val CP_ID 	   = "cpId"
	val LOCATION   = "loc"
	val DEVICE_APP = "D_"
	val DEVICE_APP_LENGTH = DEVICE_APP.length()
	
	val JSON_KEYWORD_ERRORS = (errorMsg:Any) => {
    errorMsg match {
      case errorMsg:String =>Json.obj("errors" -> Json.arr(errorMsg))
      case errorArr:JsArray =>Json.obj("errors" -> errorArr)
      case _ => Logger.error("Error message not supported"); Json.obj("errors" -> "System encountered unexpected error")
    }
  }
	val JSON_KEYWORD_OK = (successMsg:String) => { Json.obj("success" -> successMsg) }

  val ERR_COMMON_NO_RECORD_FOUND = JSON_KEYWORD_ERRORS("no record found")
  val SUC_COMMON_OK = JSON_KEYWORD_OK("ok")
	  
	val EMPTY_CPID = ""
    
	val AUTH_DEFAULT_LVL = 1;
	val AUTH_CAL_CREATE_LVL  = 2;
	
	val TERNARY_POS_EMAIL = 2;
	val TERNARY_POS_CONTACTNO = 3;
	val TERNARY_POS_ADDRESS = 4;
  
	val EMAIL_REMINDER_TYPE='R';
	val EMAIL_VALIDATOR_TYPE='V';

  val MONGODB_HEX_ID_PATTERN = """[0-9|a-f]{24}""";
}
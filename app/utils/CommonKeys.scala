package utils

import play.api.Play.current

object CommonKeys{
  
	val OTYPE      = "otype"
	val USER_ID    = "userId"
	val PUBLIC_KEY = "pubKey"
	val ACCESS_LVL = "accLvl"
	val CP_ID 	   = "cpId"
	val LOCATION   = "loc"
	val DEVICE_APP = "D_"
	val DEVICE_APP_LENGTH = DEVICE_APP.length()
	
	val JSON_KEYWORD_ERRORS = "errors"
	val JSON_KEYWORD_OK	= "success"
	  
	val EMPTY_CPID = ""
    
	val AUTH_DEFAULT_LVL = 1;
	val AUTH_CAL_CREATE_LVL  = 2;
	
	val TERNARY_POS_EMAIL = 2;
	val TERNARY_POS_CONTACTNO = 3;
	val TERNARY_POS_ADDRESS = 4;
  
	val EMAIL_REMINDER_TYPE='R';
	val EMAIL_VALIDATOR_TYPE='V';
}
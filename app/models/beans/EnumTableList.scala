package models.beans

object EnumTableList extends Enumeration{
  
	val CALENDAR = Value("calendar")						//Events available
	val CALENDAR_SETUP = Value("calendarSetup")				//Coorporate setup events property
	val CALENDAR_TEMP = Value("calendar_temp")				//Stage 2 after event has been setup. User then checks on the list
	val CALENDAR_NOTIFY = Value("calendarNotify")			//List of emails reminders to be sent out
	
	val EMAIL_NOTIFY_LIST = Value("emailNotifyList")		//Events that needs to be notify in 7 days
	val EMAIL_VALID_LIST = Value("emailValidList")			//Valid emails per user
	
	val SUBSCRIPTION = Value("subscription")				//List of all available subscriptions and who subscribes to it.
	
	val REPORT_NOTIFY_LIST = Value("reportNotifyList")		//Store reporting process status
	val REPORT_SETTING = Value("report_setting")			//Store report settings
	
	val REMINDER = Value("reminder")						//Store user reminder setting
	val PROFILE = Value("profile")							//Stores user basic profile
	val USER = Value("user")								//User login details, this is not their profile
	
	
	val LOCATION = Value("location")						//Static information about weather locations in lad/lat
	
	val IMAGE = Value("introImage")							//Intro Image
	
	val PASS_COLLECTOR = Value("passwdCollector")			//Store all matching passwords that user uses when logged in. Changes and not fixed.
}
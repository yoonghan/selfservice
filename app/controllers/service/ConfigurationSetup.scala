package controllers.service

import play.api.Play.current

object ConfigurationSetup {

	private val bookinghr = current.configuration.getString("min-server-booking-hr").get
	
	
	val DATE_FORMAT = new java.text.SimpleDateFormat("dd-MM-yyyy")
	val DATETIME_FORMAT = new java.text.SimpleDateFormat("dd-MM-yyyy hh:mm:ss")
	val TEMP_FOLDER = current.configuration.getString("tempfile_folder.uri").get 
	
	val MIN_BOOKING_HR = try{Integer.parseInt(bookinghr,10)} catch {case e:Exception => 0}
	
	//Header controls
	val ALLOWED_ORIGINS = current.configuration.getString("header-origin").get
	val ALLOWED_METHODS = "GET, POST, DELETE, PUT, OPTIONS"
	val ALLOWED_HEADERS = "Origin, Content-Type, Accept, X-Requested-With, api_key, Authorization"
	  
	//Excel type
	val EXCEL_TYPE = ".xlsx"
    
	val LOGOUT_LOCATION = current.configuration.getString("logout").get;
}
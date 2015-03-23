package utils

import play.api.Play.current
import play.Logger

object ConfigurationSetup {

	private val bookinghr = current.configuration.getString("min-server-booking-hr").get
	
	val DATE_FORMAT = new java.text.SimpleDateFormat("dd-MM-yyyy")
	val DATETIME_FORMAT = new java.text.SimpleDateFormat("dd-MM-yyyy hh:mm:ss")
	
	val FOLDER_TEMP = current.configuration.getString("tempfile_folder.uri").get
	val FOLDER_STORE = current.configuration.getString("storefile_folder.uri").get
	val FOLDER_PICTURE = "picture/"
  val IMAGE_GENERATOR = current.configuration.getString("image_generator.uri").get
	
	val MIN_BOOKING_HR = try{Integer.parseInt(bookinghr,10)} catch {case e:Exception => 0}
	
	//Header controls
	val ALLOWED_ORIGINS = current.configuration.getString("header-origin").get
	val ALLOWED_METHODS = "GET, POST, DELETE, PUT, OPTIONS"
	val ALLOWED_HEADERS = "Origin, Content-Type, Accept, X-Requested-With, api_key, Authorization"
	  
	//Excel type
	val EXCEL_TYPE = ".xlsx"
    
	val LOGOUT_LOCATION = current.configuration.getString("logout").get;
	
	//Email Controls
	val EMAIL_VALID_PATH = current.configuration.getString("email.validated").get;
	val EMAIL_INVALID_PATH = current.configuration.getString("email.invalidated").get;
	
	//Control Upload Size
	val MAX_UPLOAD_SIZE:Int = {
	  val uploadSize = current.configuration.getString("uploadSize").get
	  //Get type Mega/Kilo of bytes
	  val factor = uploadSize.charAt(uploadSize.length-1).toUpper
	  val size = uploadSize.dropRight(1).toInt
	  if(factor.toUpper == 'M'){
			size * 1024 * 1024
	  }else if(factor.toUpper == 'K'){
		  size * 1024
	  }else{
		  size * 1024
	  }
	}
}
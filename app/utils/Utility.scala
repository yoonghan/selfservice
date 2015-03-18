package utils

import play.api.Logger
import com.jaring.jom.util.email.EmailUtility
import java.io.File
import java.nio.file.Files
import java.nio.file.StandardCopyOption
/**
 * Created only for utility service.
 */
object Utility {
  
	val testEnv:Option[String] = play.api.Play.current.configuration.getString("environment")
	val testVal = "test"
	  
	def getImage(fileName:String):Array[Byte] ={
		try{
			val source = scala.io.Source.fromFile(ConfigurationSetup.FOLDER_STORE+ConfigurationSetup.FOLDER_PICTURE+fileName)(scala.io.Codec.ISO8859)
			val byteArray = source.map(_.toByte).toArray
			source.close()
		    
			byteArray
		}catch{
			case e:Exception=>Array[Byte]()
		}
	}
  
	def sendEmail(message:String){
	  
      if(testEnv.isDefined && testEnv.get == testVal){
        Logger.error("Replicate of email sending to:["+message+"]");
      }else {
		new EmailUtility().sendEmail(message )
      }
	}
	
	def sendEmail(bccList:Option[String], subject:String, message:String){
      if(testEnv.isDefined && testEnv.get == "test"){
        Logger.error("Replicate of email sending to:["+bccList+"], message["+message+"]");
      }else {
		new EmailUtility().sendEmail(bccList, subject, message )
      }
	}
	
	def sendEmail(bccList:Option[String], subject:String, message:String, fileName:String){
	  if(testEnv.isDefined && testEnv.get == "test"){
		  Logger.error("Replicate email sending with attachment:["+message+"]");
		  val copyFile = new File(fileName)
		  Files.copy(new File(fileName).toPath(), new File(fileName+"_bak").toPath(), StandardCopyOption.REPLACE_EXISTING);
	  }else{
		  val emailUtil = new EmailUtility()
		  val multipart = emailUtil.createMultipart(fileName)
		  emailUtil.sendEmail(Option.empty, bccList, "Events Report", message, Some(multipart))
	  }
	}
}
package controllers.service.mail

import play.api.cache.Cache
import play.api.Play.current
import utils.ConfigurationSetup
import com.jaring.jom.util.common.PropertyLoaderUtil
import controllers.jobs.LogActor


object MailReader {

  def readCachedMessage(templateName:String):String = {
    val cacheVal = Cache.getOrElse[String](templateName) {	//forever
      try{
		  val source = scala.io.Source.fromInputStream(PropertyLoaderUtil.fileLoader(templateName))
		  val lines = try source.mkString finally source.close()
		  lines
      }catch{
        case exp:Exception => LogActor.logActor ! (exp.getMessage()); exp.printStackTrace(); "" 
      }
	}
    cacheVal
  }
  
  def getTemplate(message:ObjReplace) = {
    val emailTemplate = readCachedMessage(ConfigurationSetup.MAILTEMPLATE)
    emailTemplate
    			.replaceAll("""\*\|MC:SUBJECT\|\*""", ConfigurationSetup.SUBJECT  )
    			.replaceAll("""\*\|CP:COMPANY\|\*""", ConfigurationSetup.COMPANY_NAME  )
    			.replaceAll("""\*\|CP:CURRENT_YEAR\|\*""", ConfigurationSetup.CURR_YEAR )
    			.replaceAll("""\*\|MC:MONTH\|\*""", message.month )
    			.replaceAll("""\*\|MC:DATE\|\*""", message.date )
    			.replaceAll("""\*\|MC:START_TIME\|\*""", message.startTime )
    			.replaceAll("""\*\|MC:END_TIME\|\*""", message.endTime )
    			.replaceAll("""\*\|MC:TITLE\|\*""", message.title )
    			.replaceAll("""\*\|MC:DESCRIPTION\|\*""", message.desc )
    			.replaceAll("""\*\|CP:LOGIN_SITE\|\*""", ConfigurationSetup.LOGIN_PAGE )
  }
  
  case class ObjReplace(month:String, date:String, title: String, startTime:String, endTime:String, desc:String) 
}
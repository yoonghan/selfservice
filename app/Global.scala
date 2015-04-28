import play.Application
import play.GlobalSettings
import play.libs.Akka
import scala.concurrent.duration.Duration
import akka.actor.ActorRef
import akka.actor.Props
import java.util.concurrent.TimeUnit
import play.api.Logger
import controllers.jobs.{LogActor,CalendarNotifyJob,EmailSendJob}
import java.io.File
import play.Configuration
import play.Mode

/**Global play settings, must always be in root folder**/
 
class Global extends GlobalSettings {
      override def onStart(app:Application) {
        LogActor.init
        CalendarNotifyJob.init
        EmailSendJob.init
      }
      
      override def onLoadConfig(config: Configuration, path: File, classloader: ClassLoader): Configuration = {
        
        	initialize(
        	    storeFolder=config.getString("storefile_folder.uri"),
        	    tempFolder = config.getString("tempfile_folder.uri")
        	    );
        
        super.onLoadConfig(config, path, classloader)
      }
      
      /**
       * Please use print instead of logger. Logger have not been loaded in production.
       */
      private def initialize(storeFolder:String, tempFolder:String){
        println("Start Initialize file")
        checkFolder(_strFolder = storeFolder, _tmpFolder = tempFolder)
        //clearTempFolder(_tmpFolder=tempFolder) // cannot work in production, not sure of why. Scala problem?
      }
      
      private def checkFolder(_strFolder:String, _tmpFolder:String){
      
        try{
          val storeFolder = new File(_strFolder)
          val tempFolder = new File(_tmpFolder)
          val extraFolder = Array("picture/","special/")
          
          if( ! storeFolder.exists()){
            println("Created:"+ storeFolder.getAbsolutePath()  + ",stat:" + storeFolder.mkdir())
          }
          if( ! tempFolder.exists()){
            println("Created:"+ tempFolder.getAbsolutePath() + ",stat:" + tempFolder.mkdir())
          }
          
          for(perFolder <- extraFolder){
            
            val sub_storeFolder = new File(_strFolder+perFolder)
            val sub_tempFolder = new File(_tmpFolder+perFolder)
            
            if( ! sub_storeFolder.exists()){
            	println("Created:"+sub_storeFolder.getAbsolutePath() + ",stat:" + sub_storeFolder.mkdir())
            }
            if( ! sub_tempFolder.exists()){
            	println("Created:"+sub_tempFolder.getAbsolutePath() + ",stat:" + sub_tempFolder.mkdir())
            }
          }
        }catch{
          case e:Exception => {
            Logger.error("Error during folder check" + e.getMessage())
            e.printStackTrace()
          }
        }
      }
      
      private def clearTempFolder(_tmpFolder:String){        
        val tempFolder = new File(_tmpFolder)
        val files = tempFolder.listFiles()
        try{
	        if(files!=null && ! files.isEmpty){
				for(subFile:File <- files){
				  Logger.info("Clear folder:"+tempFolder.getAbsolutePath());
					if(subFile.isFile()){
					  Logger.info("Removing files in:"+subFile.getName())
					  subFile.delete()
					}
				}
			}
        }finally{
          //do nothing
        }
      }
}
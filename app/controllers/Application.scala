package controllers

import play.api._
import play.api.mvc._
import utils.ConfigurationSetup._
import utils.{HttpUtils}
import play.api.data.Form
import play.api.data.Forms.{mapping, longNumber, nonEmptyText, text, optional, boolean, list, number}
import java.io.File
import utils.ResumableInfoStorage
import utils.ResumableInfo
import utils.ResumableChunkNumber
import java.io.RandomAccessFile
import play.api.libs.Files.TemporaryFile
import java.io.FileInputStream
import utils.ConfigurationSetup
import controllers.jobs.TimweReportJob

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Sorry, this page is just a servicing page."))
  }
  
  def help = Action {
     Ok(views.html.help(""))
  }
  
  def ping = Action {
    Ok("Pong").withHeaders(
        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS))
  }
  
  def get = Action{ implicit request =>
    val newProductForm = flowJSForm.bindFromRequest
    newProductForm.fold(
        hasErrors = { form =>
          BadRequest("Responded").withHeaders(
		        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
		        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
		        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
		        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS)) 
        
        },
        success = { form =>
          val base_dir = ConfigurationSetup.FOLDER_TEMP + ConfigurationSetup.FOLDER_SPECIAL
          val fileAbsPath = (new File(base_dir,form.flowRelativePath)).getAbsolutePath() + ".temp"

          val info = ResumableInfoStorage.get(
              form.flowChunkSize  ,
              form.flowTotalSize ,
              form.flowIdentifier ,
              form.flowFilename ,
              form.flowRelativePath ,
              fileAbsPath
              )
          if(!info.valid){
            ResumableInfoStorage.remove(info)
          }
          if(info.uploadedChunks.contains(ResumableChunkNumber(form.flowChunkNumber))){
        	Ok("Responded").withHeaders(
		        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
		        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
		        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
		        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS)) 
          }else{
            NotFound("Responded").withHeaders(
		        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
		        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
		        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
		        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS))
          }
        })
  }

   val multipartMaxLengthParser = parse.maxLength(1024*1024*5, parse.multipartFormData)
  
  def upload = Action(multipartMaxLengthParser) {
    implicit request =>{
      request.body.fold(sizeExceeded, sizeAccepted)
    }
  }
   
  private def sizeAccepted(multipart: MultipartFormData[TemporaryFile])(implicit request: RequestHeader) = {
    multipart.file("file").map { file =>
      //checking type, we want only pictures
      dealWithFile(file.ref.file , multipart)
      
      
    }.getOrElse {
      BadRequest("Missing file")
    }
  }
  
  def dealWithFile(file:File , multipart: MultipartFormData[TemporaryFile])(implicit request: RequestHeader): Result = { 
    val newProductForm = flowJSForm.bindFromRequest(multipart.asFormUrlEncoded)
    newProductForm.fold(
        hasErrors = { form =>
          BadRequest("Responded").withHeaders(
		        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
		        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
		        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
		        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS)) 
        
        },
        success = { form =>
          val base_dir = "c:/temp"
          val fileAbsPath = (new File(base_dir,form.flowRelativePath)).getAbsolutePath() + ".temp"

          val info = ResumableInfoStorage.get(
              form.flowChunkSize ,
              form.flowTotalSize  ,
              form.flowIdentifier ,
              form.flowFilename ,
              form.flowRelativePath ,
              fileAbsPath
              )
          
          val raf = new RandomAccessFile(info.resumableFilePath, "rw")
          raf.seek((form.flowChunkNumber  - 1) * form.flowChunkSize );
              
          val is = new FileInputStream(file)
          var readed:Long = 0L;
          val contentLength: Long = request.headers("Content-Length").toLong
	      val bytes: Array[Byte] = new Array[Byte](1024 * 100)
	        var r: Int = 0
		    do {
		      r = is.read(bytes)
		      if (r > 0) {
		        raf.write(bytes, 0, r)
		        readed += r
		      }
		    }
		    while (readed < contentLength && r > 0)
	        raf.close();
              
          info.uploadedChunks.add(ResumableChunkNumber(form.flowChunkNumber))
          
          if(info.checkIfUploadFinished){
            ResumableInfoStorage.remove(info)
            TimweReportJob.executeJob(info.resumableFilename )
        	Ok("Responded").withHeaders(
		        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
		        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
		        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
		        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS)) 
          }else{
            Ok("Responded").withHeaders(
		        (ACCESS_CONTROL_ALLOW_ORIGIN, ALLOWED_ORIGINS),
		        (ACCESS_CONTROL_ALLOW_CREDENTIALS , "true"),
		        (ACCESS_CONTROL_ALLOW_METHODS , ALLOWED_METHODS),
		        (ACCESS_CONTROL_ALLOW_HEADERS , ALLOWED_HEADERS))
          }
        }
    )
    
  }
   
  private def sizeExceeded(size: MaxSizeExceeded) = {
    BadRequest("File size exceeded")
  }
  
  case class FlowJS(
      flowChunkNumber:Int, 
      flowTotalChunks:Int, 
      flowChunkSize:Int, 
      flowTotalSize:Int, 
      flowIdentifier:String, 
      flowFilename:String, 
      flowRelativePath:String)
  val flowJSForm:Form[FlowJS]=Form(
      mapping(
          "flowChunkNumber" -> number,
          "flowTotalChunks" -> number,
          "flowChunkSize" -> number,
          "flowTotalSize" -> number,
          "flowIdentifier" -> nonEmptyText,
          "flowFilename" -> nonEmptyText,
          "flowRelativePath" -> nonEmptyText
          )(FlowJS.apply)(FlowJS.unapply)
      )

}
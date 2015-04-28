package controllers.service

import java.io._
import play.api.Logger
import play.api.libs.Files.TemporaryFile
import play.api.mvc.{MaxSizeExceeded, _}
import utils.{FlowHelper, FlowInfoStorage, FlowInfo}
import utils.ConfigurationSetup
import utils.CommonKeys

abstract class UploadBase extends BaseApiController with FlowHelper {

  //we want to parse only up to 5MB
  val multipartMaxLengthParser = parse.maxLength(ConfigurationSetup.MAX_UPLOAD_SIZE, parse.multipartFormData)

  def upload = AuthorizeUser(multipartMaxLengthParser) {
    implicit request =>{
      request.body.fold(sizeExceeded, sizeAccepted)
    }
  }
  
  val acceptedType:List[String]
  
  private def checkAcceptedType(contentType:Option[String])(types:Seq[String]):Boolean = {
    if(types == Nil || contentType.isEmpty)
      false
    else{
        val eachType = types.head
        if(contentType.get.equals(eachType)){
        	true
        }else
        	checkAcceptedType(contentType)(types.tail)
    }
  }

  private def sizeExceeded(size: MaxSizeExceeded) = {
    JsonResponse(BadRequest("File size exceeded"))
  }

  private def sizeAccepted(multipart: MultipartFormData[TemporaryFile])(implicit request: RequestHeader) = {
    multipart.file("file").map { picture =>
      if(checkAcceptedType(picture.contentType)(acceptedType) ){
    	val is = new FileInputStream(picture.ref.file)
      	dealWithFile(is, multipart)
      }else{
        JsonResponse(BadRequest("Invalid content transferred. Accepted only: "+ acceptedType) )
      }
    }.getOrElse {
      JsonResponse(BadRequest("Missing file"))
    }
  }

  def dealWithFile(is: InputStream, multipart: MultipartFormData[TemporaryFile])(implicit request: RequestHeader): Result = {
    
    val userId = request.session(CommonKeys.USER_ID)
    val oType = request.session(CommonKeys.OTYPE )
    val userAuth = userIDCombination(oType, userId)
      
    val flowChunkNumber: Int = getFlowChunkNumberMultipart(multipart)
    val info: FlowInfo = getFlowInfoMultipart(multipart, Option.apply(userAuth))
    val contentLength: Long = request.headers("Content-Length").toLong
    writeInTempFile(flowChunkNumber, info, contentLength, is)
    info.uploadedChunks += flowChunkNumber
    if (info.checkIfUploadFinished) {
      FlowInfoStorage.remove(info)
      JsonResponse(Ok("All finished."))
    }
    else {
      JsonResponse(Ok("Upload"))
    }
  }
  
  def uploadGet() = Action {
    request =>
      val userId = request.session(CommonKeys.USER_ID)
      val oType = request.session(CommonKeys.OTYPE )
      val userAuth = userIDCombination(oType, userId)
      
      val flowChunkNumber: Int = getFlowChunkNumber(request)
      val info: FlowInfo = getFlowInfo(request, Option.apply(userAuth))
      if (info.uploadedChunks.contains(flowChunkNumber)) {
        JsonResponse(Ok)
      }
      else {
        JsonResponse(NotFound)
      }
  }
}
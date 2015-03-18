package controllers.service.html

import java.io._
import play.api.Logger
import play.api.libs.Files.TemporaryFile
import play.api.mvc.{MaxSizeExceeded, _}
import utils.{FlowHelper, FlowInfoStorage, FlowInfo}
import controllers.service.BaseApiController
import utils.ConfigurationSetup
import utils.CommonKeys

/**
 * Copied from User: Kayrnt
 */

object Upload extends BaseApiController with FlowHelper {

  //we want to parse only up to 5MB
  val multipartMaxLengthParser = parse.maxLength(ConfigurationSetup.MAX_UPLOAD_SIZE, parse.multipartFormData)

  def upload = AuthorizeUser(multipartMaxLengthParser) {
    implicit request =>{
      request.body.fold(sizeExceeded, sizeAccepted)
    }
  }

  private def sizeExceeded(size: MaxSizeExceeded) = {
    BadRequest("File size exceeded")
  }

  private def sizeAccepted(multipart: MultipartFormData[TemporaryFile])(implicit request: RequestHeader) = {
    multipart.file("file").map { picture =>
      //checking type, we want only pictures
      picture.contentType match {
        case Some("image/jpeg") | Some("image/png") =>
          val is = new FileInputStream(picture.ref.file)
          dealWithFile(is, multipart)
        case _ => BadRequest("invalid content type")
      }
    }.getOrElse {
      BadRequest("Missing file")
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
      Ok("All finished.")
    }
    else {
      Ok("Upload")
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
        Ok
      }
      else {
        NotFound
      }
  }
}
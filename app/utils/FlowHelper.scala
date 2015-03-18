package utils

import java.io.{InputStream, RandomAccessFile, File}
import play.api.Logger
import play.api.libs.Files.TemporaryFile
import play.api.mvc.{MultipartFormData, RequestHeader}

/**
 * Copied from User: Kayrnt
 */
trait FlowHelper {

  val base_dir: String = ConfigurationSetup.FOLDER_TEMP+"picture/"

  protected def getFlowChunkNumber(request: RequestHeader): Int =
    request.getQueryString("flowChunkNumber").fold(-1)(_.toInt)

  protected def getFlowChunkNumberMultipart(multipart: MultipartFormData[TemporaryFile]): Int ={
    val form = multipart.asFormUrlEncoded
    form.getOrElse("flowChunkNumber", Nil).headOption.fold(-1)(_.toInt)
  }

  protected def checkFlowInfo(info: FlowInfo): FlowInfo = {
    if (!info.valid) {
      Logger.error("invalid : " + info)
      FlowInfoStorage.remove(info)
      throw new IllegalArgumentException("Invalid request params.")
    }
    else info
  } 

  protected def getFlowInfo(request: RequestHeader, overrideFileName:Option[String]): FlowInfo = {
    val flowChunkSizeOpt = request.getQueryString("flowCurrentChunkSize")
    val flowTotalSizeOpt = request.getQueryString("flowTotalSize")
    val flowIdentifierOpt = request.getQueryString("flowIdentifier")
    val flowFilenameOpt = if(overrideFileName.isEmpty){ request.getQueryString("flowFilename") }
    						else{ Option.apply(overrideFileName.get + getContentTypeExt(request.getQueryString("flowFilename")))}
    val flowRelativePathOpt = request.getQueryString("flowRelativePath")
    (flowChunkSizeOpt, flowTotalSizeOpt, flowIdentifierOpt, flowFilenameOpt, flowRelativePathOpt) match{
      case (Some(flowChunkSize), Some(flowTotalSize), Some(flowIdentifier),
      Some(flowFilename), Some(flowRelativePath)) => val flowFilePath = new File(base_dir, flowFilename).getAbsolutePath + ".temp"
        val info: FlowInfo =
          FlowInfoStorage.get(flowChunkSize.toInt, flowTotalSize.toInt,
            flowIdentifier, flowFilename, flowRelativePath, flowFilePath)
        checkFlowInfo(info)
      case _ => throw new IllegalArgumentException("Invalid request params.")
    }
  }

  protected def getFlowInfoMultipart(multipart: MultipartFormData[TemporaryFile], overrideFileName:Option[String]): FlowInfo = {
    val form = multipart.asFormUrlEncoded
    def getField(key: String) = form.getOrElse(key, Nil).headOption
    val flowChunkSizeOpt = getField("flowCurrentChunkSize")
    val flowTotalSizeOpt = getField("flowTotalSize")
    val flowIdentifierOpt = getField("flowIdentifier")
    val flowFilenameOpt = if(overrideFileName.isEmpty){ getField("flowFilename") }else{ 
    							Option.apply(overrideFileName.get + getContentTypeExt(getField("flowFilename")))}
    val flowRelativePathOpt = getField("flowRelativePath")
    (flowChunkSizeOpt, flowTotalSizeOpt, flowIdentifierOpt, flowFilenameOpt, flowRelativePathOpt) match{
      case (Some(flowChunkSize), Some(flowTotalSize), Some(flowIdentifier),
      Some(flowFilename), Some(flowRelativePath)) => val flowFilePath = new File(base_dir, flowFilename).getAbsolutePath + ".temp"
        val info: FlowInfo =
          FlowInfoStorage.get(flowChunkSize.toInt, flowTotalSize.toInt,
            flowIdentifier, flowFilename, flowRelativePath, flowFilePath)
        checkFlowInfo(info)
      case _ => throw new IllegalArgumentException("Invalid request params.")
    }
  }
  
  protected def getContentTypeExt(content:Option[String]):String = {
    if(content.isDefined){
      val contentType = content.get
      contentType.substring(contentType.lastIndexOf("."), contentType.length())
    }else{
      Logger.error("Invalid file name provided")
      ""
    }
  }
  
  protected def writeInTempFile(flowChunkNumber: Int, info: FlowInfo, contentLength: Long, input: InputStream) = {
    val raf: RandomAccessFile = new RandomAccessFile(info.resumableFilePath, "rw")
    raf.seek((flowChunkNumber - 1) * info.resumableChunkSize)
    var readData: Long = 0
    val bytes: Array[Byte] = new Array[Byte](1024 * 100)
    var r: Int = 0
    do {
      r = input.read(bytes)
      if (r > 0) {
        raf.write(bytes, 0, r)
        readData += r
      }
    }
    while (readData < contentLength && r > 0)
    raf.close
  }
  
}
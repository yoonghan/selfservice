package controllers.service.html

import controllers.service.UploadBase

object ZipFileUpload extends UploadBase {
	override val acceptedType:List[String] = List("image/jpeg","image/png")
}
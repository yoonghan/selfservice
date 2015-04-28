package controllers.service.html

import controllers.service.UploadBase

object ImageUpload extends UploadBase {
  override val acceptedType:List[String] = List("image/jpeg","image/png")
}
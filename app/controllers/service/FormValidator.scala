package controllers.service

import play.api.data._

object FormValidator {
  
  private type fieldMatcher = String => String
  private type T = Any
  
  private def matchError(types:String)={
     types match {
       case "error.required" => "cannot be blank"
       case "error.minLength" => "requires minimum of "
       case "error.maxLength" => "requires maximum of "
       case _ => types
     }
  }
  
  def validForm(validForm:Either[Seq[play.api.data.FormError], T], matchField:fieldMatcher) = {
	validForm.fold(
          failure => {
            val listErrors = failure.asInstanceOf[List[FormError]]
            for(le <- listErrors;
                types <- {val FormError(field, errors, list) = le; errors}
                ) yield {
              val FormError(field, errors, list) = le
              matchField(field) + " " + matchError(types) + 
              	( if (list.isEmpty) "" else " " + list.head + " characters") 
            }
          },
          success =>List()
      )
  }
}
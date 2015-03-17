package controllers.service.html

import play.api._
import play.api.mvc._
import play.api.data.Form
import play.api.data.Forms.{mapping, number, optional, nonEmptyText}
import controllers.service.BaseApiController
import play.modules.reactivemongo.MongoController
import utils.CommonKeys._
import models.beans.SubscriptionModel._
import play.api.i18n.Messages
import utils.ConfigurationSetup._
import play.Logger
import controllers.service.rest.SubscriptionController

object AdminController extends BaseApiController{
  def admin = AuthorizeUser(BodyParsers.parse.anyContent, AUTH_DEFAULT_LVL ) {
    implicit request =>
	    val form = if(request2flash.get("error").isDefined){
			cpForm.bind(request2flash.data)
	    }else
			cpForm
			
		Ok(views.html.admin(form))
  }
  
  private val cpForm:Form[Subscription] = Form(validateSubscription)
  
  def save = AuthorizeAsyncUser(BodyParsers.parse.urlFormEncoded, AUTH_DEFAULT_LVL ){
    request =>
      
      val requestMod = request.body + ("ver" -> Seq("0"))

      val cpDetails = cpForm.bindFromRequest(requestMod)
      cpDetails.fold(
          hasErrors={ form=>
        	scala.concurrent.Future.successful(Redirect(routes.AdminController.admin()).
        		flashing(Flash(form.data) +
        				("error" -> "Please check for errors")))
          }, 
          success={ form =>
            
            val userId = request.session(USER_ID)
            val oType = request.session(OTYPE)
            val authLvl = request.session(ACCESS_LVL).toInt
            
            val response = SubscriptionController.createSubscription(userId, oType, authLvl, form);
            import play.api.libs.concurrent.Execution.Implicits.defaultContext
            
            val resp = response.map(insert_result => 
              if(insert_result)
            	Redirect(routes.AdminController.admin()).
				flashing(("success"-> "Success"))
			  else
			    Redirect(routes.AdminController.admin()).
				flashing(("error"-> "Creation failed, you had a same provider name as others."))
            )
            
            resp
          }
      )
      
  }
}
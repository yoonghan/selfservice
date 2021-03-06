package models.beans

import play.api.libs.json._
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText, text, optional, email}
import play.api.data.FormError
import controllers.service.FormValidator

object UserModel {
  
  private val contactNoExpr = """^\+?[0-9]+$"""
  private val postalCodeExpr = """^[1-9][0-9]{4}$"""
  private val reduceValForMask = 1000
  
  case class UserLogin(
      email:Option[String],
      otype:String,
      id:String)
  case class UserConfidential(_id:String, id:String, otype:String)
  case class User(id:String, otype:String, name:String, newUser:Boolean, authLevel:Option[Int], cpId:Option[String])
  case class UserIdentity(name:String, email:String)
  case class UserName(firstName:String, lastName:String, newUser:Boolean)
  case class UserMasked(
      conf:Boolean, 
      firstName:String, 
      lastName:String, 
      addr:Option[String],
      pstCd:Option[String], 
      state:Option[String], 
      email:Option[String], 
      ctcNo:Option[String], 
      maskId:String)
  case class UserProfileWithId(
      _id:String,
      firstName:String,
      midName:Option[String],
      lastName:String,
      gender:String,
      country:String,
      pstCd:Option[String],
      addr:Option[String],
      email:Option[String],
      ctcNo:Option[String],
      state:String
      )
  case class ContentProviderViewProfile(
       maskId:String,
       firstName:String,
       midName:Option[String],
       lastName:String,
       gender:String,
       email:Option[String],
       ctcNo:Option[String]
       )
  case class UserProfile(
      firstName:String, 
      midName:Option[String], 
      lastName:String, 
      gender:String,
      country:String,
      pstCd:Option[String],
      addr:Option[String],
      email:Option[String],
      ctcNo:Option[String],
      state:String
      )
  case class UserStorageModel(
	    id:String,
	    addr:Option[String],
	    pstCd:Option[String],
	    state:Option[String],
	    email:Option[String],
	    ctcNo:Option[String]
	    )
      
  def mongoReads[T](r: Reads[T]) = {
    __.json.update((__ \ '_id).json.copyFrom((__ \ '_id \ '$oid).json.pick[JsString] )) andThen r
  }
 
  def mongoWrites[T](w : Writes[T]) = {
    w.transform( js => js.as[JsObject] - "_id"  ++ Json.obj("_id" -> Json.obj("$oid" -> js \ "_id")) )
  }
	    
  implicit val userFormat = Json.format[User]
  implicit val userNameFormat = Json.format[UserName]
  implicit val userLoginFormat = Json.format[UserLogin]
  implicit val userMaskedFormat = Json.format[UserMasked]
  implicit val userProfileFormat = Json.format[UserProfile]
  implicit val userStorageFormat = Json.format[UserStorageModel]
  implicit val userProfileWithIdFormat = Json.format[UserProfileWithId]
  implicit val contentProviderViewProfileFormat = Json.format[ContentProviderViewProfile]
  implicit val userReadFormat:Reads[User] =  mongoReads[User](Json.format[User])
  implicit val userWriteFormat:Writes[User] = mongoWrites[User](Json.format[User])
  implicit val userConfidentialRead:Reads[UserConfidential] = mongoReads[UserConfidential](Json.reads[UserConfidential])
  implicit val userConfidentialWrites:Writes[UserConfidential] = mongoWrites[UserConfidential](Json.writes[UserConfidential])

  def toUserError(errorList:List[String]) = {
    val errorArray = errorList.foldLeft(JsArray())((acc, x) => acc ++ Json.arr(x))
    utils.CommonKeys.JSON_KEYWORD_ERRORS(errorArray)
  }
  
  private val validateForm = mapping(
      "firstName" -> nonEmptyText(2,30),
      "midName" -> optional(text(1,30)), 
      "lastName" -> nonEmptyText(2,30),
      "gender" -> nonEmptyText(1),
      "country" -> nonEmptyText(2,3),
      "pstCd" -> optional(text(5)).verifying("is not a valid Postal Code", _.getOrElse("12345").trim().matches(postalCodeExpr)),
      "addr" -> optional(text(1,100)),
      "email" -> optional(email),
      "ctcNo" -> optional(text(7,13)).verifying("is invalid only numbers allowed", _.getOrElse("0123456").trim().matches(contactNoExpr)),
      "state" -> nonEmptyText(2,20)
  )(UserProfile.apply)(UserProfile.unapply)
  
  def matchField(field:String):String = {
     field match {
       case "firstName" => "First Name"
       case "midName" => "Middle Name"
       case "lastName" => "Last Name"
       case "gender" => "Gender"
       case "country" => "Country"
       case "pstCd" => "Postcode"
       case "addr" => "Address"
       case "email" => "Email"
       case "ctcNo" => "Contact No"
       case "state" => "State"
       case _ => field
     }
  }
   
  def validateInput(userProfile: UserProfile):List[String] = {      
    val validForm = validateForm.bind(
        Map(
    	"firstName" -> userProfile.firstName,
    	"midName" -> userProfile.midName.getOrElse(""),
    	"lastName" -> userProfile.lastName,
    	"gender" -> userProfile.gender,
    	"country" -> userProfile.country,
    	"pstCd" -> userProfile.pstCd.getOrElse(""),
    	"addr" -> userProfile.addr.getOrElse(""),
    	"email" -> userProfile.email.getOrElse(""),
    	"ctcNo" -> userProfile.ctcNo.getOrElse(""),
    	"state" -> userProfile.state
	 ))
      
     FormValidator.validForm(validForm, matchField);
  }
  
  def maskId(userId:String):String = {
    val firstChar = userId.charAt(0)
    val byteVal = firstChar.toByte
    val byteValToString = byteVal.toString
    val newId = byteValToString + userId.substring(1)
    newId
  }
  
  def unmaskId(maskId:String):String = {
    val firstChar = maskId.substring(0,2)
    val newId = try{
    	val parseInt = Integer.parseInt(firstChar)
    	val stringVal = parseInt.toChar
    	stringVal + maskId.substring(2)
    }catch{
    	case e:Exception => "" 
    }
    newId
  }

//  implicit val dbUserProfile = Json.format[DBUserProfile]  
        
//Database part[S]
//  case class DBUserProfile(
//      id:String,
//      firstName:String, 
//      midName:Option[String], 
//      lastName:String, 
//      gender:String,
//      contactNo:Option[String],
//      country:String,
//      state:String)
//Database part[E]
  
//  def createProfile(userProfile:UserProfile, id:String) = {
//    new DBUserProfile(
//        id, 
//        userProfile.firstName,
//        userProfile.midName,
//        userProfile.lastName,
//        userProfile.gender,
//        userProfile.contactNo ,
//        userProfile.country ,
//        userProfile.state)
//  }
}
package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import scala.xml.Text
import scala.annotation.tailrec
import scala.xml.NodeSeq
import Helpers._

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users" // define the DB table name
  override def screenWrap = Full(<lift:surround with="default" at="content">
                                   <lift:bind/>
                                 </lift:surround>)
  // define the order fields will appear in forms and output
  override def fieldOrder = List(id,
    firstName,
    lastName,
    email,
    password,
    sex,
    preference,
    isDrinking,
    isDriving,
    isSmoking,
    isWorking,
    introduction)

  // comment this line out to require email validations
  //override def skipEmailValidation = true

  def findConfirmation(conf: String) = {
    User.find(By(User.confirmation, conf))
  }
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  object sex extends MappedEnum(this, VeganGenders) {
    override def displayName = "Τι φύλο είσαι ?"
  }
  object preference extends MappedEnum(this, VeganPreferences) {
    override def displayName = "Ποιο φύλο σε ενδοιαφέρει ?"
  }
  
  object diet extends MappedEnum(this, VegeterianDiets) {
    override def displayName = "Ποιά χορτοφαγία πρωτιμάς ?"
  }
  
  object isDrinking extends MappedBoolean(this) {
    override def displayName = "Πίνεις αλκοολ ?"
  }
  object isDriving extends MappedBoolean(this) {
    override def displayName = "Έχεις δίπλωμα οδήγησης ?"
  }
  object isSmoking extends MappedBoolean(this) {
    override def displayName = "Καπνίζεις ?"
  }
  object isWorking extends MappedBoolean(this) {
    override def displayName = "Εργάζεσαι ?"
  }
  
  object isWorkingOut extends MappedBoolean(this) {
    override def displayName = "Γυμνάζεσαι ?"
  }
  
  object isDancing extends MappedBoolean(this) {
    override def displayName = "Ξέρεις να χορευείς ?"
  }
  
  object isPlayingMusic extends MappedBoolean(this){
     
    override def displayName = "Παίζεις μουσική ?"

  }
  
  object birthyear extends MappedInt(this) {
    override def displayName = "Σημειώσε την χρονιά γέννησης"
    override def validations = {
      ((value: Int) =>
        if (value > (Helpers.year(new java.util.Date) - 17))
          List(FieldError(this, "Πρεπει να εχεις γεννηθει πριν απο το " + (Helpers.year(new java.util.Date) - 17) + " !"))

        else if (value < 1930)
          List(FieldError(this, "Πρεπει να εχεις γεννηθει μετα απο το 1930 !"))
        else
          List[FieldError]()) :: super.validations
    }
  }
  object confirmation extends MappedString(this, 100)
  object county extends MappedLongForeignKey(this, County)
  object registrationDate extends MappedDateTime(this)
  object loginDate extends MappedDateTime(this)
  // define an additional field for a personal essay
  object introduction extends MappedText(this) {
    override def displayName = "Γράψε για τον εαυτό σου"
  }

  @tailrec
  final def addConfirmation {
    var s = Helpers.randomString(13)
    if (User.findConfirmation(s).isEmpty) {
      this.confirmation(s).save
    } else {
      addConfirmation
    }
  }

  def removeConfirmation {
    this.confirmation("").save
  }

  def findProfilePicture = {
    PhotoUser.find(By(PhotoUser.user, this), By(PhotoUser.isProfile, true))
  }
  
  def imgMiniPhoto(height:Int,width:Int) ={
    findProfilePicture.map(pu => pu.imgMiniPhoto(height,width) )
      .getOrElse(<img src={ VeganGenders.images(this.sex.is.id) } height={height.toString} width={width.toString}/>)
  }
  
   def imgPhoto(height:Int,width:Int) ={
    findProfilePicture.map(pu => pu.imgPhoto(height,width) )
      .getOrElse(<img src={ VeganGenders.images(this.sex.is.id) } height={height.toString} width={width.toString}/>)
  }
   
   def getName= firstName.is + " " + lastName.is
   def getAge= (year(now) - this.birthyear.is)

   def delete{
     Event.findByUser(this).map{
       e => e.delete 
     }
     CommentEvent.findByUser(this).map{
       ce => ce.delete_!
     }
     UserMessage.findByUser(this).map{
       um => um.delete_!
     }
     UserMessageBase.findMeOnBoth(this).map{
       umb => umb.delete_!
     }
     UserViewOtherUser.findUserOnBoth(this).map{
       uvo => uvo.delete_!
     }
     PhotoUser.findByUser(this).map{
       pu => pu.delete
     }
     
     
     this.delete_!
   }
}

object DefaultUser {
  def save {
    val email = Props.get("vlove.admin.email").openOr("admin@vlove.org")
    val pass = Props.get("vlove.admin.pass").openOr("abc123")
    if (User.find(By(User.email, email)).isEmpty) {
      val u = User.create
      u.email(email).password(pass).validated(true).superUser(true).save
    }
    //tests
   
    if(User.find(By(User.email,"test3@test.org")).isEmpty){
      val u = User.create
      u.firstName("Test 3")
      .lastName("Testing")
      .email("test3@test.org")
      .password("test3")
      .birthyear(1988)
      .sex(VeganGenders.mapped(1))
      .county(18)
      .preference(VeganPreferences.mapped(2))
      .validated(true).save
    }
    if(User.find(By(User.email,"test4@test.org")).isEmpty){
      val u = User.create
      u.firstName("Test 4")
      .lastName("Testing")
      .email("test4@test.org")
      .password("test4")
      .birthyear(1990)
      .sex(VeganGenders.mapped(2))
      .county(18)
      .preference(VeganPreferences.mapped(1))
      .validated(true).save
    }
    if(User.find(By(User.email,"test5@test.org")).isEmpty){
      val u = User.create
      u.firstName("Test 5")
      .lastName("Testing")
      .email("test5@test.org")
      .password("test5")
      .birthyear(1984)
      .sex(VeganGenders.mapped(3))
      .county(18)
      .preference(VeganPreferences.mapped(1))
      .validated(true).save
    }
  }
}
package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._
import net.liftweb.http.NamedCometListener
import code.lib.VeganNamedActorsMessages

class UserMessageBase extends LongKeyedMapper[UserMessageBase] with IdPK {
  def getSingleton = UserMessageBase
  object me extends MappedLongForeignKey(this, User)
  object other extends MappedLongForeignKey(this, User)
  object dateOfLastMsg extends MappedDateTime(this)
  object newMessages extends MappedInt(this)
  def changeDateOfLastMsg {
    this.dateOfLastMsg(Helpers.now).save
  }
  def getMessages = {
    UserMessage.findByBase(this)
  }
}
object UserMessageBase extends UserMessageBase with LongKeyedMetaMapper[UserMessageBase] with Loggable {
  def add(me: User, other: User) ={
    //Για να ενημερωθουν και οι δυο messenger
    
    val umb1 = UserMessageBase.create
    umb1.me(me).other(other).dateOfLastMsg(Helpers.now)
    val umb1Box = saveBox(umb1)
    
    //αλλα στελνουμαι μονο για αυτον που αφωρα το ετοιμα 
    umb1Box.map {
      mb =>
        VeganNamedActorsMessages.updateMessenger(other)
        VeganNamedActorsMessages.updateMessenger(me)

        mb
    }
  }

  def findOthersByMe(me: User) = {
    UserMessageBase.findAll(By(UserMessageBase.me, me), OrderBy(UserMessageBase.id, Descending))
  }

  def findMeOnBoth(me: User) = {
    findAll(BySql("me=" + me.id.is.toString + " OR other=" + me.id.is.toString, IHaveValidatedThisSQL("rmanolis", "28-12-12")),
      OrderBy(UserMessageBase.dateOfLastMsg, Descending))
  }

  def findMeOnly(me: User) = {
    findAll(By(UserMessageBase.me, me),
      OrderBy(UserMessageBase.dateOfLastMsg, Descending))
  }

  def findMeAndOther(me: User, other: User) = {
    UserMessageBase.find(By(UserMessageBase.me, me), By(UserMessageBase.other, other))
  }

}

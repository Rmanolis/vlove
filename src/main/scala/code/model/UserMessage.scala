package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._
import net.liftweb.http.NamedCometListener
import code.lib.VeganNamedActorsMessages

class UserMessage extends LongKeyedMapper[UserMessage] with IdPK with Logger {
  def getSingleton = UserMessage
  object user extends MappedLongForeignKey(this, User)
  object base extends MappedLongForeignKey(this, UserMessageBase)
  object message extends MappedText(this)
  object datetime extends MappedDateTime(this)

  def getName = user.obj
    .map(u => u.firstName.is + " " + u.lastName.is)
    .getOrElse {
      error("UserMessage id=" + this.id.is + " does not have a proper user_id")
      ""
    }

}
object UserMessage extends UserMessage with LongKeyedMetaMapper[UserMessage] with Loggable {
  def add(me: User, base: UserMessageBase, text: String) = {

    val um = UserMessage.create
    um.user(me).base(base).message(text).datetime(new java.util.Date)
    saveBox(um).map {
      mb =>
        base.changeDateOfLastMsg
        //Για να ενημερώσει τον Messenger και το Chat
        VeganNamedActorsMessages.updateMessenger(me)

        base.other.obj.map {
          other =>
            VeganNamedActorsMessages.updateMessenger(other)

        }

        VeganNamedActorsMessages.updateChat(base, me)

        mb
    }
  }

  def findByBase(base: UserMessageBase) = {
    findAll(By(UserMessage.base, base), OrderBy(UserMessage.id, Descending))
  }
  
  def findByUser(u:User)={
    findAll(By(UserMessage.user, u))
  }
}
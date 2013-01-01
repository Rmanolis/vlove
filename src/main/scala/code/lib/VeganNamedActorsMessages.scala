package code.lib
import net.liftweb.common._
import code.model._
import code.comet._
import net.liftweb.http._

object VeganNamedActorsMessages extends Logger {
  def updateMessenger(user: User) {
    NamedCometListener.getDispatchersFor(Full("messenger_" + user.id.is.toString)).foreach {
      case actorM => {
        info("Update messenger's user_id=" + user.id.toString + " by the user " + user.getName)
        actorM map { _ ! Messenger(user) }
      }

    }
  }
  def updateChat(base: UserMessageBase, user: User) {
    NamedCometListener.getDispatchersFor(Full("chat_" + base.id.is.toString)).foreach {
      case actorM => {
        info("Update chat's base_id=" + base.id.toString + " from the user " + user.getName)
        actorM map { _ ! code.comet.Chat(base) }
      }

    }
  }

  def updateCommentEvents(event: Event,user:User, lim: Long) {
    NamedCometListener.getDispatchersFor(Full("event_" + event.id.is.toString)).foreach {
      case actorM => {
        info("Update event's id=" + event.id.toString+ " from the user "+user.getName)
        actorM map { _ ! EventComment(event.id.is, lim) }
      }

    }
  }
}
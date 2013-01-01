package code.comet
import net.liftweb._
import http._
import SHtml._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE.Str
import code.lib._
import code.model._
import scala.xml.NodeSeq
import net.liftweb.mapper._
import scala.xml.Text

case class Messenger(user: User)
class MessengerComet extends NamedCometActorTrait with Loggable {
  var userBox: Box[User] = Empty
  override def lowPriority: PartialFunction[Any, Unit] = {
    case Messenger(u) => {
      userBox = Full(u)
      partialUpdate(SetHtml("messages", userBox.map {
        base => table(base)
      }.getOrElse(<table id="messages"></table>)))
    }
    case _ => error("The message for ShowEventCommentsComet is not correct")
  }

  def table(user: User) = {
    <table id="messages">
      {
        UserMessageBase.findMeOnBoth(user).map {
          umb =>
            val theOther = if(umb.me.is == user.id.is) umb.other.obj else umb.me.obj
            theOther.map {
              other =>
                val firstMessage = umb.getMessages.headOption
                val sender = firstMessage.map{m => if(m.user.is == user.id.is) "Εσύ : " else "" }.getOrElse("")
                val message = firstMessage.map(_.message.is).getOrElse("").take(10)

                <tr>
                  <td>{ other.imgMiniPhoto(50,50) }</td>
                  <td>{ other.getName }</td>
                  <td>{ sender + message }</td>
                  <td>{ SHtml.ajaxButton(Text("Chat"),()=>{
                    RedirectTo(Site.chatLoc.calcHref(umb))
                  })}</td>
                </tr>
            }.getOrElse(NodeSeq.Empty)

        }
      }
    </table>
  }

  def render = {
    var out = NodeSeq.Empty
    /*for{
      user <- userBox
    }yield{
      out = table(user)
    }*/
    out
  }

}
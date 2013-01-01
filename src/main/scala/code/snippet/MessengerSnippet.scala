package code.snippet
import scala.xml._
import net.liftweb._
import common._
import http._
import SHtml._
import js._
import JE._
import JsCmds._
import js.jquery._
import JqJsCmds._
import common._
import util._
import Helpers._
import code.model._
import code.lib._
import code.comet._
import net.liftweb.mapper._

object MessengerCometSnippet extends NamedCometActorSnippet {
  def name = "messenger_" + User.currentUser.map(_.id.is.toString).getOrElse("messenger")
  def cometClass = "MessengerComet"
}

object ChatCometSnippet extends NamedCometActorSnippet {
  def name = "chat_" + Site.chatLoc.currentValue.map(_.id.is.toString).getOrElse("chat")
  def cometClass = "ChatComet"
}

class ChatSnippet extends Logger {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      base <- Site.chatLoc.currentValue
      u1 <- base.me.obj
      u2 <- base.other.obj
    } yield {
      if (BlacklistUser.isPairBlacklisted(u1, u2)) { // Να μην μπορει να στηλει  μυνημα για να μην ενοχλεί
        out = <h3> Δεν επιτρέπεται να μιλήσεται με αυτό το άτομο. Έχει απογρευθεί από εσάς ή από τον ίδιο </h3>

      } else {
        VeganNamedActorsMessages.updateChat(base, user)
        var message = ""
        out = (
          "#message" #> SHtml.ajaxText(message, s => {
            message = s

            Focus("submit")
          }, "id" -> "message") &
          "#submit" #> SHtml.ajaxButton(Text("Στείλε"), () => {
            UserMessage.add(user, base, message)
            SetValById("message", "")&
            Focus("message")
          }, "id" -> "submit") &
          "#oldMessages" #> SHtml.button(Text("Ιστορικό"), () => {
            S.redirectTo(Site.oldMessagesLoc.calcHref(base))
          }, "id" -> "oldMessages")).apply(in)
      }
    }
    out
  }
}

class MessengerSnippet extends Logger {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
    } yield {
      VeganNamedActorsMessages.updateMessenger(user)
    }
    out
  }

}

class FixChatCometSnippet extends Logger {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      base <- Site.chatLoc.currentValue

    } yield {
      VeganNamedActorsMessages.updateChat(base, user)
    }
    out
  }
}

class ReadAllMessages extends PaginatorSnippet[UserMessage] {
  override def count = UserMessage.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[net.liftweb.mapper.QueryParam[UserMessage]] = List()
    //Είναι αναγκαίο  για να δείχνει όλες τις εκδηλώσεις απο ολη την Ελλαδα
    Site.chatLoc.currentValue.map {
      base =>
        list +:= By(UserMessage.base, base)
    }
    list +:= OrderBy(UserMessage.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    UserMessage.findAll(list: _*)
  }

  def renderPage(in: NodeSeq): NodeSeq = {

    <table title="Messages">
      <thead>
        <tr>
          <th> Όνομα </th>
          <th> Ημερομηνία </th>
          <th> Μύνημα </th>
        </tr>
      </thead>{
        page.map {
          um =>
            um.user.obj.map {
              user =>
                <tr>  {
                  <td> { user.getName } </td> ++
                    <td> { um.datetime.is.toString } </td> ++
                    <td> { um.message.is } </td>
                }</tr>
            }.getOrElse(NodeSeq.Empty)
        }
      }
    </table>
  }
}
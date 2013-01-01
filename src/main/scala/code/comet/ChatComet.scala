package code.comet
import net.liftweb._
import http._
import SHtml._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.actor._
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JsCmds.{ SetHtml }
import net.liftweb.http.js.JE.Str
import code.lib._
import code.model._
import scala.xml.NodeSeq
import net.liftweb.mapper._

case class Chat(base: UserMessageBase)
class ChatComet extends NamedCometActorTrait  with Loggable {
  var baseBox: Box[UserMessageBase] = Empty
  override def lowPriority: PartialFunction[Any, Unit] = {
    case Chat(b) => {
      baseBox = Full(b)
       partialUpdate(SetHtml("chat", baseBox.map {
        base => chat(base)
      }.getOrElse(<table id="chat"></table>)))
    }
    case _ => error("The message for ShowEventCommentsComet is not correct")
  }

  def chat(base: UserMessageBase) = {
    <ul id="chat">
      {
        UserMessage.findAll(By(UserMessage.base,base),
            OrderBy(UserMessage.datetime,Descending),
            MaxRows(10)).reverse.map {
          um =>
            <li>
              <span>{ um.getName }</span>
              <span>{ um.datetime.is.toString } </span>
              <span>{ um.message.is } </span>
            </li>
        }
      }
    </ul>
  }
  
  def render ={
    var out = NodeSeq.Empty
    /*for{
      base <- baseBox
    }yield{
      out = chat(base)
    }*/
    out
  }

}
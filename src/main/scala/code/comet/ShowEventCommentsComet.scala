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

case class EventComment(eventId: Long, lim: Long)
class ShowEventCommentsComet extends NamedCometActorTrait with Loggable {
  private var limit = 10L
  private var eventId: Long = 0
  private var eventBox: Box[Event] = Empty
  private val curPage = 0L
  private var count = 0L
 
  
  override def lowPriority: PartialFunction[Any, Unit] = {
    case EventComment(id, lim) => {
      eventId = id
      limit = lim
      eventBox = Event.find(eventId)
      count = CommentEvent.count(By(CommentEvent.event,eventBox))
      partialUpdate(SetHtml("comments", eventBox.map {
        event => table(event)
      }.getOrElse(<ul id="comments"></ul>)))

    }
    
    case _ => error("The message for ShowEventCommentsComet is not correct")
  }

  def table(event: Event) = {
    <ul id="comments"> {
      CommentEvent.findAll(By(CommentEvent.event,event),
          OrderBy(CommentEvent.datetime, Descending),
          StartAt(curPage * limit),
          MaxRows(limit)).flatMap {
        ce =>
          ce.user.obj.map { user =>
            
            <li>  {
              <span><a href={ Site.viewUserLoc.calcHref(user) }> { user.imgMiniPhoto(50,50) } </a></span> ++
                <span><a href={ Site.viewUserLoc.calcHref(user) }>{ user.getName }</a></span> ++
                <span> { ce.message.is } </span>
            }</li>
          }
      }
    }</ul>
  }

  def render = {
    var out = NodeSeq.Empty
    
    out
  }
}

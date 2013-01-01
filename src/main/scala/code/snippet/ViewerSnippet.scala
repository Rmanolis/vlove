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

class ViewUser {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    for {
      me <- User.currentUser
      other <- Site.viewUserLoc.currentValue

    } yield {
      if (me.id.is != other.id.is) {
        UserViewOtherUser.add(me, other)
      }
      out = (
        "#img" #> other.imgPhoto(300, 300) &
        "#firstName" #> other.firstName.is &
        "#lastName" #> other.lastName.is &
        "#sex" #> VeganGenders.mapped(other.sex.is.id).toString() &
        "#preference" #> VeganPreferences.mapped(other.preference.is.id).toString &
        "#age" #> (Helpers.year(Helpers.now) - other.birthyear.is).toString &
        "#county" #> other.county.obj.map(_.name.is).getOrElse("") &
        "#introduction" #> other.introduction.is &
        "#message" #> VeganPermisions.messageButton(me, other) &
        "#block" #> VeganPermisions.blockButton(me, other)).apply(in)
    }

    out
  }
}

class ViewUserPhoto {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      other <- Site.viewUserLoc.currentValue

    } yield {
      out = <ul id="photos">
              {
    		  	PhotoUser.findByUser(other).map{
    		  	  pu => <li> {
    		  	    <img src={ "/photo/" + pu.photo.is } height={"200"} width={"200"}/>
    		  	  } </li>
    		  	}
              }
            </ul>
    }
    out
  }
}

class ViewEvent {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      event <- Site.viewEventLoc.currentValue

    } yield {
      val img = event.photo.obj.map { p => <img src={ "/photo/" + event.photo.is } height="300" width="300"/> }.getOrElse(NodeSeq.Empty)
      out = (
        "#img" #> img &
        "#name" #> event.name.is &
        "#introduction" #> event.introduction.is &
        "#creator" #> event.user.obj.map(s => s.firstName.is + " " + s.lastName.is).getOrElse("") &
        "#lat *" #> event.geolocation.obj.map(_.latitude.is.toString).getOrElse("") &
        "#lon *" #> event.geolocation.obj.map(_.longitude.is.toString).getOrElse("") &
        "#flag" #> SHtml.button(Text("Flag"), () => {
          S.redirectTo(Site.flagEventLoc.calcHref(event))
        })).apply(in)
    }
    out
  }
}

class ViewFlagEvent {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      event <- Site.flagEventLoc.currentValue

    } yield {
      var reason = ""
      out = (
        "#name" #> event.name.is &
        "#reason" #> SHtml.textarea(reason, reason = _) &
        "#submit" #> SHtml.button(Text("Submit"), () => {
          FlagEvent.add(user, event, reason)
          S.redirectTo(Site.home.fullUrl)
        }) &
        "#cancel" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.home.fullUrl)
        })).apply(in)
    }
    out
  }
}

class ViewBlockUser {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      cUser <- User.currentUser
      blockUser <- Site.blockUserLoc.currentValue

    } yield {
      var reason = ""
      var important = false
      out = (
        "#name" #> (blockUser.firstName.is + " " + blockUser.lastName.is) &
        "#reason" #> SHtml.textarea(reason, reason = _) &
        "#important" #> SHtml.checkbox(important, important = _) &
        "#submit" #> SHtml.button(Text("Submit"), () => {
          BlacklistUser.add(cUser, blockUser, reason, important)
          S.redirectTo(Site.home.fullUrl)
        }) &
        "#cancel" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.home.fullUrl)
        })).apply(in)
    }
    out
  }
}

class ViewUserGoingToEvent {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      me <- User.currentUser
      event <- Site.viewUsersGoingToEventLoc.currentValue

    } yield {
      out = <table title="Events">
              <thead>
                <tr>
                  <th> Image </th>
                  <th> Name </th>
                  <th> Message </th>
                  <th> Block </th>
                </tr>
              </thead>{
                UserGoingToEvent.findByEvent(event).flatMap(_.user.obj).map {
                  other =>

                    <tr>  {
                      <td><a href={ Site.viewUserLoc.calcHref(other) }>
                            { other.imgMiniPhoto(50, 50) }
                          </a></td> ++
                        <td>{ other.getName }</td> ++
                        <td> {
                          VeganPermisions.messageButton(me, other)
                        } </td> ++
                        <td> {
                          VeganPermisions.blockButton(me, other)
                        } </td>
                    }</tr>
                }
              }
            </table>
    }
    out
  }
}
object AddCommentCometSnippet extends NamedCometActorSnippet {
  def name = "event_" + Site.viewEventLoc.currentValue.map(_.id.is.toString).getOrElse("event")
  def cometClass = "ShowEventCommentsComet"
}

class AddCommentSnippet extends Logger {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      event <- Site.viewEventLoc.currentValue
    } yield {

      var message = ""
      var lim = 10L
      val count = CommentEvent.count(By(CommentEvent.event, event))
      var limits: Seq[(String, String)] = Seq()
      if (count < 10) {
        limits = Seq((10.toString, 10.toString))

      } else {
        limits = ((10L to count by 10) ++ List(count)).map(s => (s.toString, s.toString))

      }
      VeganNamedActorsMessages.updateCommentEvents(event, user, lim)

      out = (
        "#limits" #> SHtml.ajaxSelect(limits, Full("10"), s => {
          lim = asLong(s).getOrElse(10L)
          VeganNamedActorsMessages.updateCommentEvents(event, user, lim)
        }) &
        "#message" #> SHtml.ajaxText(message, message = _, "id" -> "message") &
        "#submit" #> SHtml.ajaxButton(Text("Send"), () => {
          CommentEvent.add(user, event, message)
          VeganNamedActorsMessages.updateCommentEvents(event, user, lim)

          SetValById("message", "")

        })).apply(in)
    }
    out
  }
}

class ViewStalkers {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      me <- User.currentUser

    } yield {
      out = <table title="Events">
              <thead>
                <tr>
                  <th> Image </th>
                  <th> Name </th>
                  <th> times </th>
                  <th> Message </th>
                  <th> Block </th>
                </tr>
              </thead>{
                UserViewOtherUser.getStalkers(me).map {
                  uv =>
                    (for {
                      other <- uv.stalker.obj

                    } yield {
                      <tr>  {
                        <td><a href={ Site.viewUserLoc.calcHref(other) }>
                              { other.imgMiniPhoto(50, 50) }
                            </a></td> ++
                          <td>{ other.getName }</td> ++
                          <td>{ uv.times.is } </td> ++
                          <td> {
                            VeganPermisions.messageButton(me, other)
                          } </td> ++
                          <td> {
                            VeganPermisions.blockButton(me, other)
                          } </td>
                      }</tr>
                    }).getOrElse(NodeSeq.Empty)
                }
              }
            </table>
    }
    out
  }
}


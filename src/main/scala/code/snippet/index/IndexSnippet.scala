package code.snippet.index
import scala.xml._
import net.liftweb._
import common._
import http._
import util._
import Helpers._
import code.model._
import code.lib.Site
import net.liftweb.mapper._

class NumberViewedUser {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
    } yield {
      out = ("#number" #> <a href={ Site.stalkers.fullUrl }>{ UserViewOtherUser.getNumberOfViewers(user) }</a>).apply(in)
    }
    out
  }

}

class ShowUsers {
  def renderNewUsers = {
    "#newUsers" #>
      {
        User.findAll(OrderBy(User.id, Descending), MaxRows(5)).filter(u => u.superUser.is == false).map {
          user =>
            "li" #> {
              "#img" #> user.imgMiniPhoto(50, 50) &
                "#name" #> <a href={ Site.viewUserLoc.calcHref(user) }>{ user.getName }</a> &
                "#age" #> user.getAge

            }
        }
      }
  }

  def renderSomeUsersYouMayLike(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      me <- User.currentUser
    } yield {
      out = ("#greatUsers" #> {
        val listAge: Seq[Int] = (me.birthyear.is to (me.birthyear.is - 5)) ++ (me.birthyear.is to (me.birthyear.is + 5))
        val (sex, preference) = VeganPreferences.preferSex(me.sex.is.id, me.preference.is.id)
        User.findAll(
          By(User.sex, VeganGenders.mapped(sex)),
          By(User.preference, VeganPreferences.mapped(preference)),
          By(User.county,me.county.is),
          ByList(User.birthyear, listAge)).filter(u => u.id.is != me.id.is && u.superUser.is == false).map {
            user =>
              "li" #> {
                "#img" #> user.imgMiniPhoto(50, 50) &
                  "#name" #> <a href={ Site.viewUserLoc.calcHref(user) }>{ user.getName }</a> &
                  "#age" #> user.getAge
              }
          }
      }).apply(in)
    }
    out
  }
}
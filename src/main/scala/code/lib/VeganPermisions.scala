package code.lib
import net.liftweb._
import http._
import code.model._
import scala.xml._

object VeganPermisions {
  def messageButton(current: User, other: User) = {
    if (current.id.is == other.id.is) {
      NodeSeq.Empty
    } else {
      if (BlacklistUser.isPairBlacklisted(current, other)) {
        NodeSeq.Empty
      } else {
        SHtml.button(Text("Message"), () => {
          UserMessageBase.findMeAndOther(current, other).map {
            umb =>
              S.redirectTo(Site.chatLoc.calcHref(umb))
          }.getOrElse {
            UserMessageBase.add(current, other).map {
              umb =>
                S.redirectTo(Site.chatLoc.calcHref(umb))
            }
          }

        })
      }
    }
  }

  def blockButton(current: User, other: User) = {
    if (current.id.is == other.id.is) {
      NodeSeq.Empty
    } else {
      BlacklistUser.findMeBlockingTheOther(current, other).map {
        bu =>
          SHtml.button(Text("Unblock"), () => {
            bu.delete_!
            S.redirectTo(S.uri)
          })
      }.getOrElse {
        SHtml.button(Text("Block"), () => {
          S.redirectTo(Site.blockUserLoc.calcHref(other))
        })
      }
    }

  }

  def goEventButton(user: User, event: Event) = {
    UserGoingToEvent.findByEventAndUser(event, user).map {
      uge =>
        SHtml.button(Text("Δεν θα πάω"), () => {
          uge.delete_!
          S.redirectTo(S.uri)
        })

    }.getOrElse {
      SHtml.button(Text("θα πάω"), () => {
        UserGoingToEvent.add(user, event)
        S.redirectTo(S.uri)
      })
    }

  }
}
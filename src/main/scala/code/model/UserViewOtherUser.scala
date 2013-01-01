package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._

class UserViewOtherUser extends LongKeyedMapper[UserViewOtherUser] with IdPK {
  def getSingleton = UserViewOtherUser
  object stalker extends MappedLongForeignKey(this, User)
  object victim extends MappedLongForeignKey(this, User)
  object times extends MappedLong(this)
  object latest extends MappedDateTime(this)

  def addOneMoreTime = {
    this.times(this.times.is + 1).latest(Helpers.now)
    saveBox(this)
  }
}

object UserViewOtherUser extends UserViewOtherUser with LongKeyedMetaMapper[UserViewOtherUser] {
  def add(stalker: User, victim: User) = {
    if (stalker.id.is != victim.id.is) {
      findView(stalker, victim).map {
        v => v.addOneMoreTime
      }.getOrElse {
        val uv = UserViewOtherUser.create
        uv.stalker(stalker).victim(victim).latest(Helpers.now).times(1)
        saveBox(uv)
      }
    } else {
      Empty
    }
  }

  def getNumberOfViewers(user: User) = {
    count(By(UserViewOtherUser.victim, user))
  }
  def getStalkers(user: User) = {
    findAll(By(UserViewOtherUser.victim, user))
  }
  def findView(stalker: User, victim: User) = {
    find(By(UserViewOtherUser.stalker, stalker), By(UserViewOtherUser.victim, victim))
  }

  def findUserOnBoth(me: User) = {
    findAll(BySql("stalker=" + me.id.is.toString + " OR victim=" + me.id.is.toString, IHaveValidatedThisSQL("rmanolis", "28-12-12")))
  }
}

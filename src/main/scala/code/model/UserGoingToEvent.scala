package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._

class UserGoingToEvent extends LongKeyedMapper[UserGoingToEvent] with IdPK {
  def getSingleton = UserGoingToEvent
  object user extends MappedLongForeignKey(this, User)
  object event extends MappedLongForeignKey(this, Event)
  object datetime extends MappedDateTime(this)

}

object UserGoingToEvent extends UserGoingToEvent with LongKeyedMetaMapper[UserGoingToEvent] {
	def add(user:User, event:Event)={
	  val cu = UserGoingToEvent.create
	  cu.user(user).event(event).datetime(Helpers.now)
	  saveBox(cu)
	}
	
	def findByEvent(event:Event)={
	  findAll(By(UserGoingToEvent.event,event))
	}
	
	def findByUser(user:User)={
	  findAll(By(UserGoingToEvent.user,user))
	}
	
	def findByEventAndUser(event:Event,user:User)={
	  find(By(UserGoingToEvent.user,user),By(UserGoingToEvent.event,event))
	}
	
}

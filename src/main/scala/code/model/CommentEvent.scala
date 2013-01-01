package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._


class CommentEvent extends LongKeyedMapper[CommentEvent] with IdPK {
  def getSingleton = CommentEvent
  object user extends MappedLongForeignKey(this, User)
  object event extends MappedLongForeignKey(this, Event)
  object message extends MappedText(this)
  object datetime extends MappedDateTime(this)

}
object CommentEvent extends CommentEvent with LongKeyedMetaMapper[CommentEvent] {
	def add(user:User, event:Event, text:String)={
	  val cu = CommentEvent.create
	  cu.user(user).event(event).message(text).datetime(Helpers.now)
	  saveBox(cu)
	}
	def findByEvent(event:Event)={
	  findAll(By(CommentEvent.event,event))
	}
	def findByUser(user:User)={
	  findAll(By(CommentEvent.user,user))
	}
}
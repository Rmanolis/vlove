package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._

class FlagEvent extends LongKeyedMapper[FlagEvent] with IdPK {
  def getSingleton = FlagEvent
  object user extends MappedLongForeignKey(this, User)
  object event extends MappedLongForeignKey(this, Event)
  object time extends MappedDateTime(this)
  object reason extends MappedText(this)
}


object FlagEvent  extends FlagEvent with LongKeyedMetaMapper[FlagEvent]  {
   def add(user:User , event : Event,reason:String)={
     val fe = FlagEvent.create
     fe.user(user).event(event).reason(reason).time(Helpers.now)
     saveBox(fe)
   }
   def findByEvent(event:Event)={
	  findAll(By(FlagEvent.event,event))
	}
}

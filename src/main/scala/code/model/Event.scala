package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._
import java.util.Date
import org.joda.time.DateTime
class Event extends LongKeyedMapper[Event] with IdPK {
  def getSingleton = Event
  object name extends MappedString(this, 400) {
    override def validations = valMinLen(1, "Το όνομα δεν μπορεί να είναι κενό") _ ::
      valMaxLen(400, "Το όνομα είναι πολύ μεγάλο") _ ::
      super.validations
  }
  object introduction extends MappedText(this)
  object user extends MappedLongForeignKey(this, User)
  object photo extends MappedLongForeignKey(this, Photo)
  object geolocation extends MappedLongForeignKey(this, Geolocation)
  object starts extends MappedDateTime(this)
  object ends extends MappedDateTime(this)
  object county extends MappedLongForeignKey(this, County)

  def edit(user: User, county: County, photo: Box[Photo], geo: Box[Geolocation], name: String, intro: String, starts: Date, ends: Date) = {
    this.user(user).photo(photo).geolocation(geo).name(name).introduction(intro).starts(starts).ends(ends).county(county)
    saveBox(this)
  }

  def getUserGoingToEvent = {
    UserGoingToEvent.findAll(By(UserGoingToEvent.event, this))
  }

  def delete {
    this.photo.obj.map(_.delete)
    this.geolocation.obj.map(_.delete_!)
    CommentEvent.findByEvent(this).map(_.delete_!)
    FlagEvent.findByEvent(this).map(_.delete_!)
    UserGoingToEvent.findByEvent(this).map(_.delete_!)
    this.delete_!
  }

  def imgMiniPhoto(height: Int, width: Int) = {
    <img src={ "/minphoto/" + photo.is } height={ height.toString } width={ width.toString }/>
  }

  def imgPhoto(height: Int, width: Int) = {
    <img src={ "/photo/" + photo.is } height={ height.toString } width={ width.toString }/>

  }
}

object Event extends Event with LongKeyedMetaMapper[Event] {
  def add(user: User, county: County, photo: Box[Photo], geo: Box[Geolocation], name: String, intro: String, starts: Date, ends: Date) = {
    val e = Event.create
    e.user(user).photo(photo).geolocation(geo).name(name).introduction(intro).starts(starts).ends(ends).county(county)
    saveBox(e)
  }

  def findByUser(user: User) = {
    Event.findAll(By(Event.user, user))
  }
}
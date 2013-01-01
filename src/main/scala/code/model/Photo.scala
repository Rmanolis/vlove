package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._
import code.lib.ImageHelpers
class Photo extends LongKeyedMapper[Photo] with IdPK {
  def getSingleton = Photo
  object name extends MappedString(this, 400)
  object minName extends MappedString(this, 400)
  object source extends MappedString(this, 400)
  object minSource extends MappedString(this, 400)
  object mimeType extends MappedString(this, 400) {
    override def validations = {
      ((value: String) =>
        if (value != "image/jpeg") {
          List(FieldError(this, "Πρέπει η φωτογραφία να ειναι JPEG"))
        } else
          List[FieldError]()) :: super.validations

    }
  }

  def edit(name: String, source: String) = {
    this.name(name).source(source)
    saveBox(this)
  }

  def delete {
    ImageHelpers.delete(this.source.is)
    ImageHelpers.delete(this.minSource.is )
    this.delete_!
  }
}

object Photo extends Photo with LongKeyedMetaMapper[Photo] {
  def add(mimeType: String):Box[Photo] = {
    val name = Helpers.randomString(20)
    if (findByName(name).isEmpty) {
      val a = Photo.create
      a.name(name + ".jpg")
        .minName(name + "_200x200.jpg")
        .source(Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + name + ".jpg")
        .minSource(Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + name + "_200x200.jpg")
        .mimeType(mimeType)
      saveBox(a)
    } else {
    	add(mimeType)
    }
  }

  def findByName(name: String) = {
    Photo.findAll(By(Photo.name, name))
  }
}

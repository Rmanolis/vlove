package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._

class Country extends LongKeyedMapper[Country] with IdPK{
    def getSingleton = Country
    object name extends MappedString(this, 400){
    override def validations = valMinLen(1, "Το όνομα δεν μπορεί να είναι κενό") _ ::
      valMaxLen(400, "Το όνομα είναι πολύ μεγάλο") _ ::
      super.validations
  }
    def edit(name:String)={
      this.name(name)
      saveBox(this)
    }
}
object Country extends Country  with LongKeyedMetaMapper[Country] {
  def add(name:String )={
    val c = Country.create
    c.name(name)
    saveBox(c)
  }
  
  def findByName(name:String)={
    Country.find(By(Country.name, name))
  }
}

object DefaultCountry{
  def save{
    if(Country.findByName("Ελλάδα").isEmpty){
      Country.add("Ελλάδα")
    }
  }
  
}
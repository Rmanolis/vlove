package code.model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._


class Geolocation extends LongKeyedMapper[Geolocation] with IdPK{
    def getSingleton = Geolocation
    object latitude extends MappedDouble(this)
    object longitude extends MappedDouble(this)
 

}

object Geolocation extends Geolocation  with LongKeyedMetaMapper[Geolocation]{
  def add(lat:Double  ,lon:Double )={
    val g = Geolocation.create
    g.longitude(lon).latitude(lat)
    saveBox(g)
  }
}
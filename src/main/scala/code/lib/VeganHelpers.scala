package code.lib
import net.liftweb.mapper._
import net.liftweb.http.S
import net.liftweb.common._

object ModelHelpers {
  def saveBox[T <: LongKeyedMapper[_]](v: T): Box[T] = {
    v.validate match {
      case Nil => {
        if (v.save) {
          Full(v)
        } else {
          Empty
        }
      }
      case xs => S.error(xs); Empty
    }
  }
}
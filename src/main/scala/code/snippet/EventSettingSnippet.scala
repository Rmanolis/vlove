package code.snippet
import scala.xml._
import net.liftweb._
import common._
import http._
import SHtml._
import js._
import JE._
import JsCmds._
import js.jquery._
import JqJsCmds._
import common._
import util._
import Helpers._
import code.model._
import code.lib._
import net.liftweb.mapper._
import ImageHelpers._
import java.util.Date
import org.joda.time.DateTime

class AddEvent extends StatefulSnippet {
  var introduction = ""
  var name = ""

  var lat: Box[Double] = Empty
  var lon: Box[Double] = Empty

  val dt = DateTime.now()
  val date = dt.toDate

  var day = Helpers.day(date)
  var month = Helpers.month(date)
  var year = Helpers.year(date)
  var hour = dt.getHourOfDay()
  var min = dt.getMinuteOfHour()

  var eDay = Helpers.day(date)
  var eMonth = Helpers.month(date)
  var eYear = Helpers.year(date)
  var eHour = dt.getHourOfDay()
  var eMin = dt.getMinuteOfHour()
  var county= User.currentUser.map(_.county.is).getOrElse[Long](1).toString
  def dispatch: DispatchIt = {
    case "render" => render _
  }

  def render(in: NodeSeq): NodeSeq = {

    var days = (1 to 31).map(i => (i.toString, i.toString))
    var months = (1 to 12).map(i => (i.toString, i.toString))
    var years = List((year.toString, year.toString), ((year + 1).toString, (year + 1).toString))
    var hours = (0 to 23).map(i => (i.toString, i.toString))
    var mins = (0 to 59).map(i => (i.toString, i.toString))
    val counties = County.findAll.map(c => (c.id.is.toString,c.name.is))
    
    ("#startSelectDay" #> SHtml.select(days, Full(day.toString), s => {
      day = asInt(s).getOrElse(day)
    }) &
      "#startSelectMonth" #> SHtml.select(months, Full(month.toString), s => {
        month = asInt(s).getOrElse(month)
      }) &
      "#startSelectYear" #> SHtml.select(years, Full(year.toString), s => {
        year = asInt(s).getOrElse(year)
      }) &
      "#startSelectHour" #> SHtml.select(hours, Full(hour.toString), s => {
        hour = asInt(s).getOrElse(hour)
      }) &
      "#startSelectMinute" #> SHtml.select(mins, Full(min.toString), s => {
        min = asInt(s).getOrElse(min)
      }) &
      "#endSelectDay" #> SHtml.select(days, Full(eDay.toString), s => {
        eDay = asInt(s).getOrElse(eDay)
      }) &
      "#endSelectMonth" #> SHtml.select(months, Full(eMonth.toString), s => {
        eMonth = asInt(s).getOrElse(eMonth)
      }) &
      "#endSelectYear" #> SHtml.select(years, Full(eYear.toString), s => {
        eYear = asInt(s).getOrElse(eYear)
      }) &
      "#endSelectHour" #> SHtml.select(hours, Full(eHour.toString), s => {
        eHour = asInt(s).getOrElse(eHour)
      }) &
      "#endSelectMinute" #> SHtml.select(mins, Full(eMin.toString), s => {
        eMin = asInt(s).getOrElse(eMin)
      }) &
      "#name" #> SHtml.text(name, name = _) &
      "#introduction" #> SHtml.textarea(introduction, introduction = _) &
      "#county" #> SHtml.select(counties,User.currentUser.flatMap(_.county.obj.map(_.id.is.toString)),county=_)&
      "#lat" #> SHtml.text("", s => {
        lat = asDouble(s)
      }, "id" -> "lat") &
      "#lon" #> SHtml.text("", s => {
        lon = asDouble(s)
      }, "id" -> "lon") &
      "#upload" #> SHtml.fileUpload { fp =>
        theUpload(Full(fp))
      } &
      "#submit" #> SHtml.button(Text("Submit"), () => {

        var photo: Box[Photo] = Empty
        var geolocation: Box[Geolocation] = Empty
        theUpload.is.map {
          up =>
            photo = Photo.add(up.mimeType)
        }

        for {
          up <- theUpload
          ph <- photo
        } yield {
          save(up.fileStream, Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + ph.name.is)
          saveBI(resize(up.fileStream, 200, 200), Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + ph.minName.is)
        }

        for {
          lt <- lat
          ln <- lon
        } yield {
          geolocation = Geolocation.add(lt, ln)
        }
        User.currentUser.map {
          user =>
            val coun=County.find(county).getOrElse(user.county.obj.get)
            val start = new DateTime(year, month, day, hour, min)
            val end = new DateTime(eYear, eMonth, eDay, eHour, eMin)
            if (start.isBefore(end)) {
              Event.add(user,coun, photo, geolocation, name, introduction, start.toDate(), end.toDate()).map {
                S.redirectTo(Site.events.fullUrl)
              }

            } else {
              S.error("date", "Βάλε την έναρξη και την λήξη σωστά")
            }

        }

      }) &
      "#cancel" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.events.fullUrl)
      })).apply(in)
  }
}

class Events extends PaginatorSnippet[Event]{
  override def count = Event.count
  override def itemsPerPage = 10
  override def page = Event.findAll(By(Event.user,User.currentUser),OrderBy(Event.id, Descending),StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))

  
  def renderPage(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
    } yield {
      out = <table id="tableEvents"> {
        <tr>
          <th> Photo </th>
          <th> name </th>
          <th> edit </th>
          <th> Delete </th>
        </tr> ++
          page.map {
            ev =>
              val img = ev.photo.obj.map(s => <img src={ "/minphoto/" + ev.photo.is } height="100" width="100"/>).getOrElse(NodeSeq.Empty)
              <tr> {
                <td><a href={ Site.viewEventLoc.calcHref(ev) }>{img}</a></td> ++
                  <td> <a href={ Site.viewEventLoc.calcHref(ev) }>{
                    ev.name.is
                  }</a></td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editEventLoc.calcHref(ev))
                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      ev.delete
                      S.redirectTo(Site.events.fullUrl)
                    })
                  }</td>
              }</tr>
          }
      }</table>
    }
    out
  }
}

class EditEvent {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      event <- Site.editEventLoc.currentValue

    } yield {
      if (event.user.is == user.id.is) {
        var name = event.name.is
        var introduction = event.introduction.is

        var lat: Box[Double] = event.geolocation.obj.map(_.latitude.is)
        var lon: Box[Double] = event.geolocation.obj.map(_.longitude.is)

        val dt = new DateTime(event.starts.is)
        val date = dt.toDate

        var day = Helpers.day(date)
        var month = Helpers.month(date)
        var year = Helpers.year(new Date)
        var hour = dt.getHourOfDay()
        var min = dt.getMinuteOfHour()

        val eDt = new DateTime(event.ends.is)
        val eDate = eDt.toDate

        var eDay = Helpers.day(eDate)
        var eMonth = Helpers.month(eDate)
        var eYear = Helpers.year(eDate)
        var eHour = eDt.getHourOfDay()
        var eMin = eDt.getMinuteOfHour()

        var county= event.county.is.toString

        
        var days = (1 to 31).map(i => (i.toString, i.toString))
        var months = (1 to 12).map(i => (i.toString, i.toString))
        var years = List((year.toString, year.toString), ((year + 1).toString, (year + 1).toString))
        var hours = (0 to 23).map(i => (i.toString, i.toString))
        var mins = (0 to 59).map(i => (i.toString, i.toString))
        val counties = County.findAll.map(c => (c.id.is.toString,c.name.is))


        out = ("#startSelectDay" #> SHtml.select(days, Full(day.toString), s => {
          day = asInt(s).getOrElse(day)
        }) &
          "#startSelectMonth" #> SHtml.select(months, Full(month.toString), s => {
            month = asInt(s).getOrElse(month)
          }) &
          "#startSelectYear" #> SHtml.select(years, Full(year.toString), s => {
            year = asInt(s).getOrElse(year)
          }) &
          "#startSelectHour" #> SHtml.select(hours, Full(hour.toString), s => {
            hour = asInt(s).getOrElse(hour)
          }) &
          "#startSelectMinute" #> SHtml.select(mins, Full(min.toString), s => {
            min = asInt(s).getOrElse(min)
          }) &
          "#endSelectDay" #> SHtml.select(days, Full(eDay.toString), s => {
            eDay = asInt(s).getOrElse(eDay)
          }) &
          "#endSelectMonth" #> SHtml.select(months, Full(eMonth.toString), s => {
            eMonth = asInt(s).getOrElse(eMonth)
          }) &
          "#endSelectYear" #> SHtml.select(years, Full(eYear.toString), s => {
            eYear = asInt(s).getOrElse(eYear)
          }) &
          "#endSelectHour" #> SHtml.select(hours, Full(eHour.toString), s => {
            eHour = asInt(s).getOrElse(eHour)
          }) &
          "#endSelectMinute" #> SHtml.select(mins, Full(eMin.toString), s => {
            eMin = asInt(s).getOrElse(eMin)
          }) &
          "#name" #> SHtml.text(name, name = _) &
          "#introduction" #> SHtml.textarea(introduction, introduction = _) &
          "#county" #> SHtml.select(counties,Full(county),county=_)&
          "#lat" #> SHtml.text(lat.map(_.toString).getOrElse(""), s => {
            lat = asDouble(s)
          }, "id" -> "lat") &
          "#lon" #> SHtml.text(lon.map(_.toString).getOrElse(""), s => {
            lon = asDouble(s)
          }, "id" -> "lon") &
          "#upload" #> SHtml.fileUpload { fp =>
            theUpload(Full(fp))
          } &
          "#submit" #> SHtml.button(Text("Submit"), () => {

            var photo: Box[Photo] = event.photo.obj
            var geolocation: Box[Geolocation] = event.geolocation.obj

            theUpload.is.map {
              up =>
                photo = Photo.add(up.mimeType)
            }

            for {
              up <- theUpload
              ph <- photo

            } yield {
              event.photo.obj.map(_.delete)
              save(up.fileStream, Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + ph.name.is)
              saveBI(resize(up.fileStream, 200, 200), Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + ph.minName.is)
            }

            for {
              lt <- lat
              ln <- lon
            } yield {
              event.geolocation.obj.map(_.delete_!)
              geolocation = Geolocation.add(lt, ln)
            }

            User.currentUser.map {
              user =>
                val coun=County.find(county).getOrElse(user.county.obj.get)
                val start = new DateTime(year, month, day, hour, min)
                val end = new DateTime(eYear, eMonth, eDay, eHour, eMin)
                if (start.isBefore(end)) {
                  event.edit(user,coun, photo, geolocation, name, introduction, start.toDate, end.toDate).map {
                    S.redirectTo(Site.events.fullUrl)
                  }
                } else {
                  S.error("date", "Βάλε την έναρξη και την λήξη σωστά")
                }
            }

          }) &
          "#cancel" #> SHtml.button(Text("Cancel"), () => {
            S.redirectTo(Site.events.fullUrl)
          })).apply(in)
      }
    }
    out
  }
}

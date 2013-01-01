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

class SearchEventSnippet extends PaginatorSnippet[Event] {
  private val paramList = List(
    "county" -> "")
  val county = S.param("county").flatMap(s => asLong(s).map(i => By(Event.county, i)))
  override def count = Event.count
  override def itemsPerPage = 10

  override def pageUrl(offset: Long): String = appendParams(super.pageUrl(offset), paramList)
  override def page = {
    var list: List[net.liftweb.mapper.QueryParam[Event]] = List()
    //Είναι αναγκαίο  για να δείχνει όλες τις εκδηλώσεις απο ολη την Ελλαδα
    S.param("county").map {
      c =>
        if (c != "1") {
          county.map(list +:= _)
        }
    }
    list +:= OrderBy(Event.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    Event.findAll(list: _*)
  }

  def renderPage(in: NodeSeq): NodeSeq = {


    <table title="Events">
      <thead>
        <tr>
          <th> Image </th>
          <th> Name </th>
          <th> Starts </th>
          <th> Ends </th>
          <th> Going </th>
          <th> Go </th>
          <th> Flag </th>
        </tr>
      </thead>{
        page.map {
          event =>
            var img = event.photo.obj.map { p => <img src={ "/minphoto/" + event.photo.is } height="100" width="100"/> }.getOrElse(NodeSeq.Empty)
            <tr>  {
              <td><a href={ Site.viewEventLoc.calcHref(event) }>
                    { img }
                  </a></td> ++
                <td><a href={ Site.viewEventLoc.calcHref(event) }>{ event.name.is }</a></td> ++
                <td> { event.starts.is.toString } </td> ++
                <td> { event.ends.is.toString } </td> ++
                <td> <a href={ Site.viewUsersGoingToEventLoc.calcHref(event) }>{ event.getUserGoingToEvent.size.toString } </a> </td> ++
                <td> { User.currentUser.map(VeganPermisions.goEventButton(_,event)).getOrElse(NodeSeq.Empty)}</td> ++
                <td> {
                  SHtml.button(Text("Flag"), () => {
                    S.redirectTo(Site.flagEventLoc.calcHref(event))
                  })
                } </td>
            }</tr>
        }
      }
    </table>
  }

  def searchPage(in: NodeSeq): NodeSeq = {
    import scala.collection.mutable.HashMap
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
    } yield {
      val counties = County.findAll.map(c => (c.id.is.toString, c.name.is))
      var queries: HashMap[String, String] = HashMap()
      out = (

        "#county" #> SHtml.select(counties, user.county.obj.map(_.id.is.toString), s => {
          queries += ("county" -> s)
        }) &
        "#submit" #> SHtml.button(Text("Submit"), () => {
          val parms = queries.map {
            case (p, i) =>
              p + "=" + i
          }.toList.foldLeft("&")((s, i) => s + i + "&")
          S.redirectTo(Site.searchEvents.fullUrl + "?" + parms)
        })).apply(in)
    }

    out
  }

}
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


class CRUDEvents extends PaginatorSnippet[Event] {
  override def count = Event.count
  override def page = Event.findAll(OrderBy(Event.starts, Descending),StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))
  override def itemsPerPage = 10
  def renderPage = {
    <table title="Events">
		<thead>
			<tr>
				<th>Id</th>
				<th>Title</th>
				<th>Start</th>
				<th>End</th>
				<th>Delete</th>
			</tr>
		</thead> {
      page.map {
        event =>
          <tr>  {
            <td>{ event.id.is }</td> ++
            <td>{event.name.is}</td> ++
             <td> {event.starts.is.toString} </td> ++
             <td>{ event.ends.is.toString }</td> ++
              <td> {
                SHtml.button(Text("Delete"), () => {
                  event.delete_!
                  S.redirectTo(Site.crudUsers.fullUrl)
                })
              }</td>
          }</tr>
      }
    }</table> 
  }
}
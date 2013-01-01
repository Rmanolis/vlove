package code.snippet
import scala.xml._
import code.model._
import code.lib._
import net.liftweb.http._
import net.liftweb.mapper._

class AttendingEventsUserSnippet extends PaginatorSnippet[UserGoingToEvent] {
  override def count = UserGoingToEvent.count
  override def itemsPerPage = 10
  override def page = {
    User.currentUser.map {
      user =>
        var list: List[net.liftweb.mapper.QueryParam[UserGoingToEvent]] = List()
        list +:= By(UserGoingToEvent.user, user)
        list +:= OrderBy(UserGoingToEvent.datetime, Descending)
        list +:= StartAt(curPage * itemsPerPage)
        list +:= MaxRows(itemsPerPage)
        UserGoingToEvent.findAll(list: _*)
    }.getOrElse(List())

  }
  def renderPage(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      me <- User.currentUser
    } yield {
      out = <table title="Events">
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
                page.flatMap(_.event.obj).map {
                  event =>
                    <tr>  {
                      <td><a href={ Site.viewEventLoc.calcHref(event) }>
                            { event.imgMiniPhoto(50, 50) }
                          </a></td> ++
                        <td><a href={ Site.viewEventLoc.calcHref(event) }>{ event.name.is }</a></td> ++
                        <td> { event.starts.is.toString } </td> ++
                        <td> { event.ends.is.toString } </td> ++
                        <td> <a href={ Site.viewUsersGoingToEventLoc.calcHref(event) }>{ event.getUserGoingToEvent.size.toString } </a> </td> ++
                        <td> { User.currentUser.map(VeganPermisions.goEventButton(_, event)).getOrElse(NodeSeq.Empty) }</td> ++
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

    out
  }
}
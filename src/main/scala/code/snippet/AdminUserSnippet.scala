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

class CRUDUsers extends PaginatorSnippet[User] {
  override def count = User.count
  override def itemsPerPage = 10
  override def page = User.findAll(OrderBy(User.registrationDate, Descending),StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))

  def renderPage = {
    <table title="Users">
      <thead>
        <tr>
          <th>id</th>
          <th>First name</th>
          <th>Last name</th>
          <th> Age </th>
          <th>Email</th>
          <th>Confirmed </th>
          <th>Admin</th>
          <th>Delete</th>
        </tr>
      </thead>{
        page.map {
          user =>
            val action = if (user.superUser.is) { Text("Remove as Admin") } else { Text("Approve as Admin") }
            <tr>  {
              <td>{ user.id.is }</td> ++
                <td>{ user.firstName.is }</td> ++
                <td> { user.lastName.is } </td> ++
                <td> { Helpers.year(new java.util.Date) - user.birthyear.is } </td> ++
                <td>{ user.email.is }</td> ++
                <td>{
                  if (user.validated.is) {
                    user.validated.is
                  } else {
                    SHtml.button(Text("Confirm"), () => {
                      user.validated(true).save
                      S.redirectTo(Site.crudUsers.fullUrl)
                    })
                  }
                }</td> ++
                <td> {
                  SHtml.button(action, () => {
                    if (user.superUser.is) {
                      user.superUser(false).save
                    } else {
                      user.superUser(true).save
                    }
                    S.redirectTo(Site.crudUsers.fullUrl)
                  })
                }</td> ++
                <td> {
                  SHtml.button(Text("Delete"), () => {
                    user.delete
                    S.redirectTo(Site.crudUsers.fullUrl)
                  })
                }</td>
            }</tr>
        }
      }
    </table>
  }
}
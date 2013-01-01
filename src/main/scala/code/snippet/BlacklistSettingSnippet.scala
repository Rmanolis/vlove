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
import code.comet._
import net.liftweb.mapper._

class BlacklistSettingSnippet {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      cUser <- User.currentUser
    } yield {
      out = <table id="blacklist">
              <thead>
                <tr>
                  <td> Image </td>
                  <td> Name  </td>
                  <td> Unblock </td>
                </tr>
              </thead>
              <tbody>
                {
                  BlacklistUser.findByUser(cUser).map {
                    blu =>
                      <tr>
                        <td> { blu.blocked.obj.map(_.imgMiniPhoto(50, 50)).getOrElse(NodeSeq.Empty) }</td>
                        <td>{ blu.blocked.obj.map(_.getName).getOrElse("") } </td>
                        <td>{SHtml.button(Text("Unblock"),()=>{
                          blu.delete_!
                          S.redirectTo(Site.blacklist.fullUrl)
                        })}</td>
                      </tr>
                  }
                }
              </tbody>
            </table>
    }

    out
  }
}
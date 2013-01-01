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

private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)

class AddPhoto {
  import ImageHelpers._
  def render = {
    
    var isProfile = false
    var text = ""
    "#upload" #> SHtml.fileUpload { fp =>
      theUpload(Full(fp))
    } &
      "#isProfile" #> SHtml.checkbox(isProfile, isProfile = _) &
      "#text" #> SHtml.textarea(text, text = _) &
      "#submit" #> SHtml.button(Text("Submit"), () => {

        for {
          up <- theUpload.is
          photo <- Photo.add(up.mimeType)
          user <- User.currentUser
          pu <- PhotoUser.add(user, photo, isProfile, text)
        } yield {

          save(up.fileStream, Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + photo.name.is)
          saveBI(resize(up.fileStream, 200, 200), Props.get("vlove.photo.source").openOr("src/main/webapp/photos/") + photo.minName.is)
          S.redirectTo(Site.photos.fullUrl)
        }

      }) &
      "#cancel" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.photos.fullUrl)
      })
  }
}

class Photos extends PaginatorSnippet[PhotoUser]{
 override def count = PhotoUser.count
  override def itemsPerPage = 10
  override def page = PhotoUser.findAll(By(PhotoUser.user,User.currentUser),OrderBy(PhotoUser.id, Descending),StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))

  def renderPage(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
    } yield {
      out = <table id="tablePhotos"> {
        <tr>
          <th> Photo </th>
          <th> isProfile </th>
          <th> edit </th>
          <th> Delete </th>
        </tr> ++
          page.map {
            pu =>
              <tr> {
                <td><a href={ Site.editPhotoLoc.calcHref(pu.photo.obj.get) }><img src={ "/minphoto/" + pu.photo.is } height="100" width="100"/> </a> </td> ++
                  <td> {

                    if (pu.isProfile.is) {
                      pu.isProfile.is
                    } else {
                      button(Text("Βάλε την φώτο στο προφιλ"), () => {
                        pu.putOnProfile
                        S.redirectTo(Site.photos.fullUrl)
                      })
                    }
                  }</td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editPhotoLoc.calcHref(pu.photo.obj.get))
                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      pu.delete
                      S.redirectTo(Site.photos.fullUrl)
                    })
                  }</td>
              }</tr>
          }
      }</table>
    }
    out
  }

}

class EditPhoto {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      photo <- Site.editPhotoLoc.currentValue
      pu <- PhotoUser.findByPhoto(photo)
    } yield {
      if (pu.user.is == user.id.is) {
        var text = pu.explanation.is
        var isProfile = pu.isProfile.is
        out = (
          "#img" #> <img src={ "/photo/" + photo.id.is } height="500" width="500"/> &
          "#text" #> SHtml.textarea(text, text = _) &
          "#isProfile" #> SHtml.checkbox(isProfile, isProfile = _) &
          "#submit" #> SHtml.button(Text("Submit"), () => {
            pu.edit(isProfile, text)
            S.redirectTo(Site.photos.fullUrl)
          }) &
          "#cancel" #> SHtml.button(Text("Cancel"), () => {
            S.redirectTo(Site.photos.fullUrl)
          })).apply(in)
      }
    }
    out
  }
}
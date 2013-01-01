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

class SearchUserSnippet extends PaginatorSnippet[User] {
  private val paramList = List(
    "sex" -> "",
    "preference" -> "",
    "age" -> "",
    "county" -> "",
    "diet" -> "",
    "driving" -> "",
    "working" -> "",
    "drinking" -> "",
    "smoking" -> "",
    "dancing" -> "",
    "workingOut" -> "",
    "playingMusic" -> "")

  val age = S.param("age").flatMap(s => asInt(s).map(i => By(User.birthyear, (year(now) - i))))
  val county = S.param("county").flatMap(s => asLong(s).map(i => By(User.county, i)))
  val sex = S.param("sex").flatMap(s => asInt(s).map(i => By(User.sex, VeganGenders.mapped(i))))
  val preference = S.param("preference").flatMap(s => asInt(s).map(i => By(User.preference, VeganPreferences.mapped(i))))
  val diet = S.param("diet").flatMap(s => asInt(s).map(i => By(User.diet, VegeterianDiets.mapped(i))))
  val drinking = S.param("drinking").flatMap(s => asBoolean(s).map(b => By(User.isDrinking, b)))
  val driving = S.param("driving").flatMap(s => asBoolean(s).map(b => By(User.isDriving, b)))
  val smoking = S.param("smoking").flatMap(s => asBoolean(s).map(b => By(User.isSmoking, b)))
  val working = S.param("working").flatMap(s => asBoolean(s).map(b => By(User.isWorking, b)))
  val dancing = S.param("dancing").flatMap(s => asBoolean(s).map(b => By(User.isDancing, b)))
  val workingOut = S.param("workingOut").flatMap(s => asBoolean(s).map(b => By(User.isWorkingOut, b)))
  val playingMusic = S.param("playingMusic").flatMap(s => asBoolean(s).map(b => By(User.isPlayingMusic, b)))
  
  
  override def count = User.count
  override def itemsPerPage = 10

  override def pageUrl(offset: Long): String = appendParams(super.pageUrl(offset), paramList)
  override def page = {
    var list: List[net.liftweb.mapper.QueryParam[User]] = List()
    sex.map(list +:= _)
    preference.map(list +:= _)
    drinking.map(list +:= _)
    driving.map(list +:= _)
    smoking.map(list +:= _)
    working.map(list +:= _)
    age.map(list +:= _)
    county.map(list +:= _)
    list +:= OrderBy(User.registrationDate, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    User.findAll(list: _*)
  }

  def renderPage(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      me <- User.currentUser
    } yield {
      out = <table title="Users">
              <thead>
                <tr>
                  <th> Image </th>
                  <th> Name </th>
                  <th> Age </th>
                  <th> Message </th>
                  <th> Block </th>
                </tr>
              </thead>{
                page.filter(u => u.id.is != me.id.is && u.superUser.is == false).map {
                  other =>
                    <tr>  {
                      <td><a href={ Site.viewUserLoc.calcHref(other) }> { other.imgMiniPhoto(50, 50) } </a></td> ++
                        <td><a href={ Site.viewUserLoc.calcHref(other) }>{ other.getName }</a></td> ++
                        <td> { year(now) - other.birthyear.is } </td> ++
                        <td> {
                          VeganPermisions.messageButton(me, other)

                        } </td> ++
                        <td> {
                          VeganPermisions.blockButton(me, other)

                        } </td>
                    }</tr>
                }
              }
            </table>
    }
    out

  }

  def searchPage(in: NodeSeq): NodeSeq = {
    import scala.collection.mutable.HashMap
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
    } yield {

      val genders = VeganGenders.values.map { i => (i.id.toString, i.toString) }.toSeq
      val preferences = VeganPreferences.values.map { i => (i.id.toString, i.toString) }.toSeq
      val counties = County.findAll.map(c => (c.id.is.toString, c.name.is))
      val ages = (17 to 99).map(i => (i.toString, i.toString))
      val diets = VegeterianDiets.values.map { i => (i.id.toString, i.toString) }.toSeq
      var queries: HashMap[String, String] = HashMap()
      val (sex,preference) = VeganPreferences.preferSex(user.sex.is.id,user.preference.is.id)
      out = (
        "#sex" #> SHtml.select(genders, Full(sex.toString), s => {

          queries += ("sex" -> s)
        }) &
        "#preference" #> SHtml.select(preferences, Full(preference.toString), s => {
          queries += ("preference" -> s)
        }) &
        "#county" #> SHtml.select(counties, user.county.obj.map(_.id.is.toString), s => {
          queries += ("county" -> s)
        }) &
        "#age" #> SHtml.select(ages, Full((year(now) - user.birthyear.is).toString), s => {
          queries += ("age" -> s)
        }) &
        "#diet" #> SHtml.select(diets, Full(user.diet.is.id.toString), s => {
          queries += ("diet" -> s)
        }) &
        "#drinking" #> SHtml.checkbox(user.isDrinking.is, b => {
          if (b) {
            queries += ("drinking" -> "true")
          }
        }) &
        "#driving" #> SHtml.checkbox(user.isDriving.is, b => {
          if (b) {
            queries += ("driving" -> "true")
          }
        }) &
        "#smoking" #> SHtml.checkbox(user.isSmoking.is, b => {
          if (b) {
            queries += ("smoking" -> "true")
          }
        }) &
        "#working" #> SHtml.checkbox(user.isWorking.is, b => {
          if (b) {
            queries += ("working" -> "true")
          }
        }) &
        "#dancing" #> SHtml.checkbox(user.isDancing.is, b => {
          if (b) {
            queries += ("dancing" -> "true")
          }
        }) &
        "#workingOut" #> SHtml.checkbox(user.isWorkingOut.is, b => {
          if (b) {
            queries += ("workingOut" -> "true")
          }
        }) &
        "#playingMusic" #> SHtml.checkbox(user.isPlayingMusic.is, b => {
          if (b) {
            queries += ("playingMusic" -> "true")
          }
        }) &
        "#submit" #> SHtml.button(Text("Submit"), () => {
          val parms = queries.map {
            case (p, i) =>
              p + "=" + i
          }.toList.foldLeft("&")((s, i) => s + i + "&")
          S.redirectTo(Site.searchUsers.fullUrl + "?" + parms)
        })).apply(in)
    }

    out
  }
}
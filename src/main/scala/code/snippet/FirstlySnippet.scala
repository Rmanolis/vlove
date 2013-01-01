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

object SignUpMinScreen extends LiftScreen {

  object person extends ScreenVar(User.create)

  addFields(() => person.is.firstName)
  addFields(() => person.is.lastName)
  addFields(() => person.is.sex)
  addFields(() => person.is.preference)
  addFields(() => person.is.birthyear)
  val countyName = select("Διάλεξε τον νομό που μένεις", "Ελλάδα (Γενικά)", County.findAll.map(_.name.is))

  addFields(() => person.is.email)
  addFields(() => person.is.password)
  def finish() {
    County.findByName(countyName).map {
      c =>
        person.is.county(c)
    }
    person.is.addConfirmation
    person.is.registrationDate(Helpers.now)
    VeganMail.sendConfirmationEmail(person.email.is, person.is)
    S.notice("Το e-mail για την επικύρωση, έχει αποσταλθεί !")
    S.redirectTo(Site.home.fullUrl)
  }
}






class ConfirmationSnippet {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    Site.confirmationLoc.currentValue.map {
      user =>
        user.validated(true).save
        out = (
          "#mesage" #> "Ο λογαριασμός σας επικυρώθηκε").apply(in)
        S.notice("Ο λογαριασμός σας επικυρώθηκε !")
        S.redirectTo(Site.home.fullUrl)

    }.getOrElse {
      out = (
        "#mesage" #> "Η επικύρωση δεν είναι σωστή ").apply(in)
    }

    out
  }
}
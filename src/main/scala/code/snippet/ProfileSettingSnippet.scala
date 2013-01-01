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

object EditProfileScreen extends LiftScreen {

  object person extends ScreenVar(User.currentUser)

  addFields(() => person.is.get.firstName)
  addFields(() => person.is.get.lastName)
  addFields(() => person.is.get.email)
  addFields(() => person.is.get.sex)
  addFields(() => person.is.get.preference)
  addFields(() => person.is.get.diet)
  addFields(() => person.is.get.birthyear)
  val countyName = select("Διάλεξε τον νομό που μένεις", person.is.get.county.obj.map(_.name.is).getOrElse(""), County.findAll.map(_.name.is))
  addFields(() => person.is.get.introduction)
  addFields(() => person.is.get.isWorking)
  addFields(() => person.is.get.isDrinking)
  addFields(() => person.is.get.isDriving)
  addFields(() => person.is.get.isSmoking)
  
  addFields(() => person.is.get.isWorkingOut)
  addFields(() => person.is.get.isDancing)
  addFields(() => person.is.get.isPlayingMusic)
  
  
  def finish() {
    County.findByName(countyName).map {
      c =>
        person.is.get.county(c)
    }
    person.is.get.save
    S.redirectTo(Site.home.fullUrl)
  }

}



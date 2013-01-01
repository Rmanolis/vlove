package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._

class County extends LongKeyedMapper[County] with IdPK {
  def getSingleton = County
  object name extends MappedString(this, 400) {
    override def validations = valMinLen(1, "Το όνομα δεν μπορεί να είναι κενό") _ ::
      valMaxLen(400, "Το όνομα είναι πολύ μεγάλο") _ ::
      super.validations
  }
  object country extends MappedLongForeignKey(this, Country)
  def edit(name: String, country: Country) = {
    this.name(name).country(country)
    saveBox(this)
  }
}
object County extends County with LongKeyedMetaMapper[County] {
  def add(name: String, country: Country) = {
    val c = County.create
    c.name(name).country(country)
    saveBox(c)
  }
  def findByName(name: String) = {
    County.find(By(County.name, name))
  }
}

object DefaultCounty {
  def save {
    val grCounties = List("Ελλάδα (Γενικά)",
      "Νομός Αιτωλοακαρνανίας",
      "Νομός Αργολίδας",
      "Νομός Αρκαδίας",
      "Νομός Άρτας",
      "Νομός Αττικής",
      "Νομός Αχαΐας",
      "Νομός Βοιωτίας",
      "Νομός Γρεβενών",
      "Νομός Δράμας",
      "Νομός Δωδεκανήσου",
      "Νομός Έβρου",
      "Νομός Εύβοιας",
      "Νομός Ευρυτανίας",
      "Νομός Ζακύνθου",
      "Νομός Ηλείας",
      "Νομός Ημαθίας",
      "Νομός Ηρακλείου",
      "Νομός Θεσπρωτίας",
      "Νομός Θεσσαλονίκης",
      "Νομός Ιωαννίνων",
      "Νομός Καβάλας",
      "Νομός Καρδίτσας",
      "Νομός Καστοριάς",
      "Νομός Κέρκυρας",
      "Νομός Κεφαλληνίας",
      "Νομός Κιλκίς",
      "Νομός Κοζάνης",
      "Νομός Κορινθίας",
      "Νομός Κυκλάδων",
      "Νομός Λακωνίας",
      "Νομός Λάρισας",
      "Νομός Λασιθίου",
      "Νομός Λέσβου",
      "Νομός Λευκάδας",
      "Νομός Μαγνησίας",
      "Νομός Μεσσηνίας",
      "Νομός Ξάνθης",
      "Νομός Πέλλας",
      "Νομός Πιερίας",
      "Νομός Πρέβεζας",
      "Νομός Ρεθύμνου",
      "Νομός Ροδόπης",
      "Νομός Σάμου",
      "Νομός Σερρών",
      "Νομός Τρικάλων",
      "Νομός Φθιώτιδας",
      "Νομός Φλώρινας",
      "Νομός Φωκίδας",
      "Νομός Χαλκιδικής",
      "Νομός Χανίων",
      "Νομός Χίου")
      
    Country.findByName("Ελλάδα").map { c =>
      grCounties.map {
        grc =>

          if (County.findByName(grc).isEmpty) {
            County.add(grc, c)
          }
      }
    }
  }

}
package code.model
import net.liftweb.mapper._

object VeganGenders extends Enumeration {
  val Male = Value(1, "Άντρας")
  val Female = Value(2, "Γυναίκα")
  val Trans = Value(3, "Τρανς")
  val mapped = Map((1 -> Male), (2 -> Female), (3, Trans))
  val images = Map((1 -> "/images/male.png"), (2 -> "/images/female.png"), (3, "/images/trans.png"))

}

object VeganPreferences extends Enumeration {
  val Male = Value(1, "Άντρες")
  val Female = Value(2, "Γυναίκες")
  val Trans = Value(3, "Τρανς")
  val Bisexual = Value(4, "Bisexual")
  val mapped = Map((1 -> Male), (2 -> Female), (3 -> Trans), (4 -> Bisexual))

  def preference(user: User) = {
    var list: List[net.liftweb.mapper.QueryParam[User]] = List()
    if (user.sex.is == 1 && user.preference.is == 2) { //Antres gia gunaikes 12 == 21
      list +:= By(User.sex, VeganGenders.apply(2))
      list +:= By(User.preference, VeganPreferences.apply(1))
    } else if (user.sex.is == 2 && user.preference.is == 1) { // Gunaikes gia antres 21 == 12
      list +:= By(User.sex, VeganGenders.apply(1))
      list +:= By(User.preference, VeganPreferences.apply(2))
    } else if (user.sex.is == 1 && user.preference.is == 3) { // antres gia trans 13 == 31
      list +:= By(User.sex, VeganGenders.apply(3))
      list +:= By(User.preference, VeganPreferences.apply(1))
    } else if (user.sex.is == 3 && user.preference.is == 1) { // trans gia antres  31 == 13
      list +:= By(User.sex, VeganGenders.apply(1))
      list +:= By(User.preference, VeganPreferences.apply(3))
    } else if (user.sex.is == 2 && user.preference.is == 3) { // gunaikes gia trans 23 == 32
      list +:= By(User.sex, VeganGenders.apply(3))
      list +:= By(User.preference, VeganPreferences.apply(2))
    } else if (user.sex.is == 3 && user.preference.is == 2) { // trans gia gunaikes 32 == 23
      list +:= By(User.sex, VeganGenders.apply(2))
      list +:= By(User.preference, VeganPreferences.apply(3))
    } else if (user.sex.is == 1 && user.preference.is == 1) { // antres gia antres 11 == 11
      list +:= By(User.sex, VeganGenders.apply(1))
      list +:= By(User.preference, VeganPreferences.apply(1))
    } else if (user.sex.is == 2 && user.preference.is == 2) { // gunaikes gia gunaikes 22 == 22
      list +:= By(User.sex, VeganGenders.apply(2))
      list +:= By(User.preference, VeganPreferences.apply(2))
    } else if (user.sex.is == 3 && user.preference.is == 3) { // trans gia trans 33 == 33
      list +:= By(User.sex, VeganGenders.apply(3))
      list +:= By(User.preference, VeganPreferences.apply(3))
    } else if (user.sex.is == 1 && user.preference.is == 4) { // antres gia all 14 == 1
      list +:= By(User.preference, VeganPreferences.apply(1))
    } else if (user.sex.is == 2 && user.preference.is == 4) { // antres gia all 24 == 2
      list +:= By(User.preference, VeganPreferences.apply(2))
    } else if (user.sex.is == 3 && user.preference.is == 4) { // antres gia all 34 == 3
      list +:= By(User.preference, VeganPreferences.apply(3))
    }
    list
  }

  def preferSex(sex: Int, preference: Int) = {

    if (sex == 1 && preference == 2) { //Antres gia gunaikes 12 == 21
      (2, 1)
    } else if (sex == 2 && preference == 1) { // Gunaikes gia antres 21 == 12
      (1, 2)
    } else if (sex == 1 && preference == 3) { // antres gia trans 13 == 31
      (3, 1)
    } else if (sex == 3 && preference == 1) { // trans gia antres  31 == 13
      (1, 3)
    } else if (sex == 2 && preference == 3) { // gunaikes gia trans 23 == 32
      (3, 2)
    } else if (sex == 3 && preference == 2) { // trans gia gunaikes 32 == 23
      (2, 3)
    } else if (sex == 1 && preference == 1) { // antres gia antres 11 == 11
      (1, 1)
    } else if (sex == 2 && preference == 2) { // gunaikes gia gunaikes 22 == 22
      (2, 2)
    } else if (sex == 3 && preference == 3) { // trans gia trans 33 == 33
      (3, 3)
    } else if (sex == 1 && preference == 4) { // antres gia all 14 == 1
      (0, 1)
    } else if (sex == 2 && preference == 4) { // antres gia all 24 == 2
      (0, 2)
    } else if (sex == 3 && preference == 4) { // antres gia all 34 == 3
      (0, 3)
    } else {
      (0, 0)
    }
  }

}

object VegeterianDiets extends Enumeration {
  val noDairyEggHoney = Value(1, "Οχι στα γαλακτοκομικά , αυγά και μέλι")
  val noDairyEgg = Value(2, "Οχι στα γαλακτοκομικά και αυγά")
  val noDairy = Value(3, "Οχι στα γαλακτοκομικά")
  val yesDairyEggHoney = Value(4, "Ναι στα γαλακτοκομικά, αυγά και μέλι")
  val mapped = Map((1 -> noDairyEggHoney), (2 -> noDairyEgg), (3 -> noDairy))

}


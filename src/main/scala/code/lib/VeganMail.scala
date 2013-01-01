package code.lib
import net.liftweb.util.Mailer
import net.liftweb.util.Mailer._
import javax.mail.{ Authenticator, PasswordAuthentication }
import net.liftweb.common._
import net.liftweb.util.Props
import code.model.User

object VeganMail {

  def setupMailer {
    var isAuth = Props.get("mail.smtp.auth", "false").toBoolean

    Mailer.customProperties = Props.get("mail.smtp.host", "localhost") match {
      case "smtp.gmail.com" =>
        isAuth = true
        Map(
          "mail.smtp.host" -> "smtp.gmail.com",
          "mail.smtp.port" -> "587",
          "mail.smtp.auth" -> "true",
          "mail.smtp.starttls.enable" -> "true")
      case host => Map(
        "mail.smtp.host" -> host,
        "mail.smtp.port" -> Props.get("mail.smtp.port", "25"),
        "mail.smtp.auth" -> isAuth.toString)
    }

    if (isAuth) {
      (Props.get("mail.user"), Props.get("mail.password")) match {
        case (Full(username), Full(password)) =>
          Mailer.authenticator = Full(new Authenticator() {
            override def getPasswordAuthentication = new PasswordAuthentication(username, password)
          })
        case _ => new Exception("Username/password not supplied for Mailer.")
      }
    }
  }

  def sendConfirmationEmail(recipient: String,user:User) {
    Mailer.sendMail(From("rmanolis@live.com"), Subject("Επικυρώστε τον λογαριασμό σας"),
      List(PlainMailBodyType("Μπείτε σε αυτή την ιστοσελίδα για να επικυρωθεί ο λογαριασμός σας http://0.0.0.0:8080/firstly/confirmation/"+ user.confirmation.is), To(recipient)): _*)
      
  }

}
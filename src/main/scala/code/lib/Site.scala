package code.lib
import net.liftweb._
import common._
import http.S
import sitemap._
import sitemap.Loc._
import code.model._

case class MenuLoc(menu: Menu) {
  lazy val url: String = S.contextPath + menu.loc.calcDefaultHref
  lazy val fullUrl: String = S.hostAndPath + menu.loc.calcDefaultHref
}

object MenuGroups {
  val AdminGroup = LocGroup("admin")
  val SettingsGroup = LocGroup("settings")
  val SearchGroup = LocGroup("search")
  val UserGroup = LocGroup("user")

}
object Site {
  import MenuGroups._
  val home = MenuLoc(Menu.i("Home") / "index")
  val isLoggedIn = If(() => User.loggedIn_?, "Πρέπει να είστε μέλος της ιστοσελίδας")
  val isAdminLoggedIn = If(() => User.loggedIn_? && User.currentUser.map {
    u => u.superUser.is
  }.getOrElse(false), "You must be logged in")

  val crudUsers = MenuLoc(Menu.i("CRUD Users") / "admin" / "users" >> isAdminLoggedIn >> AdminGroup)
  val crudEvents = MenuLoc(Menu.i("CRUD Events") / "admin" / "events" >> isAdminLoggedIn >> AdminGroup)

  val signUp = MenuLoc(Menu.i("Sign Up") / "firstly" / "signup" >> If(() => !User.loggedIn_?, ""))

  private val confirmationMenu = Menu.param[User]("Confirmation", "Confirmation",
    User.findConfirmation _, _.id.is.toString) / "firstly" / "confirmation" >> Hidden
  lazy val confirmationLoc = confirmationMenu.toLoc

  val editProfile = MenuLoc(Menu.i("Edit Profile") / "settings" / "editProfile" >> isLoggedIn >> SettingsGroup)

  val addPhoto = MenuLoc(Menu.i("Add Photo") / "settings" / "addPhoto" >> isLoggedIn >> SettingsGroup)
  val photos = MenuLoc(Menu.i("Setting Photos") / "settings" / "photos" >> isLoggedIn >> SettingsGroup)
  private val editPhotoMenu = Menu.param[Photo]("Edit Photo", "Edit Photo",
    Photo.find _, _.id.is.toString) / "settings" / "editPhoto" >> Hidden >> SettingsGroup
  lazy val editPhotoLoc = editPhotoMenu.toLoc

  val addEvent = MenuLoc(Menu.i("Add Event") / "settings" / "addEvent" >> isLoggedIn >> SettingsGroup)
  val events = MenuLoc(Menu.i("Setting Events") / "settings" / "events" >> isLoggedIn >> SettingsGroup)
  private val editEventMenu = Menu.param[Event]("Edit Event", "Edit Event",
    Event.find _, _.id.is.toString) / "settings" / "editEvent" >> isLoggedIn >> Hidden >> SettingsGroup
  lazy val editEventLoc = editEventMenu.toLoc

  val blacklist = MenuLoc(Menu.i("Set Blacklist") / "settings" / "blacklist" >> isLoggedIn >> SettingsGroup)

  private val viewUserMenu = Menu.param[User]("View User", "View User",
    User.find _, _.id.is.toString) / "user" >> isLoggedIn >> Hidden >> UserGroup
  lazy val viewUserLoc = viewUserMenu.toLoc

  private val viewEventMenu = Menu.param[Event]("View Event", "View Event",
    Event.find _, _.id.is.toString) / "event" >> isLoggedIn >> Hidden >> UserGroup
  lazy val viewEventLoc = viewEventMenu.toLoc

  private val blockUserMenu = Menu.param[User]("Block User", "Block User",
    User.find _, _.id.is.toString) / "blockuser" >> isLoggedIn >> Hidden >> UserGroup
  lazy val blockUserLoc = blockUserMenu.toLoc

  private val flagEventMenu = Menu.param[Event]("Flag Event", "Flag Event",
    Event.find _, _.id.is.toString) / "flagevent" >> isLoggedIn >> Hidden >> UserGroup
  lazy val flagEventLoc = flagEventMenu.toLoc

  private val viewUsersGoingToEventMenu = Menu.param[Event]("View Users Going To Event", "View Users Going To Event",
    Event.find _, _.id.is.toString) / "viewUsersGoingToEvent" >> isLoggedIn >> Hidden >> UserGroup
  lazy val viewUsersGoingToEventLoc = viewUsersGoingToEventMenu.toLoc

  private val chatMenu = Menu.param[UserMessageBase]("Chat", "Chat",
    UserMessageBase.find _, _.id.is.toString) / "messenger" / "chat" >> isLoggedIn >> Hidden >> UserGroup
  lazy val chatLoc = chatMenu.toLoc

  val messages = MenuLoc(Menu.i("Messages") / "messenger" / "messages" >> isLoggedIn >> UserGroup)

  private val oldMessagesMenu = Menu.param[UserMessageBase]("Old Messages", "Old Messages",
    UserMessageBase.find _, _.id.is.toString) / "messenger" / "oldmessages" >> isLoggedIn >> Hidden >> UserGroup
  lazy val oldMessagesLoc = oldMessagesMenu.toLoc
  
  val eventsUserGoing = MenuLoc(Menu.i("Attending Events") / "attendingEvents" >> isLoggedIn >> UserGroup)
  val stalkers = MenuLoc(Menu.i("Stalkers") / "stalkers"  >> isLoggedIn >> UserGroup)
  val searchEvents = MenuLoc(Menu.i("Search Events") / "search" / "events" >> isLoggedIn >> SearchGroup)
  val searchUsers = MenuLoc(Menu.i("Search Users") / "search" / "users" >> isLoggedIn >> SearchGroup)
  
  

  private def menu = List(
    home.menu,
    User.loginMenuLoc.get,
    User.logoutMenuLoc.get,
    User.lostPasswordMenuLoc.get,
    User.resetPasswordMenuLoc.get,
    User.changePasswordMenuLoc.get,
    chatMenu,
    messages.menu,
    oldMessagesMenu,
    confirmationMenu,
    editProfile.menu,
    addPhoto.menu,
    editPhotoMenu,
    photos.menu,
    addEvent.menu,
    editEventMenu,
    events.menu,
    stalkers.menu,
    blacklist.menu,
    signUp.menu,
    crudUsers.menu,
    crudEvents.menu,
    viewUserMenu,
    viewEventMenu,
    blockUserMenu,
    flagEventMenu,
    viewUsersGoingToEventMenu,
    eventsUserGoing.menu,
    searchEvents.menu,
    searchUsers.menu)
  def siteMap = SiteMap(menu: _*)
}
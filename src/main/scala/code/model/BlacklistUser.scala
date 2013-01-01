package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._


class BlacklistUser extends LongKeyedMapper[BlacklistUser] with IdPK {
  def getSingleton = BlacklistUser
  
  object user extends MappedLongForeignKey(this, User)
  object blocked extends MappedLongForeignKey(this, User)
  object isImportant extends MappedBoolean(this)
  object time extends MappedDateTime(this)
  object reason extends MappedText(this)
}


object BlacklistUser  extends BlacklistUser with LongKeyedMetaMapper[BlacklistUser]  {
   def add(user:User, blocked:User,reason:String , isImportant:Boolean)={
     val bu = BlacklistUser.create
     bu.user(user).blocked(blocked).reason(reason).isImportant(isImportant).time(Helpers.now)
     saveBox(bu)
   }
   
   def findByUser(user:User)={
    findAll(By(BlacklistUser.user,user))
   }
   
   def isPairBlacklisted(user1:User,user2:User)={
      val search1 = find(By(BlacklistUser.user, user1),By(BlacklistUser.blocked, user2)).toList
      val search2 = find(By(BlacklistUser.user, user2),By(BlacklistUser.blocked, user1)).toList
      !(search1 ++ search2).isEmpty
   }
   
   def findMeBlockingTheOther(me:User,other:User)={
     find(By(BlacklistUser.user, me),By(BlacklistUser.blocked, other))
   }
}
package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.ModelHelpers._
class PhotoUser extends LongKeyedMapper[PhotoUser] with IdPK {
  def getSingleton = PhotoUser

  object photo extends MappedLongForeignKey(this, Photo)
  object user extends MappedLongForeignKey(this, User)
  object isProfile extends MappedBoolean(this)
  object explanation extends MappedText(this)

  def delete {
    for {
      ph <- this.photo.obj
    } yield {
      ph.delete
      this.delete_!
    }
  }
  def imgPhoto(height: Int, width: Int) = {
    <img src={ "/photo/" + photo.is } height={ height.toString } width={ width.toString }/>
  }
  def imgMiniPhoto(height: Int, width: Int) = {
    <img src={ "/miniphoto/" + photo.is } height={ height.toString } width={ width.toString }/>

  }
  def edit(isProfile: Boolean, expl: String) = {
    if (this.isProfile == false && isProfile == true) {
      PhotoUser.findAll(By(PhotoUser.user, this.user.is), By(PhotoUser.isProfile, true)).map {
        pu => pu.isProfile(false).save
      }
    }
    this.isProfile(isProfile).explanation(expl)
    saveBox(this)
  }
  def putOnProfile {
    PhotoUser.findAll(By(PhotoUser.user, this.user.is), By(PhotoUser.isProfile, true)).map {
      pu => pu.isProfile(false).save
    }
    this.isProfile(true).save
  }

}

object PhotoUser extends PhotoUser with LongKeyedMetaMapper[PhotoUser] {
  def add(user: User, photo: Photo, isProfile: Boolean, explanation: String) = {
    val pu = PhotoUser.create
    if (isProfile) {
      PhotoUser.findAll(By(PhotoUser.user, user), By(PhotoUser.isProfile, true)).map {
        p => p.isProfile(false).save
      }
    }
    pu.user(user).photo(photo).isProfile(isProfile).explanation(explanation)
    saveBox(pu)
  }

  def findByUser(user: User) = {
    PhotoUser.findAll(By(PhotoUser.user, user))
  }
  def findByPhoto(photo: Photo) = {
    PhotoUser.find(By(PhotoUser.photo, photo))
  }
}
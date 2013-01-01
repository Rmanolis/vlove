package code.lib
import net.liftweb.http.rest.RestHelper
import java.io._
import net.liftweb.util._
import code.model._
import net.liftweb.http.StreamingResponse
import net.liftweb.http.BadResponse
import net.liftweb.http.LiftResponse


object VeganRest extends RestHelper {
  serve {

    case "photo" :: imageName :: Nil Get req => {

      Photo.find(imageName).map {
        ph =>
          val file = new File(ph.source.is)
          val inputStream = new FileInputStream(ph.source.is)

          val headers: List[(String, String)] =
            List(
              ("Content-disposition", "attachment; charset=utf-8; filename=" + ph.name.is))

          StreamingResponse(inputStream, () => inputStream.close(), file.length, headers, Nil, 200)
      }.getOrElse[LiftResponse]{
        BadResponse()
      }

    }
    
    case "miniphoto" :: imageName :: Nil Get req => {
      Photo.find(imageName).map {
        ph =>
          val file = new File(ph.minSource.is)
          val inputStream = new FileInputStream(ph.minSource.is)

          val headers: List[(String, String)] =
            List(
              ("Content-disposition", "attachment; charset=utf-8; filename=" + ph.minName.is))

          StreamingResponse(inputStream, () => inputStream.close(), file.length, headers, Nil, 200)
      }.getOrElse[LiftResponse]{
        BadResponse()
      }
    }
  }
}
package code.lib
import net.liftweb.http.JsonResponse
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{ InMemoryResponse, StreamingResponse }
import net.liftweb.http.S
import net.liftweb.http.FileParamHolder
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.common.{ Box, Full }
import net.liftweb.http.BadResponse
import net.liftweb.util.StringHelpers
import code.model._
import java.io._
import org.apache.commons.io.IOUtils.copy



object UploadManager extends RestHelper {
  serve {
    case "uploading" :: Nil Post req => {
      def saveImage(fph: FileParamHolder) = {
        val imageName = StringHelpers.randomString(16)
        
        
     val dest = new File("src/main/webapp/images/user/" + imageName)
       copy(fph.fileStream, new FileOutputStream(dest))
    
        ("name" -> imageName) ~ ("type" -> fph.mimeType) ~ ("size" -> fph.length)
      }

      val ojv: Box[JValue] = req.uploadedFiles.map(fph => saveImage(fph)).headOption
      val ajv = ("name" -> "n/a") ~ ("type" -> "n/a") ~ ("size" -> 0L)
      val ret = ojv openOr ajv

      val jr = JsonResponse(ret).toResponse.asInstanceOf[InMemoryResponse]
      InMemoryResponse(jr.data, ("Content-Length", jr.data.length.toString) ::
        ("Content-Type", "text/plain") :: Nil, Nil, 200)
    }

    case "serving" :: imageName :: Nil Get req => {
      /*Photo.find(imageName) match {
        case Full(image) =>
          val file = new File("");
          
          val imageStream = new FileInputStream(file)
          var data = scala.io.Source.fromFile(file,"aaaa")
          StreamingResponse(imageStream, () => imageStream.close(), image.length, ("Content-Type", image.contentType) :: Nil, Nil, 200)
        case _ => new BadResponse
      }*/

      val file = new File("src/main/webapp/images/user/" + imageName)
      val inputStream = new FileInputStream(file)

      val headers: List[(String, String)] =
        List(("Content-type", "image/jpeg"),
          ("Content-length", file.length.toString),
          ("Content-disposition", "attachment; filename=" + imageName + ".jpeg"))

      StreamingResponse(inputStream, () => inputStream.close(), file.length, headers, Nil, 200)

    }
  }
}
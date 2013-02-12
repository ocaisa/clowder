/**
 *
 */
package controllers

import play.api.mvc.Controller
import services.Services
import play.api.Logger
import play.api.mvc.Action
import play.api.data.Form
import play.api.data.Forms._
import models.Dataset
import views.html.defaultpages.error
import java.io.FileInputStream
import play.api.Play.current
import services.RabbitmqPlugin
import services.ElasticsearchPlugin
import java.io.File

/**
 * A dataset is a collection of files and streams.
 * 
 * @author Luigi Marini
 *
 */
object Datasets extends Controller with securesocial.core.SecureSocial {
   
  /**
   * New dataset form.
   */
  val datasetForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "description" -> nonEmptyText
    )
    ((name, description) => Dataset(name = name, description = description))
    ((dataset: Dataset) => Some((dataset.name, dataset.description)))
   )
   
  def newDataset() = Action {
    Ok(views.html.newDataset(datasetForm))
  }
  
  def createDataset() = Action { implicit request =>
    datasetForm.bindFromRequest.fold(
        failure => BadRequest("Oops"),
        {case dataset => {
          Dataset.save(dataset)
          Redirect(routes.Datasets.dataset(dataset.id.toString))   
          }
        }
    )
  }
   
  /**
   * List datasets.
   */
  def list() = Action {
    Services.files.listFiles().map(f => Logger.debug(f.toString))
    Ok(views.html.datasetList(Services.datasets.listDatasets()))
  }
  
  /**
   * Dataset.
   */
  def dataset(id: String) = Action {
    Services.datasets.get(id)  match {
      case Some(dataset) => Ok(views.html.dataset(dataset))
      case None => {Logger.error("Error getting dataset" + id); InternalServerError}
    }
  }
  
  
  def upload = Action(parse.temporaryFile) { request =>
    request.body.moveTo(new File("/tmp/picture"))
    Ok("File uploaded")
  }
  
  /**
   * Upload file.
   */
  def submit() = Action(parse.multipartFormData) { implicit request =>
    
    datasetForm.bindFromRequest.fold(
        failure => BadRequest("Oops"),
        {case dataset => {
          Dataset.save(dataset)
          Redirect(routes.Datasets.dataset(dataset.id.toString))   
          }
        }
    )
    
      request.body.file("File").map { f =>        
        Logger.info("Uploading file " + f.filename)
        // store file
        val id = Services.files.save(new FileInputStream(f.ref.file), f.filename)
        // submit file for extraction
        current.plugin[RabbitmqPlugin].foreach{_.extract(id)}
        // index file 
        if (current.plugin[ElasticsearchPlugin].isDefined) {
	        Services.files.getFile(id).foreach { file =>
	          current.plugin[ElasticsearchPlugin].foreach{
	            _.index("files","file",id,List(("filename",f.filename), 
	              ("contentType", file.contentType)))}
	        }
        }
        // redirect to file page
        Redirect(routes.Files.file(id))    
      }.getOrElse {
         BadRequest("File not attached.")
      }
  }
}
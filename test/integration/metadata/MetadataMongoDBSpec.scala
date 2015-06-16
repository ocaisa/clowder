package integration.metadata

import java.net.URL
import java.util.Date
import com.google.inject.Guice
import models.{ResourceRef, UUID, Metadata, UserAgent}
import org.scalatestplus.play.{OneServerPerSuite, PlaySpec}
import services.{DI, MetadataService, FileService}
import play.api.libs.json.JsObject
import play.api.libs.json.Json

/**
 * Test Metadata MongoDB service.
 *
 * @author Smruti Padhy
 */
class MetadataMongoDBSpec extends PlaySpec with OneServerPerSuite{
  val testCreator = UserAgent(id= UUID.generate, typeOfAgent="cat:user", userId = Some(new URL("http://dts.ncsa.illinois.edu/user06")))
  val fileId = UUID.generate
  val datasetId = UUID.generate
  val testcontent = Json.obj("score"->Json.arr("3.4"),"category"->Json.arr("cat"))
  val contextId = UUID.generate
  val testMetadata = Metadata(
    id = UUID.generate,
    attachedTo = ResourceRef("file", fileId),
    contextId = Some(contextId),
    createdAt = new Date,
    creator = testCreator,
    content = testcontent)
        
  "The Metadata MongoDB Service" must {
    "be able to add, retrieve and remove Metadata" in {
      info("insert metadata")
      val injector = Guice.createInjector(new services.ConfigurationModule)
      val metadata : MetadataService =  injector.getInstance(classOf[MetadataService])
      info("testMetadata"+ testMetadata)
      val id = metadata.addMetadata(testMetadata)
      info("new metadata added " + id)
      val retrievedMetadata = metadata.getMetadataById(id)
      info("retrieving metadata " + retrievedMetadata)
      
      val mdByattachTo = metadata.getMetadataByAttachTo(ResourceRef("file", fileId))
      info("Get Metadata By attachedTo field" + mdByattachTo)
      
      val mdByCreator = metadata.getMetadataByCreator(ResourceRef("file", fileId), "cat:user")
      info("Get Metadata By creator field" + mdByCreator)
      
      val mdByContextId = metadata.getMetadataContext(id)
      info("Get Metadata context " + mdByContextId)
    }
   }
}
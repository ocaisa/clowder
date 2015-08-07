package services.mongodb

import java.net.URL
import java.util.Date

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.commons.MongoDBObject
import models._
import play.api.{ Plugin, Logger, Application }
import play.api.Play.current
import com.mongodb.casbah.MongoURI
import com.mongodb.casbah.MongoConnection
import com.mongodb.casbah.MongoDB
import com.mongodb.casbah.MongoCollection
import com.mongodb.casbah.gridfs.GridFS
import services.{DI, AppConfigurationService}

/**
 * Mongo Salat service.
 */
class MongoSalatPlugin(app: Application) extends Plugin {
  // URI to the mongodatabase, for example mongodb://127.0.0.1:27017/medici
  var mongoURI: MongoURI = null

  // hold the connection, if connection failed it will be tried to open next time
  var mongoConnection: MongoConnection = null

  override def onStart() {
    mongoURI = if (play.api.Play.configuration.getString("mongodbURI").isDefined) {
      MongoURI(play.api.Play.configuration.getString("mongodbURI").get)
    } else if (play.api.Play.configuration.getString("mongodb.default").isDefined) {
      Logger.info("mongodb.default is deprecated, please use mongodbURI")
      MongoURI(play.api.Play.configuration.getString("mongodb.default").get)
    } else {
      Logger.info("no connection to mongo specified in , will use default URI mongodb://127.0.0.1:27017/medici")
      MongoURI("mongodb://127.0.0.1:27017/medici")
    }

    // connect to the database
    mongoConnection = mongoURI.connect.fold(l => throw l, r => r)

    // update database if needed
    updateDatabase()

    // create indices.
    Logger.debug("Ensuring indices exist")
    collection("collections").ensureIndex(MongoDBObject("created" -> -1))
    
    collection("datasets").ensureIndex(MongoDBObject("created" -> -1))
    collection("datasets").ensureIndex(MongoDBObject("tags" -> 1))
    collection("datasets").ensureIndex(MongoDBObject("files._id" -> 1))
    
    collection("uploads.files").ensureIndex(MongoDBObject("uploadDate" -> -1))
    collection("uploads.files").ensureIndex(MongoDBObject("tags" -> 1))
    collection("uploadquery.files").ensureIndex(MongoDBObject("uploadDate" -> -1))
    
    collection("previews.files").ensureIndex(MongoDBObject("uploadDate" -> -1, "file_id" -> 1))
    collection("previews.files").ensureIndex(MongoDBObject("uploadDate" -> -1, "section_id" -> 1))
    
    collection("textures.files").ensureIndex(MongoDBObject("file_id" -> 1))
    collection("tiles.files").ensureIndex(MongoDBObject("preview_id" -> 1, "filename" -> 1,"level" -> 1))
    
    collection("sections").ensureIndex(MongoDBObject("uploadDate" -> -1, "file_id" -> 1))
    
    collection("dtsrequests").ensureIndex(MongoDBObject("startTime" -> -1, "endTime" -> -1))
    collection("versus.descriptors").ensureIndex(MongoDBObject("fileId" -> 1))

  }

  override def onStop() {
    if (mongoConnection != null)
      mongoConnection.close()
    mongoConnection = null
  }

  /**
   * Returns the database for the connection
   */
  def getDB: MongoDB = mongoConnection.getDB(mongoURI.database.getOrElse("medici"))

  /**
   * Returns a collection in the database
   */
  def collection(collection: String): MongoCollection = getDB(collection)
  
  /**
   * Returns a GridFS for writing files, the files will be placed in
   * two collections that start with the prefix (&lt;prefix&gt;.fs and
   * &lt;prefix.chunks&gt;).
   */
  def gridFS(prefix: String = "fs"): GridFS = GridFS(getDB, prefix)

  /**
   * Drop all collections
   */
  def dropAllData() {
    Logger.debug("**DANGER** Deleting data collections **DANGER**")
    collection("collections").drop()
    collection("comments").drop()
    collection("datasets").drop()
    collection("dtsrequests").drop()
    collection("extractions").drop()
    collection("extractor.servers").drop()
    collection("extractor.names").drop()
    collection("extractor.inputtypes").drop()
    collection("multimedia.features").drop()
    collection("previews.chunks").drop()
    collection("previews.files").drop()
    collection("sections").drop()
    collection("streams").drop()
    collection("thumbnails.chunks").drop()
    collection("thumbnails.files").drop()
    collection("uploads.chunks").drop()
    collection("uploads.files").drop()
    collection("uploadquery.files").drop()
    collection("versus.descriptors").drop()
    collection("spaces.projects").drop()
    collection("spaces.users").drop()
    Logger.debug("**DANGER** Data deleted **DANGER**")
  }

  // ----------------------------------------------------------------------
  // CODE TO UPDATE THE DATABASE
  // ----------------------------------------------------------------------
  def updateDatabase() {
    val appConfig: AppConfigurationService = DI.injector.getInstance(classOf[AppConfigurationService])

    // migrate users to new model
    if (!appConfig.hasPropertyValue("mongodb.updates", "fixing-typehint-users")) {
      if (System.getProperty("MONGOUPDATE") != null) {
        Logger.info("[MongoDBUpdate] : Fixing _typeHint for users.")
        val q = MongoDBObject("_typeHint" -> "securesocial.core.SocialUser")
        val o = MongoDBObject("$set" -> MongoDBObject("_typeHint" -> "models.ClowderUser"))
        collection("social.users").update(q, o, multi=true, concern=WriteConcern.Safe)
        appConfig.addPropertyValue("mongodb.updates", "fixing-typehint-users")
      } else {
        Logger.warn("[MongoDBUpdate] : Missing fix _typeHint for users.")
      }
    }

    // add a space if none exists
    if (!appConfig.hasPropertyValue("mongodb.updates", "convert-to-spaces")) {
      if (System.getProperty("MONGOUPDATE") != null) {
        val datasets = collection("datasets").count(new MongoDBObject())
        val collections = collection("collections").count(new MongoDBObject())
        val users = collection("social.users").count(new MongoDBObject())
        if ((datasets != 0) || (collections != 0)) {
          Logger.info("[MongoDBUpdate] : Found datasets/collections, will add all to default space")

          // create roles (this is called before Global)
          if (RoleDAO.count() == 0) {
            RoleDAO.save(Role.Admin)
            RoleDAO.save(Role.Editor)
            RoleDAO.save(Role.Viewer)
          }

          // create the space
          val spacename = java.net.InetAddress.getLocalHost.getHostName
          val newspace = new ProjectSpace(name=spacename, description="", created=new Date(), creator=UUID("000000000000000000000000"),
            homePage=List.empty[URL], logoURL=None, bannerURL=None, metadata=List.empty[Metadata],
            collectionCount=collections.toInt, datasetCount=datasets.toInt, userCount=users.toInt)
          ProjectSpaceDAO.save(newspace)
          val spaceId = new ObjectId(newspace.id.stringify)

          // add space to all datasets/collections
          val q = MongoDBObject()
          val o = MongoDBObject("$set" -> MongoDBObject("spaces" -> List[ObjectId](spaceId)))
          collection("datasets").update(q ,o, multi=true)
          collection("collections").update(q ,o, multi=true)

          // add all users as admin
          val adminRole = collection("roles").findOne(MongoDBObject("name" -> "Admin"))
          val spaceRole = MongoDBObject("_typeHint" -> "models.UserSpaceAndRole", "spaceId" -> spaceId, "role" -> adminRole)
          collection("social.users").update(MongoDBObject(), $push("spaceandrole" -> spaceRole), multi=true)

        } else {
          Logger.info("[MongoDBUpdate] : No datasets/collections found, will not create default space")
        }
        appConfig.addPropertyValue("mongodb.updates", "convert-to-spaces")
      } else {
        Logger.warn("[MongoDBUpdate] : Missing fix to convert to spaces.")
      }
    }
  }
}

/**
 *
 */
package models

import org.bson.types.ObjectId
import services.MongoSalatPlugin
import com.novus.salat.dao.{ModelCompanion, SalatDAO}
import org.bson.types.ObjectId
import MongoContext.context
import play.api.Play.current
import play.api.Logger
import com.mongodb.casbah.commons.MongoDBObject
import com.mongodb.casbah.WriteConcern
import com.mongodb.casbah.Imports._

/**
 * A portion of a file.
 * 
 * @author Luigi Marini
 *
 */
case class Section (
    id: ObjectId = new ObjectId,
    file_id: ObjectId = new ObjectId,
    order: Int = -1,
    startTime: Option[Int] = None, // in seconds
    endTime: Option[Int] = None, // in seconds
    area: Option[Rectangle] = None,
    preview: Option[Preview] = None,
    comments: List[Comment] = List.empty,
    tags: List[String] = List.empty
)

case class Rectangle (
    x: Int,
    y: Int,
    w: Int,
    h: Int
)
    
object SectionDAO extends ModelCompanion[Section, ObjectId] {
  val dao = current.plugin[MongoSalatPlugin] match {
    case None    => throw new RuntimeException("No MongoSalatPlugin");
    case Some(x) =>  new SalatDAO[Section, ObjectId](collection = x.collection("sections")) {}
  }
  
  def findByFileId(id: ObjectId): List[Section] = {
    dao.find(MongoDBObject("file_id"->id)).sort(MongoDBObject("startTime"->1)).toList
  }

  def tag(id: String, tag: String) { 
    dao.collection.update(MongoDBObject("_id" -> new ObjectId(id)),  $addToSet("tags" -> tag), false, false, WriteConcern.Safe)
  }

  def comment(id: String, comment: Comment) {
    dao.update(MongoDBObject("_id" -> new ObjectId(id)), $addToSet("comments" -> Comment.toDBObject(comment)), false, false, WriteConcern.Safe)
  }
}
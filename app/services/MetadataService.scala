package services

import play.api.libs.json.JsValue
import models.{ResourceRef, UUID, Metadata}

/**
 * MetadataService for add and query metadata
 */
trait MetadataService {
  
  /** Add metadata to the metadata collection and attach to a section /file/dataset/collection */
  def addMetadata(metadata: Metadata) : UUID
  
  /** Get Metadata By Id*/
  def getMetadataById(id : UUID) : Option[Metadata]
  
  /** Get Metadata based on Id of an element (section/file/dataset/collection) */
  def getMetadataByAttachTo(resourceRef: ResourceRef): List[Metadata]

  /** Get metadata based on type i.e. user generated metadata or technical metadata  */
  def getMetadataByCreator(resourceRef: ResourceRef, typeofAgent:String): List[Metadata]

  /** Remove metadata */
  def removeMetadata(metadataId: UUID)
  
  /** Get metadata context if available */
  def getMetadataContext(metadataId: UUID): Option[JsValue]

  /** update Metadata */  
  def updateMetadata(metadataId: UUID, json: JsValue)
}
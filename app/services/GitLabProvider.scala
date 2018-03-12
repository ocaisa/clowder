package services

import play.api.libs.ws.WS
import play.api.{Application, Logger}
import play.api.libs.json.JsObject
import securesocial.core._


/**
 * A GitLab OAuth2 Provider
 */
class GitLabProvider(application: Application) extends OAuth2Provider(application) {
  val Error = "error"
  val Message = "message"
  val Type = "type"
  val Id = "username"
  val Name = "name"
  val Avatar = "avatar_url"
  val Email = "email"


  override def id = GitLabProvider.GitLab

  def fillProfile(user: SocialUser): SocialUser = {
    val UserInfoApi = loadProperty("userinfoUrl").getOrElse(throwMissingPropertiesException())
    val accessToken = user.oAuth2Info.get.accessToken
    val promise = WS.url(UserInfoApi.toString).withHeaders(("Authorization", "Bearer " + accessToken)).get()

    try {
      val response = awaitResult(promise)
      val me = response.json
      (me \ Error).asOpt[JsObject] match {
        case Some(error) =>
          val message = (error \ Message).as[String]
          val errorType = ( error \ Type).as[String]
          Logger.error("[securesocial] error retrieving profile information from GitLab. Error type = %s, message = %s"
            .format(errorType,message))
          throw new AuthenticationException()
        case _ =>
          val userId = (me \ Id).as[String]
          val fullName = (me \ Name).asOpt[String]
          val avatarUrl = ( me \ Avatar).asOpt[String]
          val email = ( me \ Email).asOpt[String]
          user.copy(
            identityId = IdentityId(userId, id),
            fullName = fullName.getOrElse(""),
            avatarUrl = avatarUrl,
            email = email
          )
      }
    } catch {
      case e: Exception => {
        Logger.error( "[securesocial] error retrieving profile information from GitLab", e)
        throw new AuthenticationException()
      }
    }
  }
}

object GitLabProvider {
  val GitLab = "gitlab"
}

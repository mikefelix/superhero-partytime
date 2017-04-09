package controllers

import javax.inject._

import play.api.Play
import play.api.cache.CacheApi
import play.api.mvc._
import play.api.Play.current

@Singleton
class OptionsController @Inject()(cache: CacheApi) extends Controller {
  def options(path: String) = Action { request =>
    val methods = List("OPTIONS") ++ getMethods(request)
    if (methods.length > 1)
      NoContent.withHeaders(List("Allow" -> methods.mkString(", ")): _*)
    else
      NotFound
  }

  val methodList = List("GET", "POST", "PUT", "DELETE", "PATCH")
  def getMethods(request: Request[AnyContent]) : List[String] = {
      cache.getOrElse[List[String]]("options.url." + request.uri) {
          for(m <- methodList; if Play.application.routes.handlerFor(new RequestHeader {
              val method = m
              val remoteAddress = request.remoteAddress
              val headers = request.headers
              val queryString = request.queryString
              val version = request.version
              val path = request.path
              val uri = request.uri
              val tags = request.tags
              val id = request.id
              val secure = request.secure
              val clientCertificateChain = request.clientCertificateChain
          }).isDefined) yield m
      }
  }
}
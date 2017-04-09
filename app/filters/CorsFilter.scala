package filters

import javax.inject.Inject

import akka.stream.Materializer
import play.api.mvc._

import scala.concurrent.{ ExecutionContext, Future }

class CorsFilter @Inject() (implicit val mat: Materializer, ec: ExecutionContext) extends Filter {
  def apply(nextFilter: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] = {
    val headers = request.headers.keys ++ Set("Authorization, Accept, Origin, Content-type, X-Json, X-Prototype-Version, X-Requested-With")
    nextFilter(request).map { result =>
      result.withHeaders(
        "Access-Control-Allow-Origin" -> "*",
        "Access-Control-Allow-Methods" -> "GET,POST,PUT,PATCH,DELETE,OPTIONS",
        "Access-Control-Allow-Headers" -> headers.mkString(","),
        "Access-Control-Allow-Credentials" -> "true",
        "Access-Control-Max-Age" -> (60 * 60 * 24).toString // 1 Day
      )
    }
  }
}


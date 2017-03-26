package filters

import javax.inject.Inject

import play.api.http.HttpFilters

class AppFilters @Inject() (cors: CorsFilter) extends HttpFilters {

  val filters = Seq(cors)
}
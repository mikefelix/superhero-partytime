package services

/**
  * Util
  * User: michael.felix
  * Date: 4/5/17
  */
object Util {
  def jsSafe(s: String) = s.replaceAll("\"", "\\\\\"").replaceAll("[\n\r]", " ")
}

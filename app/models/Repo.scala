package models

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile
import slick.jdbc.JdbcBackend

import scala.concurrent.Future

/**
  * Repo
  * User: michael.felix
  * Date: 11/26/16
  */
trait Repo[T] {
  val tableName: String
  protected val dbConfigProvider: DatabaseConfigProvider
  val dbConfig = dbConfigProvider.get[JdbcProfile]
  val db: JdbcBackend#DatabaseDef = dbConfig.db

  //  private val tableQuery = dbConfig.driver.api.TableQuery[QuestsTable]

//  type MyTable = dbConfig.driver.api.Table[T]

//  val tableQuery: dbConfig.driver.api.TableQuery[MyTable]

  def all: Future[List[T]]
  def create(t: T): Future[String]
  def findById(id: String): Future[Option[T]]
}

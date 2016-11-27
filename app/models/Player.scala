package models

import javax.inject.Inject

import play.api.db.slick.DatabaseConfigProvider
import slick.driver.JdbcProfile

import scala.concurrent.Future

/**
  * Player
  * User: michael.felix
  * Date: 11/26/16
  */
case class Player(id: String, gameId: String, name: String)

class PlayerRepo @Inject() (itemRepo: ItemRepo)(protected val dbConfigProvider: DatabaseConfigProvider) extends Repo[Player] {

  val dbConfig = dbConfigProvider.get[JdbcProfile]
  import dbConfig.driver.api._

  val db = dbConfig.db

  private val Players = TableQuery[PlayersTable]

  def findByGame(gameId: String): Future[List[Player]] = {
    val query = Players.filter(_.gameId === gameId).to[List]
    db.run(query.result)
  }

  override def all = db.run(Players.to[List].result)

  override def findById(id: String): Future[Option[Player]] = db.run(Players.filter(_.id === id).result.headOption)

  override def create(player: Player): Future[String] = {
    db.run(Players returning Players.map(_.id) += player)
  }

  private class PlayersTable(tag: Tag) extends Table[Player](tag, "PLAYERS") {

    def id = column[String]("ID", O.AutoInc, O.PrimaryKey)
    def gameId = column[String]("GAME")
    def name = column[String]("NAME")

    def * = (id, name) <> (Player.tupled, Player.unapply)
    def ? = (id.?, gameId.?, name.?).shaped.<>({ r => r._1.map(_ => Player.tupled((r._1.get, r._2.get, r._3.get))) }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))

  }
}




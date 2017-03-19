package models

import java.sql.{Date => SqlDate}
import java.util.Date

import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json.Json.{toJsFieldJsValueWrapper => js}
import play.api.libs.json.{JsValue, Json}
import slick.driver.PostgresDriver.api._
import slick.lifted.Tag

/*
case class Page[A](items: Seq[A], page: Int, offset: Long, total: Long) {
  lazy val prev = Option(page - 1).filter(_ >= 0)
  lazy val next = Option(page + 1).filter(_ => (offset + items.size) < total)
}

case class Employee(id: Option[Long], name: String, address: String, dob: Option[Date], joiningDate: Date, designation: Option[String])
*/

trait Model {
  def toJson(extras: (String, JsValueWrapper)*): JsValue
}

trait Requirement {
  val name: String
  val description: String
}

case class Game(id: Long, name: String) extends Model {
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "name").validate[String].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("name" -> js(name)) +:
    extras: _*
  )
}

case class Player(id: Long, game: Long, name: String) extends Model {
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "name").validate[String].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("name" -> js(name)) +:
    extras: _*
  )
}

case class Item(id: Long, game: Long, name: String, description: String, owner: Option[Long]) extends Model with Requirement {
  def this(id: Long) = this(id, 0, "", "", None)
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "name").validate[String].get,
    (body \ "description").validate[String].get,
    (body \ "owner").validateOpt[Long].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("name" -> js(name)) +:
    ("description" -> js(description)) +:
    ("owner" -> js(owner)) +:
    extras: _*
  )
}

case class Power(id: Long, game: Long, name: String, description: String) extends Model with Requirement {
  def this(id: Long) = this(id, 0, "", "")
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "name").validate[String].get,
    (body \ "description").validate[String].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("name" -> js(name)) +:
    ("description" -> js(description)) +:
    extras: _*
  )
}

case class Quest(id: Long, name: String, description: String, game: Long) extends Model {
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "name").validate[String].get,
    (body \ "description").validate[String].get,
    (body \ "game").validate[Long].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("name" -> js(name)) +:
    ("description" -> js(description)) +:
    extras: _*
  )
}

case class PlayerQuest(player: Long, quest: Long)
case class PlayerPower(player: Long, power: Long)
case class GameItem(game: Long, item: Long)
case class QuestItem(quest: Long, item: Long)
case class QuestPower(quest: Long, power: Long)

/*
class Employees(tag: Tag) extends Table[Employee](tag, "EMPLOYEE") {

  implicit val dateColumnType = MappedColumnType.base[Date, Long](d => d.getTime, d => new Date(d))

  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def address = column[String]("address")
  def dob = column[Date]("date_of_birth")
  def joiningDate = column[Date]("joining_date")
  def designation = column[String]("designation")

  def * = (id.?, name, address, dob.?, joiningDate, designation.?) <> (Employee.tupled, Employee.unapply)
}
*/

class Items(tag: Tag) extends Table[Item](tag, "items") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def name = column[String]("name")
  def description = column[String]("description")
  def owner = column[Option[Long]]("owner")

  def * = (id, game, name, description, owner) <> (Item.tupled, Item.unapply)
}

class Powers(tag: Tag) extends Table[Power](tag, "powers") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def name = column[String]("name")
  def description = column[String]("description")

  def * = (id, game, name, description) <> (Power.tupled, Power.unapply)
}

class Games(tag: Tag) extends Table[Game](tag, "games") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def name = column[String]("name")

  def * = (id, name) <> (Game.tupled, Game.unapply)
//    def ? = (id.?, gameId.?, name.?).shaped.<>({ r => r._1.map(_ => Player.tupled((r._1.get, r._2.get, r._3.get))) }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))
}

class Players(tag: Tag) extends Table[Player](tag, "players") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def name = column[String]("name")

  def * = (id, game, name) <> (Player.tupled, Player.unapply)
//    def ? = (id.?, gameId.?, name.?).shaped.<>({ r => r._1.map(_ => Player.tupled((r._1.get, r._2.get, r._3.get))) }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))
}

class Quests(tag: Tag) extends Table[Quest](tag, "quests") {
  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def name = column[String]("name")
  def description = column[String]("description")
  def game = column[Long]("game")

  def * = (id, name, description, game) <> (Quest.tupled, Quest.unapply)
//  def ? = (id.?, name.?, desc.?).shaped.<>({ r =>
//    r._1.map(_ => Quest.tupled((r._1.get, r._2.get, r._3.get)))
//  }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))
}

class PlayerQuests(tag: Tag) extends Table[PlayerQuest](tag, "player_quests"){
  def quest = column[Long]("quest")
  def player = column[Long]("player")

  def * = (player, quest) <> (PlayerQuest.tupled, PlayerQuest.unapply)
}

class PlayerPowers(tag: Tag) extends Table[PlayerPower](tag, "player_powers"){
  def power = column[Long]("power")
  def player = column[Long]("player")

  def * = (player, power) <> (PlayerPower.tupled, PlayerPower.unapply)
}

/*
class PlayerItems(tag: Tag) extends Table[PlayerItem](tag, "player_items"){
  def item = column[Long]("item")
  def player = column[Long]("player")

  def * = (player, item) <> (PlayerItem.tupled, PlayerItem.unapply)
}
*/

class QuestItems(tag: Tag) extends Table[QuestItem](tag, "quest_items"){
  def quest = column[Long]("quest")
  def item = column[Long]("item")

  def * = (quest, item) <> (QuestItem.tupled, QuestItem.unapply)
}

class QuestPowers(tag: Tag) extends Table[QuestPower](tag, "quest_powers"){
  def quest = column[Long]("quest")
  def power = column[Long]("power")

  def * = (quest, power) <> (QuestPower.tupled, QuestPower.unapply)
}

case class QuestDescription(id: Long, name: String, description: String, game: Long,
                            item1: Option[Item], item2: Option[Item], item3: Option[Item],
                            power1: Option[Power], power2: Option[Power], power3: Option[Power]){
  def this(id: Long, name: String, description: String, game: Long) = this(id, name, description, game, None, None, None, None, None, None)
  def items = Seq(item1, item2, item3).filter(_.nonEmpty).map(_.get)
  def powers = Seq(power1, power2, power3).filter(_.nonEmpty).map(_.get)
  def quest = Quest(id, name, description, game)
}

object QuestDescription {
  def applyIds(id: Long, name: String, description: String, game: Long,
                              item1: Option[Long], item2: Option[Long], item3: Option[Long],
                              power1: Option[Long], power2: Option[Long], power3: Option[Long]) = {
    new QuestDescription(id, name, description, game,
      item1.map(new Item(_)), item2.map(new Item(_)), item3.map(new Item(_)),
      power1.map(new Power(_)), power2.map(new Power(_)), power3.map(new Power(_)))
  }

  def unapplyIds(qd: QuestDescription): Option[(Long, String, String, Long, Option[Long], Option[Long], Option[Long], Option[Long], Option[Long], Option[Long])] = {
    Some(qd.id, qd.name, qd.description, qd.game,
      qd.item1.map(_.id), qd.item2.map(_.id), qd.item3.map(_.id),
      qd.power1.map(_.id), qd.power2.map(_.id), qd.power3.map(_.id))
  }

  def apply(quest: Quest, items: Seq[Item], powers: Seq[Power]) = {
    val item1 = if (items.size > 0) Some(items(0)) else None
    val item2 = if (items.size > 1) Some(items(1)) else None
    val item3 = if (items.size > 2) Some(items(2)) else None
    val power1 = if (powers.size > 0) Some(powers(0)) else None
    val power2 = if (powers.size > 1) Some(powers(1)) else None
    val power3 = if (powers.size > 2) Some(powers(2)) else None
    new QuestDescription(quest.id, quest.name, quest.description, quest.game, item1, item2, item3, power1, power2, power3)
  }
}
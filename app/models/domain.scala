package models

import java.sql.Date

import play.api.libs.json.Json.{JsValueWrapper, toJsFieldJsValueWrapper => js}
import play.api.libs.json.{JsValue, Json}
import services.Util
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

case class Game(id: Long, name: String, started: Boolean) extends Model {
  def this(id: Long) = this(id, "", false)
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "name").validate[String].get,
    (body \ "started").validate[Boolean].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("name" -> js(name)) +:
    ("started" -> js(started)) +:
    extras: _*
  )
}

case class ChatDetail(id: Long, game: Long, poster: Option[Long], posterName: String, chat: String)

object ChatDetail {
  def apply(chat: Chat, posterPlayer: Option[Player]) = {
    val name = posterPlayer match {
      case Some(player) => player.alias//s"${player.alias} (${player.name})"
      case _ => "System"
    }

    new ChatDetail(chat.id, chat.game, posterPlayer.map(_.id), name, chat.chat)
  }
}

case class Chat(id: Long, game: Long, poster: Option[Long], recipient: Option[Long], chat: String, created: Date) extends Model {
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "poster").validateOpt[Long].get,
    (body \ "recipient").validateOpt[Long].get,
    Util.jsSafe((body \ "chat").validate[String].get),
    new Date(new java.util.Date().getTime)
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("poster" -> js(poster)) +:
    ("recipient" -> js(recipient)) +:
    ("chat" -> js(chat)) +:
    ("created" -> js(created)) +:
    extras: _*
  )
}

case class Player(id: Long, game: Long, name: String, alias: String, score: Int) extends Model {
  def this(id: Long) = this(id, 0, "", "", 0)
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "name").validate[String].get,
    (body \ "alias").validate[String].get,
    0
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("name" -> js(name)) +:
    ("score" -> js(score)) +:
    extras: _*
  )
}

case class ItemNeeded(id: Long, game: Long, name: String, description: String, owner: Option[Long], found: Boolean, rumor1: Option[Long], rumor2: Option[Long]){
  def this(item: Item, found: Boolean, rumor1: Option[Long], rumor2: Option[Long]) = this(item.id, item.game, item.name, item.description, item.owner, found, rumor1, rumor2)
  def this(id: Long) = this(new Item(id), false, None, None)
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
    ("description" -> js(Util.jsSafe(description))) +:
    ("owner" -> js(owner)) +:
    extras: _*
  )
}

case class PowerNeeded(id: Long, game: Long, name: String, description: String, found: Boolean, rumor1: Option[Long], rumor2: Option[Long]) {
  def this(power: Power, found: Boolean, rumor1: Option[Long], rumor2: Option[Long]) = this(power.id, power.game, power.name, power.description, found, rumor1, rumor2)
  def this(id: Long) = this(new Power(id), false, None, None)
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
    ("description" -> js(description.replaceAll("\n", " "))) +:
    extras: _*
  )
}

object TradeStage {
  val Creating = 0
  val Offered = 1
  val Counteroffered = 2
  val Accepted = 3
  val Rejected = 4
}

object InviteStage {
  val Creating = 0
  val Offered = 1
  val Accepted = 2
  val Rejected = 3
}

case class Invite(id: Long, game: Long, inviter: Long, invitee: Long, quest: Long, stage: Int) extends Model {
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "inviter").validate[Long].get,
    (body \ "invitee").validate[Long].get,
    0,
    (body \ "stage").validate[Int].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
     ("id" -> js(id)) +:
     ("game" -> js(game)) +:
     ("inviter" -> js(inviter)) +:
     ("invitee" -> js(invitee)) +:
//     ("quest" -> js(quest)) +:
     ("stage" -> js(stage)) +:
     extras: _*
   )
}

case class Trade(id: Long, game: Long, offerer: Long, offeree: Long,
                 offererItem: Option[Long], offereeItem: Option[Long],
                 offererOther: Option[String], offereeOther: Option[String],
                 stage: Int) extends Model {
  def this(body: JsValue) = this(
    (body \ "id").validate[Long].getOrElse(-1L),
    (body \ "game").validate[Long].get,
    (body \ "offerer").validate[Long].get,
    (body \ "offeree").validate[Long].get,
    (body \ "offererItem").validateOpt[Long].get,
    (body \ "offereeItem").validateOpt[Long].get,
    (body \ "offererOther").validateOpt[String].get,
    (body \ "offereeOther").validateOpt[String].get,
    (body \ "stage").validate[Int].get
  )

  def toJson(extras: (String, JsValueWrapper)*) = Json.obj(
    ("id" -> js(id)) +:
    ("game" -> js(game)) +:
    ("offerer" -> js(offerer)) +:
    ("offeree" -> js(offeree)) +:
    ("offererItem" -> js(offererItem)) +:
    ("offereeItem" -> js(offereeItem)) +:
    ("offererOther" -> js(offererOther)) +:
    ("offereeOther" -> js(offereeOther)) +:
    ("stage" -> js(stage)) +:
    extras: _*
  )

  def accepted = stage == TradeStage.Accepted
  def counteroffered = stage == TradeStage.Counteroffered
  def offered = stage == TradeStage.Offered
  def rejected = stage == TradeStage.Rejected
}


case class Quest(id: Long, name: String, description: String, game: Long) extends Model {
  def this(id: Long) = this(id, "", "", 0)
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

case class PlayerQuest(player: Long, quest: Long, side: Boolean, completed: Boolean)
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

class Trades(tag: Tag) extends Table[Trade](tag, "trades") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def offerer = column[Long]("offerer")
  def offeree = column[Long]("offeree")
  def offererItem = column[Option[Long]]("offerer_item")
  def offereeItem = column[Option[Long]]("offeree_item")
  def offererOther = column[Option[String]]("offerer_other")
  def offereeOther = column[Option[String]]("offeree_other")
  def stage = column[Int]("stage")

  def * = (id, game, offerer, offeree, offererItem, offereeItem, offererOther, offereeOther, stage) <> (Trade.tupled, Trade.unapply)
}

class Invites(tag: Tag) extends Table[Invite](tag, "invites") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def offerer = column[Long]("inviter")
  def offeree = column[Long]("invitee")
  def quest = column[Long]("quest")
  def stage = column[Int]("stage")

  def * = (id, game, offerer, offeree, quest, stage) <> (Invite.tupled, Invite.unapply)
}

class Games(tag: Tag) extends Table[Game](tag, "games") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def name = column[String]("name")
  def started = column[Boolean]("started")

  def * = (id, name, started) <> (Game.tupled, Game.unapply)
//    def ? = (id.?, gameId.?, name.?).shaped.<>({ r => r._1.map(_ => Player.tupled((r._1.get, r._2.get, r._3.get))) }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))
}

class Players(tag: Tag) extends Table[Player](tag, "players") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def name = column[String]("name")
  def alias = column[String]("alias")
  def score = column[Int]("score")

  def * = (id, game, name, alias, score) <> (Player.tupled, Player.unapply)
//    def ? = (id.?, gameId.?, name.?).shaped.<>({ r => r._1.map(_ => Player.tupled((r._1.get, r._2.get, r._3.get))) }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))
}

class Chats(tag: Tag) extends Table[Chat](tag, "chats") {

  def id = column[Long]("id", O.AutoInc, O.PrimaryKey)
  def game = column[Long]("game")
  def poster = column[Option[Long]]("poster")
  def recipient = column[Option[Long]]("recipient")
  def chat = column[String]("chat")
  def created = column[Date]("created")

  def * = (id, game, poster, recipient, chat, created) <> (Chat.tupled, Chat.unapply)
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
  def side = column[Boolean]("side")
  def completed = column[Boolean]("completed")

  def * = (player, quest, side, completed) <> (PlayerQuest.tupled, PlayerQuest.unapply)
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

case class PlayerDescription(id: Long, game: Long, name: String, alias: String, score: Int, mainQuest: Option[Quest], sideQuest: Option[Quest],
                             item1: Option[Item], item2: Option[Item], item3: Option[Item], item4: Option[Item], item5: Option[Item],
                             item6: Option[Item], item7: Option[Item], item8: Option[Item], item9: Option[Item], item10: Option[Item],
                             power1: Option[Power], power2: Option[Power], power3: Option[Power], power4: Option[Power], power5: Option[Power]
                            ){
  def items = Seq(item1, item2, item3, item4, item5, item6, item7, item8, item9, item10).filter(_.nonEmpty).map(_.get)
  def powers = Seq(power1, power2, power3, power4, power5).filter(_.nonEmpty).map(_.get)
}

object PlayerDescription {
  def applyIds(id: Long, game: Long, name: String, alias: String, score: Int, mainQuest: Option[Long], sideQuest: Option[Long],
               item1: Option[Long], item2: Option[Long], item3: Option[Long], item4: Option[Long], item5: Option[Long],
               item6: Option[Long], item7: Option[Long], item8: Option[Long], item9: Option[Long], item10: Option[Long],
               power1: Option[Long], power2: Option[Long], power3: Option[Long], power4: Option[Long], power5: Option[Long]) = {
    new PlayerDescription(id, game, name, alias, score, mainQuest.map(new Quest(_)), sideQuest.map(new Quest(_)),
      item1.map(new Item(_)), item2.map(new Item(_)), item3.map(new Item(_)), item4.map(new Item(_)), item5.map(new Item(_)),
      item6.map(new Item(_)), item7.map(new Item(_)), item8.map(new Item(_)), item9.map(new Item(_)), item10.map(new Item(_)),
      power1.map(new Power(_)), power2.map(new Power(_)), power3.map(new Power(_)), power4.map(new Power(_)), power5.map(new Power(_))
    )
  }

  def unapplyIds(pd: PlayerDescription): Option[(Long, Long, String, String, Int, Option[Long], Option[Long],
    Option[Long], Option[Long], Option[Long], Option[Long], Option[Long],
    Option[Long], Option[Long], Option[Long], Option[Long], Option[Long],
    Option[Long], Option[Long], Option[Long], Option[Long], Option[Long])] = {
    Some(pd.id, pd.game, pd.name, pd.alias, pd.score, pd.mainQuest.map(_.id), pd.sideQuest.map(_.id),
      pd.item1.map(_.id), pd.item2.map(_.id), pd.item3.map(_.id), pd.item4.map(_.id), pd.item5.map(_.id),
      pd.item6.map(_.id), pd.item7.map(_.id), pd.item8.map(_.id), pd.item9.map(_.id), pd.item10.map(_.id),
      pd.power1.map(_.id), pd.power2.map(_.id), pd.power3.map(_.id), pd.power4.map(_.id), pd.power5.map(_.id)
    )
  }

  def apply(player: Player, mainQuest: Option[Quest], sideQuest: Option[Quest], items: Seq[Item], powers: Seq[Power]) = {
    def maybeItem(i: Int) = if (items.size > i) Some(items(i)) else None
    def maybePower(i: Int) = if (powers.size > i) Some(powers(i)) else None

    new PlayerDescription(player.id, player.game, player.name, player.alias, player.score, mainQuest, sideQuest,
      maybeItem(0), maybeItem(1), maybeItem(2), maybeItem(3), maybeItem(4), maybeItem(5), maybeItem(6), maybeItem(7), maybeItem(8), maybeItem(9),
      maybePower(0), maybePower(1), maybePower(2), maybePower(3), maybePower(4)
    )
  }
}

case class QuestDescription(id: Long, name: String, description: String, game: Long, master: Option[Long],
                            item1: Option[ItemNeeded], item2: Option[ItemNeeded], item3: Option[ItemNeeded],
                            power1: Option[PowerNeeded], power2: Option[PowerNeeded], power3: Option[PowerNeeded],
                            allies: Seq[Player]){
  def this(id: Long, name: String, description: String, game: Long, master: Option[Long]) = this(id, name, description, game, master, None, None, None, None, None, None, Nil)
  def items = Seq(item1, item2, item3).filter(_.nonEmpty).map(_.get)
  def powers = Seq(power1, power2, power3).filter(_.nonEmpty).map(_.get)
  def quest = Quest(id, name, description, game)
}

object QuestDescription {
  def applyIds(id: Long, name: String, description: String, game: Long, master: Option[Long],
                              item1: Option[Long], item2: Option[Long], item3: Option[Long],
                              power1: Option[Long], power2: Option[Long], power3: Option[Long]) = {
    new QuestDescription(id, name, description, game, master,
      item1.map(new ItemNeeded(_)), item2.map(new ItemNeeded(_)), item3.map(new ItemNeeded(_)),
      power1.map(new PowerNeeded(_)), power2.map(new PowerNeeded(_)), power3.map(new PowerNeeded(_)), Nil)
  }

  def unapplyIds(qd: QuestDescription): Option[(Long, String, String, Long, Option[Long], Option[Long], Option[Long], Option[Long], Option[Long], Option[Long], Option[Long])] = {
    Some(qd.id, qd.name, qd.description, qd.game, qd.master,
      qd.item1.map(_.id), qd.item2.map(_.id), qd.item3.map(_.id),
      qd.power1.map(_.id), qd.power2.map(_.id), qd.power3.map(_.id))
  }

  def apply(quest: Quest, master: Option[Player], items: Seq[ItemNeeded], powers: Seq[PowerNeeded], allies: Seq[Player]) = {
    def maybeItem(i: Int) = if (items.size > i) Some(items(i)) else None
    def maybePower(i: Int) = if (powers.size > i) Some(powers(i)) else None

    new QuestDescription(quest.id, quest.name, quest.description, quest.game, master.map(_.id),
      maybeItem(0), maybeItem(1), maybeItem(2),
      maybePower(0), maybePower(1), maybePower(2), allies)
  }
}
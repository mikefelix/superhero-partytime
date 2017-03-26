package controllers

import javax.inject.Inject

import models.{PartyDAO => dao, _}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue
import play.api.libs.json.Json.{toJsFieldJsValueWrapper => js}
import play.api.mvc._
import services.Auth

import scala.concurrent.Future

class ApplicationController @Inject()(auth: Auth) extends Controller {

  implicit def futurize[A](a: A): Future[A] = Future successful a

  def login = Action { implicit req =>
    Ok(s"""{"player":1}""")
  }

  def postGame = Action.async { implicit req =>
    req.body.asJson.asGame match {
      case None => BadRequest
      case Some(game) =>
        for {
          id <- dao.insertGame(game)
        } yield Created(id.toString)
    }
  }

  def getGame(id: Long) = Action.async { implicit req =>
    dao.findGameById(id).map {
      case Some(game) => Ok(game.toJson())
      case None => NotFound
    }
  }

  def deleteQuest(gameId: Long, id: Long) = Action.async { implicit req =>
    dao.deleteQuest(id).map { res => if (res > 0) NoContent else Gone }
  }

  def putQuest(gameId: Long, id: Long) = Action.async { implicit req =>
    req.body.asJson match {
      case None => BadRequest
      case Some(json) =>
        for {
          id <- dao.updateQuest(new Quest(json))
        } yield Created(id.toString)
    }
  }

  def postQuest(gameId: Long) = Action.async { implicit req =>
    req.body.asJson.asQuest match {
      case None => BadRequest
      case Some(quest) =>
        for {
          id <- dao.insertQuest(quest)
        } yield Created(id.toString)
    }
  }

/*
  def listQuests(gameId: Long) = Action.async { implicit req =>
    req.player match {
      case Some(playerId) =>
        dao.findQuestsByPlayer(playerId).map { quests =>
          Ok(quests.toJson)
        }
      case None =>
        dao.allQuests.map { quests =>
          Ok(quests.toJson)
        }
    }
  }
*/

  def listPlayers(gameId: Long) = Action.async { implicit req =>
    for (players <- dao.findPlayersByGame(gameId)) yield Ok(players.toJson)
  }

  def getQuest(gameId: Long, id: Long) = Action.async { implicit req =>
    val state = for {
      Some(quest) <- dao.findQuestById(id)
      items <- dao.findItemsByQuest(quest.id)
      players <- dao.findPlayersByQuest(quest.id)
      fulfilled <- dao.questIsFulfilled(quest.id)
    } yield (quest, items, fulfilled)

    state map {
      case (quest, items, fulfilled) =>
        Ok(quest.toJson("items" -> items.toJson, "fulfilled" -> js(fulfilled)))
    }
  }

  def listItemsForQuest(gameId: Long, questId: Long) = Action.async { implicit req =>
    for {
      items <- dao.findItemsByQuest(questId)
    } yield Ok(items.toJson)
  }

  def listItemsForPlayer(gameId: Long, playerId: Long) = Action.async { implicit req =>
    Some(playerId) match {//req.player match {
      case Some(id) if playerId == id =>
        for {
          items <- dao.findItemsByPlayer(playerId)
        } yield Ok(items.toJson)
      case _ => Unauthorized
    }
  }

  def getQuestForPlayer(gameId: Long, playerId: Long) = Action.async { implicit req =>
    Some(playerId) match {//req.player match {
      case Some(id) if playerId == id =>
        val quest = for {
          questOpt <- dao.findQuestDescByPlayer(playerId)
          quest <- questOpt
        } yield quest

        quest map {
          case Some(q) => Ok(q.toJson)
          case None => NotFound
        }
      case _ => Unauthorized
    }
  }

  def getSidequestForPlayer(gameId: Long, playerId: Long) = Action.async { implicit req =>
    Some(playerId) match {//req.player match {
      case Some(id) if playerId == id =>
        val quest = for {
          questOpt <- dao.findQuestDescByPlayer(playerId, side = true)
          quest <- questOpt
        } yield quest

        quest map {
          case Some(q) => Ok(q.toJson)
          case None => NotFound
        }
      case _ => Unauthorized
    }
  }

  def getItem(gameId: Long, questId: Long, id: Long) = Action.async { implicit req =>
    for {
      item <- dao.findItemById(id)
    } yield item match {
      case Some(item) => Ok(item.toJson())
      case None => NotFound
    }
  }

  def getItem(gameId: Long, id: Long) = Action.async { implicit req =>
    for {
      item <- dao.findItemById(id)
    } yield item match {
      case Some(item) => Ok(item.toJson())
      case None => NotFound
    }
  }

  def getItems(gameId: Long) = Action.async { implicit req =>
    for {
      item <- dao.findItemsByGame(gameId)
    } yield Ok(item.toJson)  //(views.html.quest(quest, items))
  }

  def getPower(gameId: Long, questId: Long, id: Long) = Action.async { implicit req =>
    dao.findPowerById(id) map {
      case Some(power) => Ok(power.toJson())
      case None => NotFound
    }
  }

  def getPower(gameId: Long, id: Long) = Action.async { implicit req =>
    dao.findPowerById(id) map {
      case Some(power) => Ok(power.toJson())
      case None => NotFound
    }
  }

  def getPowers(gameId: Long) = Action.async { implicit req =>
    for {
      power <- dao.findPowersByGame(gameId)
    } yield Ok(power.toJson)
  }

  def getPlayer(gameId: Long, id: Long) = Action.async { implicit req =>
    dao.findPlayerDescById(id) map {
      case Some(player) => Ok(player.toJson)
      case None => NotFound
    }
  }

  def postPlayer(gameId: Long) = Action.async { implicit req =>
    req.body.asJson.asPlayer match {
      case None => BadRequest
      case Some(player) => for (id <- dao.insertPlayer(player)) yield Created(id.toString)
    }
  }

  def postItem(gameId: Long) = Action.async { implicit req =>
    req.body.asJson.asItem match {
      case None => BadRequest
      case Some(item) => for (id <- dao.insertItem(item)) yield Created(id.toString)
    }
  }

  def postPower(gameId: Long) = Action.async { implicit req =>
    req.body.asJson.asPower match {
      case None => BadRequest
      case Some(power) => for (id <- dao.insertPower(power)) yield Created(id.toString)
    }
  }

  def putPlayer(gameId: Long, id: Long) = Action.async { implicit req =>
    req.body.asJson.asPlayer match {
      case None => BadRequest
      case Some(player) => for (id <- dao.updatePlayer(player)) yield Created(id.toString)
    }
  }

  def deletePlayer(gameId: Long, id: Long) = Action.async { implicit req =>
    dao.deletePlayer(id).map { res => if (res > 0) NoContent else Gone }
  }

  implicit class GetPlayerFromRequest(req: Request[AnyContent]) {
    def player: Option[Long] = req.headers.get("Authorization").map(_.toLong)
  }

  implicit class QuestsToJson(quests: Seq[Quest]) {
    def toJson: String = "[" + quests.map(_.toJson()).mkString(",") + "]"
  }

  implicit def questToString(quest: Quest): String =
    s"""{
        | "id":${quest.id},
        | "name":"${quest.name.replace("\"", "\\\"")}",
        | "description":"${quest.description.replace("\"", "\\\"")}"
        |}
      """.stripMargin

  implicit def questDescToString(quest: QuestDescription): String = {
    val items = quest.items.map { item =>
      s"""{ "name":"${item.name}",
         |  "description":"${item.description}",
         |  "id":${item.id}
         |}
       """.stripMargin
    }
    
    val powers = quest.powers.map { power =>
      s"""{ "name":"${power.name}",
         |  "description":"${power.description}",
         |  "id":${power.id}
         |}
       """.stripMargin
    }
    
    s"""{
        | "id":${quest.id},
        | "name":"${quest.name.replace("\"", "\\\"")}",
        | "description":"${quest.description.replace("\"", "\\\"")}",
        | "items":[${items.mkString(",")}],
        | "powers":[${powers.mkString(",")}]
        |}
      """.stripMargin
  }


  implicit class QuestDescriptionToJson(quest: QuestDescription) {
    def toJson: String = questDescToString(quest)
  }
 
  implicit def playerDescToString(player: PlayerDescription): String = {
    val items = player.items.map { item =>
      s"""{ "name":"${item.name}",
         |  "description":"${item.description}",
         |  "id":${item.id}
         |}
       """.stripMargin
    }
    
    val powers = player.powers.map { power =>
      s"""{ "name":"${power.name}",
         |  "description":"${power.description}",
         |  "id":${power.id}
         |}
       """.stripMargin
    }
    
    s"""{
        | "id":${player.id},
        | "name":"${player.name.replace("\"", "\\\"")}",
        | "alias":"${player.alias.replace("\"", "\\\"")}",
        | "items":[${items.mkString(",")}],
        | "powers":[${powers.mkString(",")}]
        |}
      """.stripMargin
  }


  implicit class PlayerDescriptionToJson(player: PlayerDescription) {
    def toJson: String = playerDescToString(player)
  }

  implicit class QuestAndItemsToJson(t: (Quest, Seq[Item])) {
    def toJson: String = {
      val (quest, items) = t
      s"""{
         | "id":${quest.id},
         | "game":${quest.game},
         | "name":"${quest.name.replace("\"", "\\\"")}",
         | "description":"${quest.description.replace("\"", "\\\"")}",
         | "items":${items.toJson}
         |}
       """.stripMargin
    }
  }

  implicit class QuestAndPlayersToJson(t: (Quest, Seq[Player])) {
    def toJson: String = {
      val (quest, players) = t
      s"""{
         | "id":${quest.id},
         | "game":${quest.game},
         | "name":"${quest.name.replace("\"", "\\\"")}",
         | "description":"${quest.description.replace("\"", "\\\"")}",
         | "players":${players.toJson}
         |}
       """.stripMargin
    }
  }


  implicit class ItemsToJson(items: Seq[Item]) {
    def toJson: String = "[" + items.map(_.toJson()).mkString(",") + "]"
  }

  implicit class PowersToJson(powers: Seq[Power]) {
    def toJson: String = "[" + powers.map(_.toJson()).mkString(",") + "]"
  }

  implicit class PlayersToJson(players: Seq[Player]) {
    def toJson: String = "[" + players.map(_.toJson()).mkString(",") + "]"
  }

  implicit class ModelFromBody(body: Option[JsValue]) {
    def asPlayer = body.map(new Player(_))
    def asItem = body.map(new Item(_))
    def asPower = body.map(new Power(_))
    def asQuest = body.map(new Quest(_))
    def asGame = body.map(new Game(_))
  }

}



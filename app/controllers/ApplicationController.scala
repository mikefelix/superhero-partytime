package controllers

import javax.inject.Inject

import models.PartyDAO._
import models.{PartyDAO => dao, _}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json.{toJsFieldJsValueWrapper => js}
import play.api.libs.json.{JsValue, Json}
import play.api.mvc._
import services.Auth
import services.Util.jsSafe

import scala.concurrent.Future

class ApplicationController @Inject()(auth: Auth) extends Controller {

  implicit def futurize[A](a: A): Future[A] = Future successful a
  implicit def optionize[A](a: A): Option[A] = Some(a)

  private val sillyExtraOffers = List("A drink", "A kiss", "A promise")
  private val pointsOffers = List(1, 5, 10, 20, 30).map(p => s"+$p" -> s"$p point${if (p != 1) "s" else ""}")

  private def withGame(gameId: Long)(block: Game => Future[Result])(implicit request: Request[_]) = {
    findStartedGameById(gameId) flatMap {
      case Some(game) => block(game)
      case None => Future successful NotFound("Game " + gameId)
    }
  }

/*
  def postGame = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asGame match {
      case None => BadRequest
      case Some(game) =>
        for {items
        } yield Created(id.toString)
    }
  }
*/

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
    req.body.asText.map(Json.parse) match {
      case None => BadRequest
      case Some(json) =>
        for {
          id <- dao.updateQuest(new Quest(json))
        } yield Created(id.toString)
    }
  }

  def postQuest(gameId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asQuest match {
      case None => BadRequest
      case Some(quest) =>
        for {
          id <- dao.insertQuest(quest)
        } yield Created(id.toString)
    }
  }

  def postChat(gameId: Long, playerId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asChat match {
      case None => BadRequest
      case Some(Chat(_, _, poster, recipient, chat, _)) =>
        for {
          id <- dao.addUserChat(gameId, poster.get, recipient, chat)
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
    for (players <- dao.findPlayerDescsByGame(gameId)) yield Ok(players.toJson)
  }


  def getOfferOptions(gameId: Long, playerId: Long) = Action.async { implicit req =>
    dao.findItemsByPlayer(playerId) flatMap { items =>
      val offers = items.map(item => item.id.toString -> s"Item: ${item.name}") ++
        pointsOffers ++
        sillyExtraOffers.map(o => o -> o)
      Ok(offers.toJson)
    }
  }

  def getLatestChats(gameId: Long) = Action.async { implicit req =>
    dao.findLatestChats(gameId, 100) map { chats =>
      Ok(chats.toJson)
    }
  }

  def getAlerts(gameId: Long, playerId: Long) = Action.async { implicit req =>
    dao.findAlerts(gameId, playerId) map { chats =>
      Ok(chats.toJson)
    }
  }

  def clearAlert(gameId: Long, id: Long) = Action.async { implicit req =>
    dao.clearAlert(id) map { res =>
      NoContent
    }
  }

  def listTradesForPlayer(gameId: Long, playerId: Long) = Action.async { implicit req =>
    for {
      offersGiven <- dao.findTradesByOfferer(gameId)
      offersReceived <- dao.findTradesByOfferer(gameId)
      offers = offersGiven ++ offersReceived
    } yield Ok(offers.toJson)
  }

  def getTrade(gameId: Long, playerId: Long, id: Long) = Action.async { implicit req =>
    dao.findTradeById(id) map {
      case Some(trade) => Ok(trade.toJson())
      case _ => NotFound
    }
  }

  def getInvite(gameId: Long, playerId: Long, id: Long) = Action.async { implicit req =>
    dao.findInviteById(id) map {
      case Some(invite) => Ok(invite.toJson())
      case _ => NotFound
    }
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
        dao.findCurrentQuestDescByPlayer(playerId, side = false) flatMap {
          case Some(quest) => Ok(quest.toJson)
          case None => NotFound
        }
      case _ => Unauthorized
    }
  }
  
  private def completionReward(quest: QuestDescription, side: Boolean = false) = {
    val itemsNeeded = quest.items.length
    val powersNeeded = quest.powers.length
    val itemsFound = quest.items.count(it => it.found)
    val powersFound = quest.powers.count(it => it.found)
    val missingItems = itemsNeeded - itemsFound
    val missingPowers = powersNeeded - powersFound
    val missingReqs = missingItems + missingPowers
    val reqsNeeded = itemsNeeded + powersNeeded
    val reqsFound = itemsFound + powersFound

    if (reqsFound == 0)
      -5
    else {
      val base = reqsFound * 10

      if (side)
        base
      else {
        if (reqsNeeded >= 5) {
          if (missingReqs == 0)
            base + 40
          else if (missingReqs == 1)
            base + 15
          else if (missingReqs == 2)
            base
          else
            0
        }
        else if (reqsNeeded == 4) {
          if (missingReqs == 0)
            base + 30
          else if (missingReqs == 1)
            base + 10
          else
            0
        }
        else {
          if (missingReqs == 0)
            base + 20
          else
            0
        }
      }
    }
  }

  def completeQuest(gameId: Long, playerId: Long) = Action.async { implicit req =>
    Some(playerId) match {//req.player match {
      case Some(id) if playerId == id =>
        dao.findCurrentQuestDescByPlayer(playerId, side = false) flatMap {
          case None => NotFound
          case Some(quest) =>
            val reward = completionReward(quest)
            val sidereward = completionReward(quest, side = true)
            dao.completeQuest(quest, reward, sidereward) flatMap { res =>
              dao.findQuestDescById(res) flatMap { case Some(newQuest) =>
                Ok(newQuest.toJson)
              }
            }
        }
      case _ => Unauthorized
    }
  }

  def leaveSidequest(gameId: Long, playerId: Long) = Action.async { implicit req =>
    Some(playerId) match {//req.player match {
      case Some(id) if playerId == id =>
        dao.findCurrentQuestDescByPlayer(playerId, side = true) flatMap {
          case None => NotFound
          case Some(quest) =>
            dao.leaveSidequest(playerId, quest.id) flatMap { res =>
              NoContent
            }
        }
      case _ => Unauthorized
    }
  }

  val NoQuest = QuestDescription(0, "None", "", 0, None, None, None, None, None, None, None, Nil)

  def getSidequestForPlayer(gameId: Long, playerId: Long) = Action.async { implicit req =>
    Some(playerId) match {//req.player match {
      case Some(id) if playerId == id =>
        val quest = for {
          questOpt <- dao.findCurrentQuestDescByPlayer(playerId, side = true)
          quest <- questOpt
        } yield quest

        quest map {
          case Some(q) => Ok(q.toJson)
          case None => Ok(NoQuest.toJson)
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
    req.body.asText.map(Json.parse).asPlayer match {
      case None => BadRequest
      case Some(postedPlayer) =>
        dao.findGameById(gameId) flatMap { case Some(game) =>
          try {
            dao.insertPlayer(postedPlayer) flatMap { player =>
              if (game.started) {
                println(s"Game is started. Giving powers and items to new player.")
                dao.assignItems(player, 2)
                dao.assignPowers(player, 2)
                dao.assignNewQuest(player.id)
              }

              dao.findPlayerDescById(player.id) map {
                case Some(dbPlayer) => Created(dbPlayer.toJson)
                case _ => BadRequest
              }
            }
          } catch {
            case e: Exception => Conflict
          }
        }
    }
  }

  sealed trait Transfer {
    val descriptionToGiver: String
    val descriptionToReceiver: String
  }

  class ItemTransfer(giver: Player, receiver: Player, item: Item) extends Transfer {
    override val descriptionToGiver = s"${receiver.alias} received your ${item.name}"
    override val descriptionToReceiver = s"you received their ${item.name}"
  }

  class OtherTransfer(giver: Player, receiver: Player, other: String) extends Transfer {
    override val descriptionToGiver = s"You will give $other to ${receiver.alias}"
    override val descriptionToReceiver = s"you will receive $other in return"
  }

  class PointsTransfer(giver: Player, receiver: Player, points: Int) extends Transfer {
    private def desc = s"$points point${if (points == 1) "" else "s"}"

    override val descriptionToGiver = s"You will give $desc to ${receiver.alias}"
    override val descriptionToReceiver = s"you will receive $desc in return"
  }

  private def transferItem(itemId: Long, giverId: Long, receiverId: Long): Future[Transfer] = {
    dao.findPlayerById(giverId) flatMap {
      case None =>
        throw new IllegalStateException(s"Invalid player for transfer: $giverId")
      case Some(giver) =>
        dao.findPlayerById(receiverId) flatMap {
          case None =>
            throw new IllegalStateException(s"Invalid player for transfer: $receiverId")
          case Some(receiver) =>
            dao.findItemById(itemId) map {
              case None =>
                throw new IllegalStateException(s"Invalid item for transfer: $itemId")
              case Some(item) =>
                dao.updateItem(item.copy(owner = Some(receiverId)))
                println(s"Transferring item ${item.id} (${item.name}) from player ${giver.id} to player ${receiver.id}")
                new ItemTransfer(giver, receiver, item)
            }
        }
    }
  }

  private def transferOther(giver: Player, receiver: Player, other: String): Future[Transfer] = {
    if (other.matches("\\+[0-9]+")){
      transferPoints(giver, receiver, Integer.parseInt(other))
    }
    else {
      Future successful new OtherTransfer(giver, receiver, other)
    }
  }

  private def transferPoints(giver: Player, receiver: Player, amount: Int): Future[Transfer] = {
    dao.transferPoints(amount, giver, receiver)
    new PointsTransfer(giver, receiver, amount)
  }

  private def tradeMessage(trade: Trade, playerId: Long, transferToOfferee: Transfer, transferToOfferer: Transfer): String = {
    val playerIsOfferer = trade.offerer == playerId

    if (playerIsOfferer){
      tradeJson(s"Trade negotiated. ${transferToOfferee.descriptionToGiver} and ${transferToOfferer.descriptionToReceiver}.")
    }
    else {
      tradeJson(s"Trade negotiated. ${transferToOfferer.descriptionToGiver} and ${transferToOfferee.descriptionToReceiver}.")
    }
  }

  def postInvite(gameId: Long, playerId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asInvite match {
      case None => BadRequest
      case Some(invite) => for (id <- dao.insertInvite(invite)) yield Ok(tradeJson("Invitation sent."))
    }
  }

  def putInvite(gameId: Long, playerId: Long, inviteId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asInvite match {
      case None => BadRequest
      case Some(invite) =>
        dao.updateInvite(invite) map { res =>
          Ok(tradeJson(res))
        }
    }
  }

  def postTrade(gameId: Long, playerId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asTrade match {
      case None => BadRequest
      case Some(trade) => for (id <- dao.insertTrade(trade)) yield Ok(tradeJson("Trade proposed."))
    }
  }

  private def tradeJson(msg: String) = s"""{"message":"${jsSafe(msg)}"}"""

  def putTrade(gameId: Long, playerId: Long, tradeId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asTrade match {
      case None => BadRequest
      case Some(trade) =>
        if (trade.rejected) {
          Ok(tradeJson("Trade cancelled."))
        }
        else if (!trade.accepted) {
          for (id <- dao.updateTrade(trade)) yield
            if (trade.counteroffered)
              Ok(tradeJson("Counteroffer sent."))
            else if (trade.offered)
              Ok(tradeJson("Offer sent."))
            else
              InternalServerError
        }
        else {
          // trade.accepted
          dao.findPlayerById(trade.offerer) flatMap {
            case None => throw new IllegalArgumentException(s"No such player ${trade.offerer}")
            case Some(offerer) =>
              dao.findPlayerById(trade.offeree) flatMap {
                case None => throw new IllegalArgumentException(s"No such player ${trade.offeree}")
                case Some(offeree) => processTrade(offerer, offeree, playerId, trade)
              }
          }
        }
    }
  }

  private def processTrade(offerer: Player, offeree: Player, playerId: Long, trade: Trade): Future[Result] = {
    (trade.offererItem, trade.offererOther, trade.offereeItem, trade.offereeOther) match {
      case (Some(offererItem), _, Some(offereeItem), _) =>
        for {
          transferToOfferee <- transferItem(offererItem, trade.offerer, trade.offeree)
          transferToOfferer <- transferItem(offereeItem, trade.offeree, trade.offerer)
          traded <- dao.updateTrade(trade)
        } yield Ok(tradeMessage(trade, playerId, transferToOfferee, transferToOfferer))

      case (None, Some(offererOther), Some(offereeItem), _) =>
        for {
          transferToOfferer <- transferItem(offereeItem, trade.offeree, trade.offerer)
          transferToOfferee <- transferOther(offerer, offeree, offererOther)
          traded <- dao.updateTrade(trade)
        } yield Ok(tradeMessage(trade, playerId, transferToOfferee, transferToOfferer))

      case (Some(offererItem), _, None, Some(offereeOther)) =>
        for {
          transferToOfferee <- transferItem(offererItem, trade.offerer, trade.offeree)
          transferToOfferer <- transferOther(offeree, offerer, offereeOther)
          traded <- dao.updateTrade(trade)
        } yield Ok(tradeMessage(trade, playerId, transferToOfferee, transferToOfferer))

      case (None, Some(offererOther), None, Some(offereeOther)) =>
        for {
          transferToOfferee <- transferOther(offerer, offeree, offererOther)
          transferToOfferer <- transferOther(offeree, offerer, offereeOther)
          traded <- dao.updateTrade(trade)
        } yield Ok(tradeMessage(trade, playerId, transferToOfferee, transferToOfferer))

      case _ => throw new IllegalStateException(s"Need one offer from each player. Got ${(trade.offererItem,
        trade.offererOther, trade.offereeItem, trade.offereeOther)}")
    }
  }

  def postItem(gameId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asItem match {
      case None => BadRequest
      case Some(item) => for (id <- dao.insertItem(item)) yield Created(id.toString)
    }
  }

  def postPower(gameId: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asPower match {
      case None => BadRequest
      case Some(power) => for (id <- dao.insertPower(power)) yield Created(id.toString)
    }
  }

  def putPlayer(gameId: Long, id: Long) = Action.async { implicit req =>
    req.body.asText.map(Json.parse).asPlayer match {
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
    s"""{"id":${quest.id},
        | "name":"${jsSafe(quest.name)}",
        | "description":"${jsSafe(quest.description)}"
        |} """.stripMargin

  implicit def questDescToString(quest: QuestDescription): String = {
    val items = quest.items.map { item =>
      s"""{"name":"${item.name}",
         |"description":"${jsSafe(item.description)}",
         |"id":${item.id},
         |"found":${item.found},
         |"rumors":[${Seq(item.rumor1, item.rumor2).filter(_.nonEmpty).map(_.get).mkString(",")}]
         |}""".stripMargin
    }

    val powers = quest.powers.map { power =>
      s"""{"name":"${power.name}",
         |"description":"${jsSafe(power.description)}",
         |"id":${power.id},
         |"found":${power.found},
         |"rumors":[${Seq(power.rumor1, power.rumor2).filter(_.nonEmpty).map(_.get).mkString(",")}]
         |}""".stripMargin
    }
    
    s"""{"id":${quest.id},
        |"name":"${jsSafe(quest.name)}",
        |"master":${quest.master.getOrElse("null")},
        |"description":"${jsSafe(quest.description)}",
        |"allies":[${quest.allies.map(_.id).mkString(",")}],
        |"items":[${items.mkString(",")}],
        |"powers":[${powers.mkString(",")}]
        |}""".stripMargin
  }


  implicit class QuestDescriptionToJson(quest: QuestDescription) {
    def toJson: String = questDescToString(quest)
  }

  implicit def chatDetailToString(chat: ChatDetail): String = {
    s"""{
       |"id":${chat.id},
       |"poster":${chat.poster.getOrElse(0)},
       |"posterName":"${chat.posterName}",
       |"chat":"${jsSafe(chat.chat)}"
       |}
     """.stripMargin
  }

  implicit def playerDescToString(player: PlayerDescription): String = {
    val items = player.items.map { item =>
      s"""{"name":"${item.name}",
         | "description":"${jsSafe(item.description)}",
         | "id":${item.id}
         |}""".stripMargin
    }

    val powers = player.powers.map { power =>
      s"""{"name":"${power.name}",
         | "description":"${jsSafe(power.description)}",
         | "id":${power.id}
         |}""".stripMargin
    }

    val mainQuest = player.mainQuest map questToString getOrElse "null"
    val sideQuest = player.sideQuest map questToString getOrElse "null"

    s"""{"id":${player.id},
        | "name":"${jsSafe(player.name)}",
        | "alias":"${jsSafe(player.alias)}",
        | "mainquest": $mainQuest,
        | "sidequest": $sideQuest,
        | "items":[${items.mkString(",")}],
        | "powers":[${powers.mkString(",")}],
        | "score":${player.score}
        |}""".stripMargin
  }



  implicit class ChatDetailToJson(chat: ChatDetail) {
    def toJson: String = chatDetailToString(chat)
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
         | "name":"${jsSafe(quest.name)}",
         | "description":"${jsSafe(quest.description)}",
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
         | "name":"${jsSafe(quest.name)}",
         | "description":"${jsSafe(quest.description)}",
         | "players":${players.toJson}
         |}
       """.stripMargin
    }
  }

  implicit class ChatsToJson(chats: Seq[Chat]) {
    def toJson: String = "[" + chats.map(_.toJson()).mkString(",") + "]"
  }

  implicit class ChatDetailsToJson(chats: Seq[ChatDetail]) {
    def toJson: String = "[" + chats.map(_.toJson).mkString(",") + "]"
  }

  implicit class TradesToJson(trades: Seq[Trade]) {
    def toJson: String = "[" + trades.map(_.toJson()).mkString(",") + "]"
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

  implicit class PlayerDescsToJson(players: Seq[PlayerDescription]) {
    def toJson: String = "[" + players.map(_.toJson).mkString(",") + "]"
  }

  implicit class OfferSeqToJson(offers: Seq[(String, String)]) {
    def toJson: String = "[" + offers.map(o => s"""{"key":"${jsSafe(o._1)}","value":"${jsSafe(o._2)}"}""").mkString(",") + "]"
  }

  implicit class ModelFromBody(body: Option[JsValue]) {
    private def get[M <: Model](body: Option[JsValue], func: (JsValue => M)) = {
      println(s"Try this body: $body")

      body map func
    }

    def asPlayer = get(body, new Player(_))
    def asItem = get(body, new Item(_))
    def asPower = get(body, new Power(_))
    def asTrade = get(body, new Trade(_))
    def asInvite = get(body, new Invite(_))
    def asQuest = get(body, new Quest(_))
    def asGame = get(body, new Game(_))
    def asChat = get(body, new Chat(_))
  }
}



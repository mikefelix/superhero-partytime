package models

import java.sql.Date

import play.api.Play.current
import play.api.db.DB
import slick.dbio.{DBIO => _, _}
import slick.driver.PostgresDriver.api._

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random.shuffle
import scala.util.{Failure, Success}

object PartyDAO {
  val db = Database.forDataSource(DB.getDataSource())

  val quests = TableQuery[Quests]
  val games = TableQuery[Games]
  val players = TableQuery[Players]
  val chats  = TableQuery[Chats]
  val items = TableQuery[Items]
  val powers = TableQuery[Powers]
  val trades = TableQuery[Trades]
  val invites = TableQuery[Invites]
  val playerQuests = TableQuery[PlayerQuests]
  val questItems = TableQuery[QuestItems]
  val questPowers = TableQuery[QuestPowers]
  val playerPowers = TableQuery[PlayerPowers]

  def resetGame(game: Game) = {
    // TODO: restrict everything to this game.

    db.run(DBIO.seq(
      items.map(_.owner).update(None),
      playerPowers.delete,
      playerQuests.delete,
      trades.filter(_.id === game.id).delete,
      invites.filter(_.id === game.id).delete,
      chats.filter(_.id === game.id).delete,
//      players.filter(_.id === game.id).delete,
      games.filter(_.id === game.id).map(_.started).update(false)
    ))
  }

  def initGame(game: Game) = {
    val elements = for {
      pl <- findPlayersByGame(game.id)
      it <- findItemsByGame(game.id)
      po <- findPowersByGame(game.id)
      qu <- findQuestDescsByGame(game.id)
    } yield (pl, it, po, qu)

    elements foreach {
      case (allPlayers, items, powers, quests) =>
        // Give 10 points
        db.run(players.filter(_.game === game.id).map(_.score).update(10))

        // Give each player the same number of powers, but at least 2
        val powerAssignments = distributePowers(powers, allPlayers, giveEachPlayerAtLeast = 3, giveEachPowerAtLeast = 2)

        // Give out all the items, giving each player at most 6 items
        val itemAssignments = distributeItems(items, allPlayers, maxPerPlayer = 6)

        giveQuests(quests, allPlayers, itemAssignments, powerAssignments)

      case _ => throw new IllegalStateException("Can't init game.")
    }

    db.run(games.filter(_.id === game.id).update(game.copy(started = true)))
  }

  private def distributePowers(powers: Seq[Power], players: Seq[Player], giveEachPlayerAtLeast: Int, giveEachPowerAtLeast: Int = 1) = {
    val powersIt = circularRandomIterator(powers)
    val powersToGive = powers.size * giveEachPowerAtLeast
    val giveToEach = math.max(giveEachPlayerAtLeast,
      (powersToGive / players.size) + (if (powersToGive % players.size == 0) 0 else 1))

    println(s"Giving each player $giveToEach powers.")

    val map = scala.collection.mutable.Map[Player, ListBuffer[Power]]()
    players.foreach { player =>
      map.put(player, new ListBuffer[Power]())
    }

    for (i <- 1 to giveToEach){
      val mixedPlayers = shuffle(players)
      for (j <- 1 to mixedPlayers.size) {
        var power: Power = Power(0, 0, "", "")
        var player: Option[Player] = None

        do {
          power = powersIt.next()
          player = mixedPlayers.find(p => map(p).size < i && !map(p).contains(power))
        /*
            .getOrElse(throw new IllegalStateException(
              s"""
                 |Could not find a player for power ${power.id}. $i of $giveToEach, $j of ${players.size}, ${powers.size} powers:
                 |${map.map(e => e._1.id + " -> " + e._2.map(_.id)).mkString("\n")}
               """.stripMargin))
*/
        } while (player.isEmpty)

        map(player.get) += power
      }
    }

    map.foreach { case (player, powersGiven) =>
      powersGiven.foreach { power =>
        println(s"Giving power ${power.name} to ${player.alias}")
        insertPlayerPower(player.id, power.id)
      }
    }

    map.toMap
  }

  private def distributeItems(items: Seq[Item], players: Seq[Player], maxPerPlayer: Int) = {
    // TODO: what if there are more players than items?
    val mixedItems = shuffle(items)
    val playersIt = circularRandomIterator(players)
    val itemsIt = items.iterator
    val maxGives = math.min(maxPerPlayer * players.size, items.size)

    val map = scala.collection.mutable.Map[Player, ListBuffer[Item]]()
    players.foreach { player =>
      map.put(player, new ListBuffer[Item]())
    }

    var max = 0
    for (i <- 1 to maxGives){
      val item = itemsIt.next()
      val player = playersIt.next()
      println(s"Giving item ${item.name} to ${player.alias}")
      map(player) += item
      max = if (max < map(player).size) map(player).size else max
      updateItem(item.copy(owner = Some(player.id)))
    }

    println(s"Gave players up to $max items.")
    map.toMap
  }

  /*
  private def distribute[I, P](toDistribute: Seq[I], distributeAmong: Seq[P])(give: (I, P) => Unit) = {
    val receivers = circularIterator(shuffle(distributeAmong))
    shuffle(toDistribute).foreach { item =>
      val receiver = receivers.next()
      give(item, receiver)
    }
  }
  */

  private def giveQuests(quests: Seq[QuestDescription], players: Seq[Player],
                         itemAssignments: Map[Player, Seq[Item]], powerAssignments: Map[Player, Seq[Power]]) = {
    val mixedQuests = shuffle(quests)
    val given = new ListBuffer[QuestDescription]()

    val ericasQuest = quests.find(_.name.toLowerCase().contains("ice penis")).getOrElse(quests.head)

    players.foreach { player =>
      val playerItems = itemAssignments(player).map(_.id)
      val playerPowers = powerAssignments(player).map(_.id)

      val quest = if (player.name.toLowerCase().startsWith("erica")){
        // Erica's special quest
        ericasQuest
      }
      else {
        // Quests for non-Erica people
        mixedQuests.find { q =>
          q != ericasQuest &&
          !given.contains(q) &&
          !q.items.exists(item => playerItems.contains(item.id)) &&
          !q.powers.exists(power => playerPowers.contains(power.id))
        } match {
          case Some(q) => q
          case None =>
            println(s"Failed to give a preferred quest to player $player.id")
            mixedQuests.head
        }
      }

      println(s"Giving quest ${quest.name} to ${player.alias}")
      insertPlayerQuest(player.id, quest.id, side = false)
      given += quest
    }

    given
  }

  private def circularRandomIterator[X](list: Seq[X]): Iterator[X] = {
    var it = shuffle(list).iterator

    new Iterator[X] {
      override def hasNext = seq.nonEmpty

      override def next() = if (it.hasNext) {
        it.next
      }
      else {
        it = shuffle(list).iterator
        it.next
      }

    }
  }

  def allQuests: Future[Seq[Quest]] = db.run(quests.result)
  def findQuestById(id: Long): Future[Option[Quest]] = db.run(questById(id).result.headOption)
  def findQuestsByGame(gameId: Long): Future[Seq[Quest]] = db.run(questsByGame(gameId).result)

  def findUnclaimedQuestIdsByGame(gameId: Long): Future[Seq[(Int)]] = {
    db.run(sql"""select q.id, count(pq.player)
          from quests q
          left join player_quests pq on (pq.quest = q.id)
          group by q.id
          having count(pq.player) = 0
      """.as[(Int, Int)]) map { f => f map { t => t._1 } }
  }

//  def findQuestsByPlayer(playerId: Long): Future[Seq[Quest]] = db.run(questsByPlayer(playerId).result)
  def insertQuest(quest: Quest): Future[Quest] = db.run(questInsert += quest)
  def insertQuest(questDesc: QuestDescription): Future[Long] = {
    insertQuest(questDesc.quest) map { quest =>

//      val questAdd = Seq(quests += Quest(quest.id, quest.name, quest.description, quest.game))
      val itemsAdd = Seq(questDesc.item1, questDesc.item2, questDesc.item3).filter(_.nonEmpty).map { q =>
        questItems += QuestItem(quest.id, q.get.id)
      }

      val powersAdd = Seq(questDesc.power1, questDesc.power2, questDesc.power3).filter(_.nonEmpty).map { q =>
        questPowers += QuestPower(quest.id, q.get.id)
      }

      db.run(DBIO.sequence(itemsAdd ++ powersAdd))
      quest.id
    }
  }

  def transferPoints(amount: Int, giver: Player, receiver: Player): Future[Unit] = {
    val newGiverScore = giver.score - amount
    val newReceiverScore = receiver.score + amount
    db.run(DBIO.seq(
      players.filter(_.id === giver.id).map(_.score).update(newGiverScore),
      players.filter(_.id === receiver.id).map(_.score).update(newReceiverScore)
    ))
  }

  def updateQuest(quest: Quest): Future[Int] = db.run(questById(quest.id).update(quest))
  def updateQuest(quest: QuestDescription): Future[List[Int]] = {
    val actions = DBIO.sequence(List(
      // Update the quest details
      quests.filter(_.id === quest.id).update(Quest(quest.id, quest.name, quest.description, quest.game)),

      // Remove all items and powers from the quest
      questItems.filter(_.quest === quest.id).delete,
      questPowers.filter(_.quest === quest.id).delete) ++

      // Add back the new items and powers
      List(quest.item1, quest.item2, quest.item3).filter(_.nonEmpty).map(q => questItems += QuestItem(quest.id, q.get.id)) ++
      List(quest.power1, quest.power2, quest.power3).filter(_.nonEmpty).map(p => questPowers += QuestPower(quest.id, p.get.id))
    )

    db run actions
  }

  def deleteQuest(id: Long): Future[Int] = db.run(questById(id).delete)

  def findPlayerById(id: Long): Future[Option[Player]] = db.run(playerById(id).result.headOption)
  def findPlayersByGame(gameId: Long): Future[Seq[Player]] = db.run(players.sortBy(_.alias).filter(_.game === gameId).result)

  def findPlayerDescById(id: Long): Future[Option[PlayerDescription]] = {
    (for {
      player <- findPlayerById(id)
      items <- findItemsByPlayer(id)
      powers <- findPowersByPlayer(id)
      mainQuest <- findQuestByPlayer(id, side = false)
      sideQuest <- findQuestByPlayer(id, side = true)
    } yield (player, items, powers, mainQuest, sideQuest)) map {
      case (playerOpt, itemsForPlayer, powersForPlayer, mainQuestForPlayer, sideQuestForPlayer) =>
        playerOpt map { player =>
          PlayerDescription(player.id, player.game, player.name, player.alias, player.score, mainQuestForPlayer, sideQuestForPlayer,
            itemsForPlayer.option(0), itemsForPlayer.option(1), itemsForPlayer.option(2), itemsForPlayer.option(3), itemsForPlayer.option(4),
            itemsForPlayer.option(5), itemsForPlayer.option(6), itemsForPlayer.option(7), itemsForPlayer.option(8), itemsForPlayer.option(9),
            powersForPlayer.option(0), powersForPlayer.option(1), powersForPlayer.option(2), powersForPlayer.option(3), powersForPlayer.option(4))
        }
    }
  }

  def findPlayerDescsByGame(gameId: Long): Future[Seq[PlayerDescription]] = {
    findPlayersByGame(gameId) flatMap { players =>
      val descs = players.map { player =>
        for {
          items <- findItemsByPlayer(player.id)
          powers <- findPowersByPlayer(player.id)
          mainQuest <- findQuestByPlayer(player.id, side = false)
          sideQuest <- findQuestByPlayer(player.id, side = true)
        } yield PlayerDescription(player, mainQuest, sideQuest, items, powers)
      }

      Future sequence descs
    }
  }

  def insertTrade(trade: Trade): Future[Int] = {
    val newTradeFuture: Future[Trade] = if (trade.offererItem.nonEmpty){
      findItemById(trade.offererItem.get) flatMap {
        case Some(offererItem) =>
          if (trade.offereeItem.nonEmpty){
            findItemById(trade.offereeItem.get) flatMap {
              case Some(offereeItem) =>
                // Insert trade with two items
                Future successful trade.copy(offererOther = Some(offererItem.name), offereeOther = Some(offereeItem.name))
            }
          }
          else {
            // Insert trade with item from offerer
            Future successful trade.copy(offererOther = Some(offererItem.name))
          }
      }
    }
    else if (trade.offereeItem.nonEmpty){
      findItemById(trade.offereeItem.get) map {
        case Some(offereeItem) =>
          // Insert trade with offeree item
          trade.copy(offereeOther = Some(offereeItem.name))
      }
    }
    else {
      // Insert trade with no items
      Future successful trade
    }


    newTradeFuture flatMap { newTrade =>
      val insertQuery = trades returning trades.map(_.id) into ((trade, id) => trade.copy(id = id))

      println(s"Insert trade: $newTrade")
      db.run(insertQuery += newTrade) flatMap { trade =>
        findPlayerById(trade.offerer) flatMap {
          case Some(offerer) =>
            println(s"Give an alert for trade ${trade.id} to ${trade.offeree}")
            addAlert(trade.game, trade.offeree, s"Trade request from ${offerer.alias} {${trade.id}}")
        }
      }
    }
  }

  def updateTrade(trade: Trade): Future[Int] = {
    db.run(tradeById(trade.id).update(trade))

    trade.stage match {
      case TradeStage.Counteroffered =>
        findPlayerById(trade.offeree) flatMap {
          case Some(offeree) => addAlert(trade.game, trade.offerer, s"Trade response from ${offeree.alias} {${trade.id}}")
        }
      case TradeStage.Accepted =>
        findPlayerById(trade.offerer) flatMap {
          case Some(offerer) => addAlert(trade.game, trade.offeree, s"Trade completed with ${offerer.alias} {${trade.id}}")
        }
      case TradeStage.Rejected =>
        findPlayerById(trade.offerer) flatMap {
          case Some(offerer) => addAlert(trade.game, trade.offeree, s"Trade rejected by ${offerer.alias} {${trade.id}}")
        }
      case _ =>
        throw new IllegalStateException("Didn't expect trade stage " + trade.stage + " here.")
    }
  }

  def insertInvite(invite: Invite): Future[Int] = {
    val insertQ = invites returning invites.map(_.id) into ((invite, id) => invite.copy(id = id))
    db.run(insertQ += invite) flatMap { inserted =>
      addAlert(invite.game, invite.invitee, s"Quest invitation received {${inserted.id}}")
    }
  }

  def updateInvite(invite: Invite): Future[String] = {
    db.run(invites.filter(_.id === invite.id).update(invite)) map { res =>
      if (invite.stage == InviteStage.Accepted){
        findQuestByPlayer(invite.inviter, side = false) flatMap { case Some(mainQuest) =>
          db.run(DBIO.seq(
            playerQuests.filter(q => q.player === invite.invitee && q.side === true).delete,
            playerQuests += PlayerQuest(invite.invitee, mainQuest.id, side = true, completed = false)
          ))

          addAlert(invite.game, invite.inviter, s"Quest invitation accepted {${invite.id}}")
        }

        "Invitation accepted."
      }
      else if (invite.stage == InviteStage.Rejected) {
        addAlert(invite.game, invite.inviter, s"Quest invitation rejected {${invite.id}}")
        "Invitation declined."
      }
      else
        "Invitation updated."
    }
  }

  def insertPlayer(player: Player): Future[Player] = {
    val insertQuery = players returning players.map(_.id) into ((player, id) => player.copy(id = id))
    db.run(insertQuery += player)
  }

  def deletePlayer(id: Long): Future[Int] = db.run(playerById(id).delete)
  def updatePlayer(player: Player): Future[Int] = {
    println(s"update player ${player.id}")
    val f = db.run(playerById(player.id).update(player))

    f.recover {
      case e: Exception => e.printStackTrace()
    }

    f.onSuccess { case s => println(s"success: $s")}
    f
  }

  def updatePlayerWithQuest(playerDesc: PlayerDescription): Future[Seq[Int]] = db.run {
    println(s"update to ${playerDesc.name}, ${playerDesc.alias}")
    val playerProj = for {player <- players if player.id === playerDesc.id} yield (player.name, player.alias)
    val updateAction = playerProj.update(playerDesc.name, playerDesc.alias)

    val myQuest = playerQuests.filter(_.player === playerDesc.id)
    val pqAction = if (playerDesc.mainQuest.nonEmpty) {
      myQuest.map(_.quest).update(playerDesc.mainQuest.get.id)
    }
    else {
      myQuest.delete
    }

    // Remove existing items and powers for player
    val removeItems = items.filter(_.owner === playerDesc.id).map(_.owner).update(None)
    val removePowers = playerPowers.filter(_.player === playerDesc.id).delete

    // Add back items and powers
    val addItems = List(playerDesc.item1, playerDesc.item2, playerDesc.item3, playerDesc.item4, playerDesc.item5,
        playerDesc.item6, playerDesc.item7, playerDesc.item8, playerDesc.item9, playerDesc.item10).filter(_.nonEmpty).map { item =>
      items.filter(_.id === item.get.id).map(_.owner).update(Some(playerDesc.id))
    }

    val addPowers = List(playerDesc.power1, playerDesc.power2, playerDesc.power3,
        playerDesc.power4, playerDesc.power5).filter(_.nonEmpty).map { power =>
      playerPowers += PlayerPower(playerDesc.id, power.get.id)
    }

    DBIO.sequence(Seq(updateAction, pqAction, removeItems, removePowers) ++ addItems ++ addPowers).asTry map {
      case Success(r) => r
      case Failure(r) => throw r
    }
  }

  def allGames: Future[Seq[Game]] = db.run(games.result)
  def insertGame(game: Game): Future[Int] = db.run(games += game)
  def updateGame(game: Game): Future[Int] = db.run(gameById(game.id).update(game))
  def findGameById(id: Long): Future[Option[Game]] = db.run(gameById(id).result.headOption)
  def findStartedGameById(id: Long): Future[Option[Game]] = db.run(gameById(id, started = true).result.headOption)

  def findTradeById(id: Long): Future[Option[Trade]] = {
    db.run(tradeById(id).result.headOption) flatMap {
      case None => Future successful None
      case Some(trade) =>
        val f = trade match {
          case Trade(id, game, offerer, offeree, Some(offererItemId), Some(offereeItemId), _, _, stage) =>
            for {
              offererItem <- findItemById(offererItemId)
              offereeItem <- findItemById(offereeItemId)
            } yield Trade(id, game, offerer, offeree, Some(offererItemId), Some(offereeItemId),
              offererItem.map(_.name), offereeItem.map(_.name), stage)

          case Trade(id, game, offerer, offeree, Some(offererItemId), None, _, offereeOther, stage) =>
            for {
              offererItem <- findItemById(offererItemId)
            } yield Trade(id, game, offerer, offeree, Some(offererItemId), None,
              offererItem.map(_.name), offereeOther, stage)

          case Trade(id, game, offerer, offeree, None, Some(offereeItemId), offererOther, _, stage) =>
            for {
              offereeItem <- findItemById(offereeItemId)
            } yield Trade(id, game, offerer, offeree, None, Some(offereeItemId),
              offererOther, offereeItem.map(_.name), stage)

          case Trade(id, game, offerer, offeree, None, None, offererOther, offereeOther, stage) =>
              Future successful Trade(id, game, offerer, offeree, None, None, offererOther, offereeOther, stage)
        }

        f map { Some(_) }
    }
  }

  def findTradesByOfferer(playerId: Long): Future[Seq[Trade]] = db.run(tradesByOfferer(playerId).result)
  def findTradesByOfferee(playerId: Long): Future[Seq[Trade]] = db.run(tradesByOfferee(playerId).result)

  def findInviteById(id: Long): Future[Option[Invite]] = db.run(inviteById(id).result.headOption)

  def findItemById(id: Long): Future[Option[Item]] = db.run(itemById(id).result.headOption)
  def findItemsByPlayer(playerId: Long): Future[Seq[Item]] = db.run(itemsByPlayer(playerId).result)
  def findItemsByQuest(questId: Long): Future[Seq[Item]] = db.run(itemsByQuest(questId).result)
  def findItemsByGame(gameId: Long): Future[Seq[Item]] = db.run(itemsByGame(gameId).result)
  def findItemsHeldByPlayers(questId: Long): Future[Seq[Item]] = db.run(itemsHeldByPlayers(questId).result)

  def findQuestByPlayer(playerId: Long, side: Boolean): Future[Option[Quest]] = db.run(currentQuestsByPlayer(playerId, side).result.headOption)
  def findPlayersByQuest(questId: Long): Future[Seq[Player]] = db.run(playersByQuest(questId).result)
  def findMasterForQuest(questId: Long): Future[Option[Player]] = db.run(playersByQuest(questId, main = Some(true)).result.headOption)
  def findAlliesForQuest(questId: Long): Future[Seq[Player]] = db.run(playersByQuest(questId, main = Some(false)).result)

  def findPowerById(id: Long): Future[Option[Power]] = db.run(powerById(id).result.headOption)
  def findPowersByPlayer(playerId: Long): Future[Seq[Power]] = db.run(powersByPlayer(playerId).result)
  def findPowersByQuest(questId: Long): Future[Seq[Power]] = db.run(powersByQuest(questId).result)
  def findPowersByGame(gameId: Long): Future[Seq[Power]] = db.run(powersByGame(gameId).result)
  def findPlayerPowersByGame(gameId: Long): Future[Seq[PlayerPower]] = db.run(playerPowersByGame(gameId).result)
  def findPowersHeldByPlayers(questId: Long): Future[Seq[PlayerPower]] = db.run(powersHeldByPlayers(questId).result)

  def findPowersWithNumPlayers(gameId: Long): Future[Seq[(Power, Int)]] = {
    val query = (powers joinLeft playerPowers on (_.id === _.power))
      .groupBy(_._1)
      .map { case (pow, p) =>
        pow -> p.map(_._2).length
      }

    db.run(query.result)
  }

  def findPlayerPowersByPlayer(playerId: Long): Future[Seq[PlayerPower]] = db.run(playerPowersByPlayer(playerId).result)
  def findPlayerPowersByPower(powerId: Long): Future[Seq[PlayerPower]] = db.run(playerPowersByPower(powerId).result)

  def insertItem(item: Item): Future[Int] = db.run(items += item)
  def updateItem(item: Item): Future[Int] = db.run(items.filter(_.id === item.id).update(item))
  def deleteItem(id: Long): Future[Int] = db.run(itemById(id).delete)

  def insertPower(power: Power): Future[Int] = db.run(powers += power)
  def updatePower(power: Power): Future[Int] = db.run(powers.filter(_.id === power.id).update(power))
  def deletePower(id: Long): Future[Int] = db.run(powerById(id).delete)

  def findLatestChats(gameId: Long, num: Int): Future[Seq[ChatDetail]] = {
    val q = for {
      chat <- chats.sortBy(_.id.asc) if chat.game === gameId && chat.poster.nonEmpty
      poster <- players if poster.id === chat.poster
    } yield (chat, poster)

    db.run(q.take(num).result) map { (seq: Seq[(Chat, Player)]) =>
      seq map { case (msg, poster) => ChatDetail(msg, Some(poster))}
    }
  }

  def clearAlert(id: Long) = {
    db.run(chats.filter(_.id === id).delete)
  }

  def findAlerts(gameId: Long, playerId: Long): Future[Seq[ChatDetail]] = {
    val q = for {
      chat <- chats.sortBy(_.id) if chat.game === gameId && chat.recipient === playerId && chat.poster.isEmpty
    } yield chat

    db.run(q.result) map { (seq: Seq[Chat]) =>
      seq map (msg => ChatDetail(msg, None))
    }
  }

  def addUserChat(gameId: Long, sender: Long, recipient: Option[Long], chat: String) = {
    db.run(chats += Chat(0L, gameId, Some(sender), recipient, chat, new Date(new java.util.Date().getTime)))
  }

  def addAlert(gameId: Long, recipient: Long, chat: String) = {
    db.run(chats += Chat(0L, gameId, None, Some(recipient), chat, new Date(new java.util.Date().getTime)))
  }

  private def addPoints(player: Player, points: Int) = {
    db.run(players.filter(_.id === player.id).map(_.score).update(player.score + points))
  }

  def leaveSidequest(playerId: Long, questId: Long) = {
    db.run(playerQuests.filter(pq => pq.player === playerId && pq.quest === questId && pq.side === true).delete)
  }

  def completeQuest(quest: QuestDescription, reward: Int, sidereward: Int): Future[Long] = {
    findPlayersByQuest(quest.id) flatMap { allies =>
      val newId = allies map { ally =>
        if (quest.master.contains(ally.id)) {
          addPoints(ally, reward)
          addAlert(quest.game, ally.id, s"Quest completed {${quest.id}/$reward}")
          assignRandomQuest(ally, except = quest.id)
        }
        else {
          addPoints(ally, sidereward)
          addAlert(quest.game, ally.id, s"Sidequest completed {${quest.id}/$sidereward}")
          Future successful 0L
        }
      }

      db.run(playerQuests.filter(_.quest === quest.id).delete)
      Future.sequence(newId) map (_.sum)
    }
  }

  private def assignRandomQuest(player: Player, except: Long): Future[Long] = {
    findUnclaimedQuestIdsByGame(player.game) map { ids =>
      val random = shuffle(ids)
      val quest = if (random.head == except)
        random.tail.head
      else random.head

      db.run(playerQuests += PlayerQuest(player.id, quest, side = false, completed = false))
      quest
    }
  }

  def findQuestDescById(id: Long): Future[Option[QuestDescription]] = {
    findQuestById(id) flatMap {
      case Some(quest) =>
        for {
          itemsNeeded <- findItemsByQuest(quest.id)
          powersNeeded <- findPowersByQuest(quest.id)
          itemsHeld <- findItemsHeldByPlayers(quest.id)
          powersHeld <- findPowersHeldByPlayers(quest.id)

          master <- findMasterForQuest(quest.id)
          players <- findPlayersByGame(quest.game)

          allItems <- findItemsByGame(quest.game)
          allPlayerPowers <- findPlayerPowersByGame(quest.game)

          itemsForQuest = getItemsNeeded(itemsNeeded, itemsHeld, allItems, players)
          powersForQuest = getPowersNeeded(powersNeeded, powersHeld, allPlayerPowers)

          allies <- findAlliesForQuest(id)

        } yield Some(QuestDescription(quest, master, itemsForQuest, powersForQuest, allies))

      case None => Future(None)
    }
  }

  def findCurrentQuestDescByPlayer(id: Long, side: Boolean = false): Future[Option[QuestDescription]] = {
    def describeQuest(quest: Quest) = for {
      itemsNeeded <- findItemsByQuest(quest.id)
      powersNeeded <- findPowersByQuest(quest.id)
      itemsHeld <- findItemsHeldByPlayers(quest.id)
      powersHeld <- findPowersHeldByPlayers(quest.id)

      master <- findMasterForQuest(quest.id)
      players <- findPlayersByGame(quest.game)

      allItems <- findItemsByGame(quest.game)
      allPlayerPowers <- findPlayerPowersByGame(quest.game)

      itemsForQuest = getItemsNeeded(itemsNeeded, itemsHeld, allItems, players)
      powersForQuest = getPowersNeeded(powersNeeded, powersHeld, allPlayerPowers)

      allies <- findAlliesForQuest(quest.id)

    } yield Some(QuestDescription(quest, master, itemsForQuest, powersForQuest, allies)): Option[QuestDescription]

    findQuestByPlayer(id, side) flatMap {
      case Some(quest) => describeQuest(quest)
      case None => Future successful None
    }
  }

  def findQuestDescsByGame(gameId: Long): Future[Seq[QuestDescription]] = {
    findQuestsByGame(gameId) flatMap { quests =>
      Future.sequence {
        for {
          quest <- quests
        } yield for {
          itemsNeeded <- findItemsByQuest(quest.id)
          powersNeeded <- findPowersByQuest(quest.id)

          itemsHeld <- findItemsHeldByPlayers(quest.id)
          powersHeld <- findPowersHeldByPlayers(quest.id)

          master <- findMasterForQuest(quest.id)
          players <- findPlayersByGame(gameId)

          allItems <- findItemsByGame(gameId)
          allPlayerPowers <- findPlayerPowersByGame(gameId)

          itemsForQuest = getItemsNeeded(itemsNeeded, itemsHeld, allItems, players)
          powersForQuest = getPowersNeeded(powersNeeded, powersHeld, allPlayerPowers)

          allies <- findAlliesForQuest(quest.id)

        } yield QuestDescription(quest, master, itemsForQuest, powersForQuest, allies)
      }
    }
  }

  def assignNewQuest(playerId: Long): Future[Quest] = {
    def getRandomQuest(list: Seq[(Int, Int)]) = {
      val rand = (math.random * list.size).toInt
      val (id, _) = list(rand)
      findQuestById(id) map {
        case None => throw new IllegalStateException(s"Can't select quest $id")
        case Some(quest) =>
          println(s"Giving player $playerId quest ${quest.id}")
          db.run(playerQuests += PlayerQuest(playerId, quest.id, side = false, completed = false))
          quest
      }
    }

    val questsUncompletedByAnyone = sql"""select quests.id, count(player_quests.player)
          from quests
          left join player_quests on (player_quests.quest = quests.id)
          group by quests.id
          having count(player_quests.player) = 0
      """.as[(Int, Int)]

    db.run(questsUncompletedByAnyone) flatMap { list =>
      if (list.nonEmpty){
        getRandomQuest(list)
      }
      else {
        val questsUncompletedByPlayer = sql"""select quests.id, count(player_quests.player)
              from quests
              left join player_quests on (player_quests.quest = quests.id)
              where player_quests.player = $playerId
              group by quests.id
              having count(player_quests.player) = 0
          """.as[(Int, Int)]

        db.run(questsUncompletedByPlayer) flatMap { list =>
          getRandomQuest(list)
        }
      }
    }
  }

  def assignItems(player: Player, number: Int): Unit = {
    def giveRandomItem(list: Seq[Item]) = {
      val rand = (math.random * list.size).toInt
      val item = list(rand)
      println(s"Giving player ${player.id} item ${item.id}")
      db.run(items.filter(_.id === item.id).map(_.owner).update(Some(player.id)))
    }

    for (i <- 1 to number){
      val itemsUnownedByAnyone = items.filter(_.owner.isEmpty).result

      db.run(itemsUnownedByAnyone) map { list =>
        println(s"There are ${list.size} unowned items.")
        if (list.nonEmpty){
          giveRandomItem(list)
        }
      }
    }
  }

  def insertPlayerPower(player: Long, power: Long) = db.run(playerPowers += PlayerPower(player, power)) recover {
    case t: Throwable => t.printStackTrace()
  }

  def insertPlayerQuest(player: Long, quest: Long, side: Boolean) = db.run(playerQuests += PlayerQuest(player, quest, side, completed = false)) recover {
    case t: Throwable => t.printStackTrace()
  }

  def assignPowers(player: Player, number: Int): Unit = {
    def giveRandomPower(list: Seq[(Long, Long)]) = {
      val rand = (math.random * list.size).toInt
      val (id, _) = list(rand)
      println(s"Giving player ${player.id} power $id")
      insertPlayerPower(player.id, id)
    }

    val powersUnownedByAnyone = sql"""select powers.id, count(player_powers.player)
          from powers
          left join player_powers on (player_powers.power = powers.id)
          group by powers.id
          having count(player_powers.player) = 0
      """.as[(Long, Long)]

    val powersUnownedByPlayer = sql"""select powers.id, count(player_powers.player)
        from powers
        left join player_powers on (player_powers.power = powers.id)
        where player_powers.player = ${player.id}
        group by powers.id
        having count(player_powers.player) = 0
      """.as[(Long, Long)]

    for (i <- 1 to number){
      db.run(powersUnownedByAnyone) map { list =>
        if (list.nonEmpty){
          println(s"There are ${list.size} unowned powers.")
          giveRandomPower(list)
        }
        else {
          db.run(powersUnownedByPlayer) map { list =>
            println(s"There are ${list.size} unowned powers.")
            giveRandomPower(list)
          }
        }
      }
    }
  }

  private def getItemsNeeded(items: Seq[Item], allyItems: Seq[Item], allItems: Seq[Item], allPlayers: Seq[Player]): Seq[ItemNeeded] = {
    def findRumors(forItem: Item, num: Int): (Player, Player) = {
      val possessor = allPlayers.find(i => forItem.owner.contains(i.id)).get
      var fake: Player = null
      do {
        fake = allPlayers((allPlayers.size * math.random).toInt)
      } while (fake == possessor)

      if (math.random < 0.5)
        (possessor, fake)
      else
        (fake, possessor)
    }

    items map { item =>
      if (allyItems.contains(item))
        new ItemNeeded(item, true, None, None)
      else {
        val rumors = findRumors(item, 2)
        new ItemNeeded(item, false, Some(rumors._1.id), Some(rumors._2.id))
      }
    }
  }

  private def getPowersNeeded(powers: Seq[Power], allyPowers: Seq[PlayerPower], allPlayerPowers: Seq[PlayerPower]): Seq[PowerNeeded] = {
    def findRumors(forPower: Power, num: Int) = {
      val possessor = allPlayerPowers.find(p => p.power == forPower.id).get
      var fake: PlayerPower = null

       do {
         fake = allPlayerPowers((allPlayerPowers.size * math.random).toInt)
       } while (fake == possessor)

      if (math.random < 0.5)
        (possessor, fake)
      else
        (fake, possessor)
    }

    powers map { power =>
      if (allyPowers.exists(_.power == power.id))
        new PowerNeeded(power, true, None, None)
      else {
        val rumors = findRumors(power, 2)
        new PowerNeeded(power, false, Some(rumors._1.player), Some(rumors._2.player))
      }
    }
  }

  def test: Future[Quest] = for {
    Some(quest) <- findQuestById(1)
  } yield quest

  implicit class GetOptionally[A](seq: Seq[A]){
    def option(idx: Int) = {
      if (seq.size < idx + 1)
        None
      else
        Some(seq(idx))
    }  
  }
  
  private def questById(id: Long) = quests.sortBy(_.name).filter(_.id === id)
  private def playerById(id: Long) = players.filter(_.id === id)
  private def gameById(id: Long, started: Boolean = false) = if (started)
    games.filter(g => g.id === id && g.started === true)
  else
    games.filter(_.id === id)

  private def inviteById(id: Long) = invites.filter(_.id === id)
  private def itemById(id: Long) = items.sortBy(_.name).filter(_.id === id)
  private def powerById(id: Long) = powers.sortBy(_.name).filter(_.id === id)

  private def tradeById(id: Long) = trades.filter(_.id === id)

  private def questInsert = quests returning quests.map(_.id) into ((quest, id) => quest.copy(id = id))

  private def currentQuestsByPlayer(playerId: Long, side: Boolean) = for {
    playerQuest <- playerQuests if playerQuest.player === playerId && playerQuest.side === side && playerQuest.completed === false
    quest <- quests if quest.id === playerQuest.quest
  } yield quest

  private def questsByGame(gameId: Long) = for {
    quest <- quests.sortBy(_.name) if quest.game === gameId
  } yield quest

  private def itemsByPlayer(playerId: Long) = items.filter(_.owner === playerId)

  private def tradesByOfferer(playerId: Long) = trades.filter(t => t.offerer === playerId && t.stage < TradeStage.Accepted)
  private def tradesByOfferee(playerId: Long) = trades.filter(t => t.offeree === playerId && t.stage < TradeStage.Accepted)

  private def itemsByQuest(questId: Long) = for {
    questItem <- questItems if questItem.quest === questId
    item <- items if item.id === questItem.item
  } yield item

  private def powersByPlayer(playerId: Long) = for {
    playerPower <- playerPowers if playerPower.player === playerId
    power <- powers if power.id === playerPower.power
  } yield power

  private def playerPowersByPlayer(playerId: Long) = for {
    playerPower <- playerPowers if playerPower.player === playerId
  } yield playerPower

  private def playerPowersByPower(powerId: Long) = for {
    playerPower <- playerPowers if playerPower.power === powerId
  } yield playerPower

  private def powersByQuest(questId: Long) = for {
    questPower <- questPowers if questPower.quest === questId
    power <- powers if power.id === questPower.power
  } yield power

  private def playersByQuest(questId: Long, main: Option[Boolean] = None) = main match {
    case Some(true) =>
      for {
        playerQuest <- playerQuests if playerQuest.quest === questId && playerQuest.side === false
        player <- players if player.id === playerQuest.player
      } yield player
    case Some(false) =>
      for {
        playerQuest <- playerQuests if playerQuest.quest === questId && playerQuest.side === true
        player <- players if player.id === playerQuest.player
      } yield player
    case None =>
      for {
        playerQuest <- playerQuests if playerQuest.quest === questId
        player <- players if player.id === playerQuest.player
      } yield player
  }

  private def itemsHeldByPlayers(questId: Long) = for {
    playerQuest <- playerQuests if playerQuest.quest === questId
    heldItem <- items if playerQuest.player === heldItem.owner
  } yield heldItem

  private def powersHeldByPlayers(questId: Long) = for {
    playerQuest <- playerQuests if playerQuest.quest === questId
    playerPower <- playerPowers if playerPower.player === playerQuest.player
  } yield playerPower

  private def itemsForQuest(questId: Long) = questItems.filter { (item) => item.quest === questId }

  def questIsFulfilled(questId: Long): Future[Boolean] = {
    def difference[A](seq1: Seq[A], seq2: Seq[A]) = Set(seq1: _*) -- Set(seq2: _*)

    for {
      itemsP <- db.run(itemsHeldByPlayers(questId).result)
      itemsQ <- db.run(itemsForQuest(questId).result)
    } yield difference(itemsP.map(_.id), itemsQ.map(_.quest)).isEmpty
  }

  private def itemsByGame(gameId: Long) = items.sortBy(_.name).filter(_.game === gameId)

  private def powersByGame(gameId: Long) = powers.sortBy(_.name).filter(_.game === gameId)
  private def playerPowersByGame(gameId: Long) = playerPowers // TODO: .filter(_.game === gameId)


}
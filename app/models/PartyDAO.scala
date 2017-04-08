package models

import java.sql.Date

import play.api.Play.current
import play.api.db.DB
import slick.dbio.{DBIO => _, _}
import slick.driver.PostgresDriver.api._

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
      trades.delete,
      chats.delete
//      players.delete
    ))
  }

  def initGame(game: Game) = {
    val gameId = game.id
    val elements = for {
      pl <- findPlayersByGame(gameId)
      it <- findItemsByGame(gameId)
      po <- findPowersByGame(gameId)
      qu <- findQuestsByGame(gameId)
    } yield (pl, it, po, qu)

    elements foreach {
      case (players, items, powers, quests) =>
        // Give out all the items
        distributeEvenly(items, players) { (item, player) =>
          println(s"Giving item ${item.name} to ${player.alias}")
          updateItem(item.copy(owner = Some(player.id)))
        }

        // Give everyone powers, making sure all powers are given twice
        distributeEvenly(powers, players, 2) { (power, player) =>
          println(s"Giving power ${power.name} to ${player.alias}")
          insertPlayerPower(player.id, power.id)
        }

        distributeEvenly(players, quests) { (player, quest) =>
          // TODO: prevent people from getting the quest that needs the item or power they already have
          println(s"Giving quest ${quest.name} to ${player.alias}")
          insertPlayerQuest(player.id, quest.id, side = false)
        }

      case _ => throw new IllegalStateException("Can't init game.")
    }

    db.run(games.filter(_.id === game.id).update(game.copy(started = true)))
  }

/*
  private def giveEach[I, P](toDistribute: Seq[I], distributeAmong: Seq[P], eachGets: Int)(give: (I, P) => Unit) = {
    val items = circularIterator(toDistribute)
    for (i <- 1 to eachGets){
      val receivers = shuffle(distributeAmong)
      receivers.foreach { receiver =>
        val item = items.next()
        give(item, receiver)
      }
    }
  }
*/

  private def distributeEvenly[I, P](toDistribute: Seq[I], distributeAmong: Seq[P], copies: Int = 1)(give: (I, P) => Unit) = {
    val receivers = circularIterator(shuffle(distributeAmong))
    shuffle(toDistribute).foreach { item =>
      for (i <- 1 to copies){
        val receiver = receivers.next()
        give(item, receiver)
      }
    }
  }

  private def circularIterator[X](iterable: Iterable[X]): Iterator[X] = {
    var it = iterable.iterator
    new Iterator[X] {
      override def hasNext = iterable.nonEmpty

      override def next() = if (it.hasNext)
        it.next
      else {
        it = iterable.iterator
        it.next
      }

    }
  }

  def allQuests: Future[Seq[Quest]] = db.run(quests.result)
  def findQuestById(id: Long): Future[Option[Quest]] = db.run(questById(id).result.headOption)
  def findQuestsByGame(gameId: Long): Future[Seq[Quest]] = db.run(questsByGame(gameId).result)
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
  def findPlayersByGame(gameId: Long): Future[Seq[Player]] = db.run(players.filter(_.game === gameId).result)

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
          PlayerDescription(player.id, player.game, player.name, player.alias, mainQuestForPlayer, sideQuestForPlayer,
            itemsForPlayer.option(0), itemsForPlayer.option(1), itemsForPlayer.option(2), itemsForPlayer.option(3), itemsForPlayer.option(4),
            powersForPlayer.option(0), powersForPlayer.option(1), powersForPlayer.option(2))
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
    println(s"Insert trade ${trade.toJson()}")
    if (trade.offererItem.nonEmpty){
      findItemById(trade.offererItem.get) flatMap {
        case None => throw new IllegalStateException()
        case Some(offererItem) =>
          if (trade.offereeItem.nonEmpty){
            findItemById(trade.offereeItem.get) flatMap {
              case None => throw new IllegalStateException()
              case Some(offereeItem) =>
                // Insert trade with two items
                db.run(trades += trade.copy(offererOther = Some(offererItem.name), offereeOther = Some(offereeItem.name)))
            }
          }
          else {
            // Insert trade with item from offerer
            db.run(trades += trade.copy(offererOther = Some(offererItem.name)))
          }
      }
    }
    else if (trade.offereeItem.nonEmpty){
      findItemById(trade.offereeItem.get) flatMap {
        case None => throw new IllegalStateException()
        case Some(offereeItem) =>
          // Insert trade with offeree item
          db.run(trades += trade.copy(offereeOther = Some(offereeItem.name)))
      }
    }
    else {
      // Insert trade with no items
      db.run(trades += trade)
    }

    findPlayerById(trade.offerer) flatMap {
      case Some(offerer) => addSystemChat(trade.game, trade.offeree, s"Trade request from ${offerer.alias}.")
    }
  }

  def updateTrade(trade: Trade): Future[Int] = {
    db.run(tradeById(trade.id).update(trade))

    trade.stage match {
      case TradeStage.Counteroffered =>
        findPlayerById(trade.offeree) flatMap {
          case Some(offeree) => addSystemChat(trade.game, trade.offerer, s"Trade response from ${offeree.alias}.")
        }
      case TradeStage.Accepted =>
        findPlayerById(trade.offerer) flatMap {
          case Some(offerer) => addSystemChat(trade.game, trade.offeree, s"Trade completed with ${offerer.alias}.")
        }
      case TradeStage.Rejected =>
        findPlayerById(trade.offerer) flatMap {
          case Some(offerer) => addSystemChat(trade.game, trade.offeree, s"Trade rejected by ${offerer.alias}.")
        }
      case _ =>
        throw new IllegalStateException("Didn't expect trade stage " + trade.stage + " here.")
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
    val addItems = List(playerDesc.item1, playerDesc.item2, playerDesc.item3, playerDesc.item4, playerDesc.item5).filter(_.nonEmpty).map { item =>
      items.filter(_.id === item.get.id).map(_.owner).update(Some(playerDesc.id))
    }

    val addPowers = List(playerDesc.power1, playerDesc.power2, playerDesc.power3).filter(_.nonEmpty).map { power =>
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

  def findTradeById(id: Long): Future[Option[Trade]] = db.run(tradeById(id).result.headOption)
  def findTradesByOfferer(playerId: Long): Future[Seq[Trade]] = db.run(tradesByOfferer(playerId).result)
  def findTradesByOfferee(playerId: Long): Future[Seq[Trade]] = db.run(tradesByOfferee(playerId).result)

  def findItemById(id: Long): Future[Option[Item]] = db.run(itemById(id).result.headOption)
  def findItemsByPlayer(playerId: Long): Future[Seq[Item]] = db.run(itemsByPlayer(playerId).result)
  def findItemsByQuest(questId: Long): Future[Seq[Item]] = db.run(itemsByQuest(questId).result)
  def findItemsByGame(gameId: Long): Future[Seq[Item]] = db.run(itemsByGame(gameId).result)
  def findItemsHeldByPlayers(questId: Long): Future[Seq[Item]] = db.run(itemsHeldByPlayers(questId).result)

  def findQuestByPlayer(playerId: Long, side: Boolean): Future[Option[Quest]] = db.run(currentQuestsByPlayer(playerId, side).result.headOption)
  def findPlayersByQuest(questId: Long): Future[Seq[Player]] = db.run(playersByQuest(questId).result)
  def findMasterForQuest(questId: Long): Future[Option[Player]] = db.run(playersByQuest(questId, main = Some(true)).result.headOption)
  def findAllyForQuest(questId: Long): Future[Option[Player]] = db.run(playersByQuest(questId, main = Some(false)).result.headOption)

  def findPowerById(id: Long): Future[Option[Power]] = db.run(powerById(id).result.headOption)
  def findPowersByPlayer(playerId: Long): Future[Seq[Power]] = db.run(powersByPlayer(playerId).result)
  def findPowersByQuest(questId: Long): Future[Seq[Power]] = db.run(powersByQuest(questId).result)
  def findPowersByGame(gameId: Long): Future[Seq[Power]] = db.run(powersByGame(gameId).result)
  def findPowersHeldByPlayers(questId: Long): Future[Seq[PlayerPower]] = db.run(powersHeldByPlayers(questId).result)

  def findPlayerPowersByPlayer(playerId: Long): Future[Seq[PlayerPower]] = db.run(playerPowersByPlayer(playerId).result)
  def findPlayerPowersByPower(powerId: Long): Future[Seq[PlayerPower]] = db.run(playerPowersByPower(powerId).result)

  def insertItem(item: Item): Future[Int] = db.run(items += item)
  def updateItem(item: Item): Future[Int] = db.run(items.filter(_.id === item.id).update(item))
  def deleteItem(id: Long): Future[Int] = db.run(itemById(id).delete)

  def insertPower(power: Power): Future[Int] = db.run(powers += power)
  def updatePower(power: Power): Future[Int] = db.run(powers.filter(_.id === power.id).update(power))
  def deletePower(id: Long): Future[Int] = db.run(powerById(id).delete)

  def findLatestChats(gameId: Long, playerId: Long, num: Int): Future[Seq[ChatDetail]] = {
    val q = for {
      chat <- chats.sortBy(_.id) if chat.game === gameId && chat.poster.nonEmpty
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

  def addSystemChat(gameId: Long, recipient: Long, chat: String) = {
    db.run(chats += Chat(0L, gameId, None, Some(recipient), chat, new Date(new java.util.Date().getTime)))
  }

  def findQuestDescById(id: Long): Future[Option[QuestDescription]] = {
    findQuestById(id) flatMap {
      case Some(quest) =>
        for {
          itemsNeeded <- findItemsByQuest(quest.id)
          powersNeeded <- findPowersByQuest(quest.id)
          itemsHeld <- findItemsHeldByPlayers(quest.id)
          powersHeld <- findPowersHeldByPlayers(quest.id)
          Some(master) <- findMasterForQuest(quest.id)
          itemsForQuest = getItemsNeeded(itemsNeeded, itemsHeld)
          powersForQuest = getPowersNeeded(powersNeeded, powersHeld)
        } yield Some(QuestDescription(quest, master, itemsForQuest, powersForQuest))

      case None => Future(None)
    }
  }

  def findCurrentQuestDescByPlayer(id: Long, side: Boolean = false): Future[Option[QuestDescription]] = {
    def describeQuest(quest: Quest) = for {
      itemsNeeded <- findItemsByQuest(quest.id)
      powersNeeded <- findPowersByQuest(quest.id)
      itemsHeld <- findItemsHeldByPlayers(quest.id)
      powersHeld <- findPowersHeldByPlayers(quest.id)
      Some(master) <- findMasterForQuest(quest.id)
      itemsForQuest = getItemsNeeded(itemsNeeded, itemsHeld)
      powersForQuest = getPowersNeeded(powersNeeded, powersHeld)
    } yield Some(QuestDescription(quest, master, itemsForQuest, powersForQuest)): Option[QuestDescription]

    findQuestByPlayer(id, side) flatMap {
      case Some(quest) => describeQuest(quest)
      case None => Future successful None
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
    def giveRandomPower(list: Seq[(Int, Int)]) = {
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
      """.as[(Int, Int)]

    val powersUnownedByPlayer = sql"""select powers.id, count(player_powers.player)
        from powers
        left join player_powers on (player_powers.power = powers.id)
        where player_powers.player = ${player.id}
        group by powers.id
        having count(player_powers.player) = 0
      """.as[(Int, Int)]

    for (i <- 1 to number){
      db.run(powersUnownedByAnyone) map { list =>
        if (list.nonEmpty){
          giveRandomPower(list)
        }
        else {
          db.run(powersUnownedByPlayer) map { list =>
            giveRandomPower(list)
          }
        }
      }
    }
  }

  def findQuestDescsByGameId(gameId: Long): Future[Seq[QuestDescription]] = {
    findQuestsByGame(gameId) flatMap { quests =>
      Future.sequence {
        for {
          quest <- quests
        } yield for {
          itemsNeeded <- findItemsByQuest(quest.id)
          powersNeeded <- findPowersByQuest(quest.id)
          itemsHeld <- findItemsHeldByPlayers(quest.id)
          powersHeld <- findPowersHeldByPlayers(quest.id)
          Some(master) <- findMasterForQuest(quest.id)
          itemsForQuest = getItemsNeeded(itemsNeeded, itemsHeld)
          powersForQuest = getPowersNeeded(powersNeeded, powersHeld)
        } yield QuestDescription(quest, master, itemsForQuest, powersForQuest)
      }
    }
  }

  private def getItemsNeeded(items: Seq[Item], allyItems: Seq[Item]): Seq[ItemNeeded] = {
    items map { item =>
      new ItemNeeded(item, allyItems.contains(item))
    }
  }

  private def getPowersNeeded(powers: Seq[Power], allyPowers: Seq[PlayerPower]): Seq[PowerNeeded] = {
    powers map { power =>
      new PowerNeeded(power, allyPowers.exists(_.power == power.id))
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


}
package models

import play.api.Play.current
import play.api.db.DB
import slick.driver.PostgresDriver.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object PartyDAO {
  val db = Database.forDataSource(DB.getDataSource())

  val quests = TableQuery[Quests]
  val games = TableQuery[Games]
  val players = TableQuery[Players]
  val items = TableQuery[Items]
  val powers = TableQuery[Powers]
  val playerQuests = TableQuery[PlayerQuests]
  val questItems = TableQuery[QuestItems]
  val questPowers = TableQuery[QuestPowers]
  val playerPowers = TableQuery[PlayerPowers]

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

  def insertPlayer(player: Player): Future[Int] = db.run(players += player)
  def deletePlayer(id: Long): Future[Int] = db.run(playerById(id).delete)
  def updatePlayer(player: Player): Future[Int] = db.run(playerById(player.id).update(player))
  def updatePlayerWithQuest(playerDesc: PlayerDescription) = db.run {
    val playerProj = for {player <- players if player.id === playerDesc.id} yield player.name
    val updateAction = playerProj.update(playerDesc.name)

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

    DBIO.sequence(Seq(updateAction, pqAction, removeItems, removePowers) ++ addItems ++ addPowers)
  }

  def allGames: Future[Seq[Game]] = db.run(games.result)
  def insertGame(game: Game): Future[Int] = db.run(games += game)
  def updateGame(game: Game): Future[Int] = db.run(gameById(game.id).update(game))
  def findGameById(id: Long): Future[Option[Game]] = db.run(gameById(id).result.headOption)

  def findItemById(id: Long): Future[Option[Item]] = db.run(itemById(id).result.headOption)
  def findItemsByPlayer(playerId: Long): Future[Seq[Item]] = db.run(itemsByPlayer(playerId).result)
  def findItemsByQuest(questId: Long): Future[Seq[Item]] = db.run(itemsByQuest(questId).result)
  def findItemsByGame(gameId: Long): Future[Seq[Item]] = db.run(itemsByGame(gameId).result)

  def findQuestByPlayer(playerId: Long, side: Boolean): Future[Option[Quest]] = db.run(questsByPlayer(playerId, side).result.headOption)
  def findPlayersByQuest(questId: Long): Future[Seq[Player]] = db.run(playersByQuest(questId).result)

  def findPowerById(id: Long): Future[Option[Power]] = db.run(powerById(id).result.headOption)
  def findPowersByPlayer(playerId: Long): Future[Seq[Power]] = db.run(powersByPlayer(playerId).result)
  def findPowersByQuest(questId: Long): Future[Seq[Power]] = db.run(powersByQuest(questId).result)
  def findPowersByGame(gameId: Long): Future[Seq[Power]] = db.run(powersByGame(gameId).result)

  def insertItem(item: Item): Future[Int] = db.run(items += item)
  def updateItem(item: Item): Future[Int] = db.run(items.filter(_.id === item.id).update(item))
  def deleteItem(id: Long): Future[Int] = db.run(itemById(id).delete)

  def insertPower(power: Power): Future[Int] = db.run(powers += power)
  def updatePower(power: Power): Future[Int] = db.run(powers.filter(_.id === power.id).update(power))
  def deletePower(id: Long): Future[Int] = db.run(powerById(id).delete)

  def findQuestDescById(id: Long): Future[Option[QuestDescription]] = {
    (for {
      quest <- findQuestById(id)
      items <- findItemsByQuest(id)
      powers <- findPowersByQuest(id)
    } yield (quest, items, powers)) map {
      case (questOpt, itemsForQuest, powersForQuest) =>
        questOpt map { quest =>
          QuestDescription(quest.id, quest.name, quest.description, quest.game,
            itemsForQuest.option(0), itemsForQuest.option(1), itemsForQuest.option(2),
            powersForQuest.option(0), powersForQuest.option(1), powersForQuest.option(2))
        }
    }
  }

  def findQuestDescByPlayer(id: Long, side: Boolean = false): Future[Option[QuestDescription]] = {
    (for {
      quest <- findQuestByPlayer(id, side)
      items <- findItemsByQuest(id)
      powers <- findPowersByQuest(id)
    } yield (quest, items, powers)) map {
      case (questOpt, itemsForQuest, powersForQuest) =>
        questOpt map { quest =>
          QuestDescription(quest.id, quest.name, quest.description, quest.game,
            itemsForQuest.option(0), itemsForQuest.option(1), itemsForQuest.option(2),
            powersForQuest.option(0), powersForQuest.option(1), powersForQuest.option(2))
        }
    }
  }

  def findQuestDescsByGameId(gameId: Long): Future[Seq[QuestDescription]] = {
    findQuestsByGame(gameId) flatMap { quests =>
      Future.sequence {
        for {
          quest <- quests
        } yield for {
          items <- findItemsByQuest(quest.id)
          powers <- findPowersByQuest(quest.id)
        } yield QuestDescription(quest, items, powers)
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
  private def gameById(id: Long) = games.filter(_.id === id)
  private def itemById(id: Long) = items.sortBy(_.name).filter(_.id === id)
  private def powerById(id: Long) = powers.sortBy(_.name).filter(_.id === id)

  private def questInsert = quests returning quests.map(_.id) into ((quest, id) => quest.copy(id = id))

  private def questsByPlayer(playerId: Long, side: Boolean) = for {
    playerQuest <- playerQuests if playerQuest.player === playerId && playerQuest.side === side
    quest <- quests if quest.id === playerQuest.quest
  } yield quest

  private def questsByGame(gameId: Long) = for {
    quest <- quests.sortBy(_.name) if quest.game === gameId
  } yield quest

  private def itemsByPlayer(playerId: Long) = items.filter(_.owner === playerId)

  private def itemsByQuest(questId: Long) = for {
    questItem <- questItems if questItem.quest === questId
    item <- items if item.id === questItem.item
  } yield item

  private def powersByPlayer(playerId: Long) = for {
    playerPower <- playerPowers if playerPower.player === playerId
    power <- powers if power.id === playerPower.power
  } yield power

  private def powersByQuest(questId: Long) = for {
    questPower <- questPowers if questPower.quest === questId
    power <- powers if power.id === questPower.power
  } yield power

  private def playersByQuest(questId: Long) = for {
    playerQuest <- playerQuests if playerQuest.quest === questId
    player <- players if player.id === playerQuest.player
  } yield player

  private def itemsHeldByPlayers(questId: Long) = for {
    playerQuest <- playerQuests if playerQuest.quest === questId
    heldItem <- items if playerQuest.player === heldItem.owner
  } yield heldItem

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
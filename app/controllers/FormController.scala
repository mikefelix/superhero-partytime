package controllers

import javax.inject._

import models.PartyDAO._
import models.{PartyDAO => dao, _}
import play.api.Configuration
import play.api.data.Forms._
import play.api.data._
import play.api.i18n.{I18nSupport, MessagesApi}
import play.api.mvc._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Login(login: String, password: String)

class FormController @Inject()(implicit configuration: Configuration, val messagesApi: MessagesApi) extends Controller with I18nSupport {
  implicit def convertResultFuture[A](res: A): Future[A] = Future.successful(res)

  val loginForm = Form(
    mapping(
      "login" -> text.verifying(_ == "a"),
      "password" -> text.verifying(_ == "b")
    )(Login.apply)(Login.unapply)
  )

  val gameForm = Form(
    mapping(
      "id" -> ignored(0L),
      "name" -> text,
      "started" -> boolean
    )(Game.apply)(Game.unapply)
  )

  val playerForm = Form(
    mapping(
      "id" -> ignored(0L),
      "game" -> ignored(0L),
      "name" -> text,
      "alias" -> text,
      "score" -> ignored(0),
      "mainQuest" -> optional(longNumber),
      "sideQuest" -> optional(longNumber),
      "item1" -> optional(longNumber),
      "item2" -> optional(longNumber),
      "item3" -> optional(longNumber),
      "item4" -> optional(longNumber),
      "item5" -> optional(longNumber),
      "item6" -> optional(longNumber),
      "item7" -> optional(longNumber),
      "item8" -> optional(longNumber),
      "item9" -> optional(longNumber),
      "item10" -> optional(longNumber),
      "power1" -> optional(longNumber),
      "power2" -> optional(longNumber),
      "power3" -> optional(longNumber),
      "power4" -> optional(longNumber),
      "power5" -> optional(longNumber)
    )(PlayerDescription.applyIds)(PlayerDescription.unapplyIds)
  )

  val powerForm = Form(
    mapping(
      "id" -> ignored(0L),
      "game" -> ignored(0L),
      "name" -> text,
      "description" -> text
    )(Power.apply)(Power.unapply)
  )

  val itemForm = Form(
    mapping(
      "id" -> ignored(0L),
      "game" -> ignored(0L),
      "name" -> text,
      "description" -> text,
      "owner" -> optional(longNumber)
    )(Item.apply)(Item.unapply)
  )

  val questForm = Form(
    mapping(
      "id" -> ignored(0L),
      "name" -> text,
      "description" -> text,
      "game" -> ignored(0L),
      "master" -> optional(longNumber),
      "item1" -> optional(longNumber),
      "item2" -> optional(longNumber),
      "item3" -> optional(longNumber),
      "power1" -> optional(longNumber),
      "power2" -> optional(longNumber),
      "power3" -> optional(longNumber)
    )(QuestDescription.applyIds)(QuestDescription.unapplyIds)
  )

  def init(gameId: Long) = Action.async { implicit req =>
    withGame(gameId) { game =>
      if (game.started)
        Redirect(routes.FormController.showGameForm(gameId)).flashing("message" -> "Game was already started; could not start.")
      else {
        initGame(game)
        Redirect(routes.FormController.showGameForm(gameId)).flashing("message" -> "Game was started.")
      }
    }
  }

  def reset(gameId: Long) = Action.async { implicit req =>
    withGame(gameId) { game =>
      resetGame(game)
      Redirect(routes.FormController.showGameForm(gameId)).flashing("message" -> "Game was reset.")
    }
  }

  def fullReset(gameId: Long) = Action.async { implicit req =>
    withGame(gameId) { game =>
      resetGame(game, withPlayers = true)
      Redirect(routes.FormController.showGameForm(gameId)).flashing("message" -> "Game was reset.")
    }
  }

  def login = Action {
    Ok(views.html.login(loginForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => {
        println(s"Error authenticating user with data ${formWithErrors.data}:\n${formWithErrors.errors.map(err => err.key + ": " + err.message).mkString("/")}")
        BadRequest(views.html.login(formWithErrors))
      },
      userData => {
        println(s"Authenticated user ${userData.login} with headers ${request.headers}")
        Redirect(routes.FormController.listGames()).withNewSession.withSession("login" -> userData.login)
      }
    )
  }

  def listQuests(gameId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      findQuestDescsByGame(game.id) flatMap { quests =>
        Ok(views.html.quests(quests, game))
      }
    }
  }

  def listPlayers(gameId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      findPlayerDescsByGame(game.id) flatMap { players =>
        Ok(views.html.players(players, game))
      }
    }
  }

  def showQuestForm(gameId: Long, questId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      val quest = if (questId < 1)
        Future(Some(new QuestDescription(questId, "", "", game.id, None)))
      else
        findQuestDescById(questId)

      quest flatMap {
        case Some(q) =>
          findItemsByGame(gameId) flatMap { items =>
            findPowersByGame(gameId) flatMap { powers =>
              Ok(views.html.quest(questId, game,
                items.map( item => item.id.toString -> s"${item.name} (${item.description})" ),
                powers.map( power => power.id.toString -> s"${power.name} (${power.description})" ),
                questForm.fill(q)))
            }
          }
        case None => Redirect(routes.FormController.listQuests(gameId))
      }
    }
  }

  def showPlayerForm(gameId: Long, playerId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      val playerById = if (playerId < 1)
        Future(Some(PlayerDescription(playerId, game.id, "", "", 0,
          None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None, None)))
      else
        findPlayerDescById(playerId)

      playerById flatMap {
        case None => NotFound
        case Some(player) =>
          findQuestsByGame(game.id) flatMap { allQuests =>
            findItemsByGame(game.id) flatMap { allItems =>
              findPowersByGame(game.id) flatMap { allPowers =>
                val form = playerForm.fill(player)
                val questSelect = allQuests.map( q => q.id.toString -> s"${q.name}" )
                val itemSelect = allItems.map( i => i.id.toString -> s"${i.name}" )
                val powerSelect = allPowers.map( p => p.id.toString -> s"${p.name}" )
                Ok(views.html.player(playerId, form, game, itemSelect, powerSelect, questSelect))
              }
            }
          }
      }
    }
  }

  def updatePlayer(gameId: Long, playerId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      playerForm.bindFromRequest.fold(
        formWithErrors => {
          Redirect(routes.FormController.showPlayerForm(gameId, playerId))
            .flashing("error" -> formWithErrors.errors.map(e => e.key + ":" + e.message).mkString(", "))
        },
        player => {
//          dao.updatePlayer(Player(player.id, gameId, player.name, player.alias)) map { u =>
          dao.updatePlayerWithQuest(player.copy(id = playerId)) map { u =>
            Redirect(routes.FormController.showPlayerForm(gameId, playerId))
          }
        }
      )
    }
  }

  def updateQuest(gameId: Long, questId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      questForm.bindFromRequest.fold(
        formWithErrors => {
          findItemsByGame(gameId) flatMap { items =>
            findPowersByGame(gameId) flatMap { powers =>
              Future successful BadRequest(views.html.quest(questId, game,
                items.map(item => (item.id.toString, item.name)),
                powers.map(power => (power.id.toString, power.name)),
                formWithErrors))
            }
          }
        },
        quest => {
          if (questId < 1)
            dao.insertQuest(quest.copy(game = gameId)) map { res =>
              // TODO: return the right ID from the DAO
              Redirect(routes.FormController.listQuests(gameId))
            }
          else
            dao.updateQuest(quest.copy(id = questId, game = gameId)) map { res =>
             // Redirect(routes.FormController.showQuestForm(gameId, questId))
              Redirect(routes.FormController.listQuests(gameId))
            }
        }
      )
    }
  }

  def listGames = Action.async { implicit request =>
    withAuth {
      allGames map { games =>
        Ok(views.html.games(games))
      }
    }
  }

  def listPowers(gameId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      dao.findPowersWithNumPlayers(gameId) map { powers =>
        Ok(views.html.powers(powers, game))
      }
    }
  }

  def listItems(gameId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      dao.findItemsByGame(gameId) map { items =>
        Ok(views.html.items(items, game))
      }
    }
  }


  def showItemForm(gameId: Long, id: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      val item = if (id < 1)
        Future(Some(Item(0L, gameId, "", "", None)))
      else
        findItemById(id)

      item flatMap {
        case Some(i) =>
          Ok(views.html.item(id, itemForm.fill(i), game))
        case None => Redirect(routes.FormController.listItems(gameId))
      }
    }
  }

  def showPowerForm(gameId: Long, id: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      val power = if (id < 1)
        Future(Some(Power(0L, gameId, "", "")))
      else
        findPowerById(id)

      power flatMap {
        case Some(i) =>
          Ok(views.html.power(id, powerForm.fill(i), game))
        case None => Redirect(routes.FormController.listPowers(gameId))
      }
    }
  }

  def updateItem(gameId: Long, id: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      itemForm.bindFromRequest.fold(
        formWithErrors => {
          println(s"$formWithErrors")
          Future successful BadRequest(views.html.item(id, formWithErrors, game))
        },
        item => {
          if (id < 1)
            dao.insertItem(item.copy(game = gameId)) map { res =>
              // TODO: return the right ID from the DAO
              Redirect(routes.FormController.listItems(gameId))
            }
          else
            dao.updateItem(item.copy(id = id, game = gameId)) map { res =>
              Redirect(routes.FormController.listItems(gameId))
            }
        }
      )
    }
  }

  def updatePower(gameId: Long, id: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      powerForm.bindFromRequest.fold(
        formWithErrors => {
          println(s"$formWithErrors")
          Future successful BadRequest(views.html.power(id, formWithErrors, game))
        },
        power => {
          if (id < 1)
            dao.insertPower(power.copy(game = gameId)) map { res =>
              // TODO: return the right ID from the DAO
              Redirect(routes.FormController.listPowers(gameId))
            }
          else
            dao.updatePower(power.copy(id = id, game = gameId)) map { res =>
              Redirect(routes.FormController.listPowers(gameId))
            }
        }
      )
    }
  }

  
/*
  def insertItem(gameId: Long) = Action.async { implicit request =>
    println(s"Insert item for $gameId")
    withGame(gameId) { game =>
      itemForm.bindFromRequest.fold(
        formWithErrors => {
          println(s"$formWithErrors")
          Future successful BadRequest(views.html.item(0L, formWithErrors, game))
        },
        item => {
          dao.insertItem(item.copy(game = gameId)) map { res =>
            // TODO: return the right ID from the DAO
            Redirect(routes.FormController.listItems(gameId))
          }
        }
      )
    }
  }

  def insertPower(gameId: Long) = Action.async { implicit request =>
    println(s"Insert power for $gameId")
    withGame(gameId) { game =>
      powerForm.bindFromRequest.fold(
        formWithErrors => {
          println(s"$formWithErrors")
          Future successful BadRequest(views.html.power(0L, formWithErrors, game))
        },
        power => {
          dao.insertPower(power.copy(game = gameId)) map { res =>
            // TODO: return the right ID from the DAO
            Redirect(routes.FormController.listPowers(gameId))
          }
        }
      )
    }
  }
*/

  def showGameForm(gameId: Long) = Action.async { implicit request =>
    withGame(gameId) { game =>
      Ok(views.html.game(gameId, gameForm.fill(game)))
    }
  }

  def updateGame(gameId: Long) = Action.async { implicit request =>
    withAuth {
      gameForm.bindFromRequest.fold(
        formWithErrors => {
          println(s"$formWithErrors")
          Future successful BadRequest(views.html.game(gameId, formWithErrors))
//            .flashing("error" -> formWithErrors.errors.map(_.message).mkString(", "))
        },
        game => {
          println(s"Save this game: ${game.name}")
          dao.updateGame(game.copy(id = gameId)) map { res =>
            Redirect(routes.FormController.showGameForm(gameId))
          }
        }
      )
    }
  }

  private def withAuth(block: => Future[Result])(implicit request: Request[_]): Future[Result] = {
    request.session.get("login") match {
      case None => Future successful Redirect(routes.FormController.login())
      case Some(_) => block
    }
  }
  
  
  private def withGame(gameId: Long)(block: Game => Future[Result])(implicit request: Request[_]) = {
    withAuth {
      findGameById(gameId) flatMap {
        case Some(game) => block(game)
        case None => Future successful NotFound("Game " + gameId)
      }
    }
  }
  
}

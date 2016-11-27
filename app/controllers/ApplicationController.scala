package controllers

import javax.inject.Inject

import models._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.{Action, AnyContent, Controller, Request}
import services.Auth

import scala.concurrent.Future

class ApplicationController @Inject()(auth: Auth, questRepo: QuestRepo, itemRepo: ItemRepo)
                           extends Controller {

  implicit def futurize[A](a: A): Future[A] = Future successful a

  def postQuest = Action.async { implicit req =>
    req.body.asJson match {
      case None => BadRequest
      case Some(json) =>
        for {
          id <- questRepo.create(Quest(json))
        } yield Created(id)
    }
  }

  def listQuests = Action.async { implicit req =>
    req.player match {
      case Some(x) =>
        questRepo.all.map { quests =>
          Ok(quests.toJson)
        }
      case None =>
        questRepo.all.map { quests =>
          Ok(quests.toJson)
    }
  }

  def getQuest(id: String) = Action.async { implicit req =>
    for {
      Some(quest) <- questRepo.findById(id)
      items <- itemRepo.findByQuestId(id)
    } yield Ok((quest, items))  //(views.html.quest(quest, items))
  }

  def items(id: String) = Action.async { implicit req =>
    for {
      Some(quest) <- questRepo.findById(id)
      items <- itemRepo.findByQuestId(id)
    } yield Ok(items.toJson)
  }

 /* def delete(name: String) = Action.async { implicit req =>
    questRepo.delete(name).map(num => if (num == 0)
      NotFound
        else
      NoContent
    )
  }*/

  implicit class GetPlayerFromRequest(req: Request[AnyContent]) {
    def player: Option[Player] = req.headers.get("Auth").map(auth.getPlayer)
  }

  implicit class QuestsToJson(quests: Seq[Quest]) {
    def toJson: String = "[" + quests.foldLeft("")( (a, q) => a + "," + q.toJson ) + "]"
  }

  implicit class QuestToJson(quest: Quest) {
    def toJson: String =
      s"""{
         | "id":"${quest.id}",
         | "name":"${quest.name}",
         | "desc":"${quest.name}"
         |}
       """.stripMargin
  }

  implicit class QuestAndItemsToJson(t: (Quest, Seq[Item])) {
    def toJson: String = {
      val (quest, items) = t
      s"""{
         | "id":"${quest.id}",
         | "name":"${quest.name}",
         | "desc":"${quest.desc}",
         | "items":${items.toJson}
         |}
       """.stripMargin
    }
  }

  implicit class ItemToJson(item: Item) {
    def toJson: String =
      s"""{
         | "id":"${item.id}",
         | "desc":"${item.desc}"
         |}
       """.stripMargin
  }

  implicit class ItemsToJson(items: Seq[Item]) {
    def toJson: String = "[" + items.foldLeft("")((a, i) => a + "," + i.toJson ) + "]"
  }

}



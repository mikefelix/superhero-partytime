package models

import javax.inject.Inject

import play.api.db.slick.DatabaseConfigProvider
import play.api.libs.json.JsValue

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class Quest(id: String, name: String, desc: String)
object Quest {
  def apply(body: JsValue) = {
    val id = body \ "id"
    val name = body \ "name"
    val desc = body \ "desc"
    new Quest(id.validate[String].get, name.validate[String].get, desc.validate[String].get)
  }
}

class QuestRepo @Inject()(itemRepo: ItemRepo)(protected val dbConfigProvider: DatabaseConfigProvider) extends Repo[Quest] {
  override val tableName: String = "QUESTS"

  import dbConfig.driver.api._

  protected val Quests = dbConfig.driver.api.TableQuery[QuestsTable]

  override def findById(id: String): Future[Option[Quest]] = db.run(Quests.filter(_.id === id).result.headOption)

  override def all: Future[List[Quest]] = db.run(Quests.to[List].result)

  override def create(quest: Quest): Future[String] = db.run(Quests returning Quests.map(_.id) += quest)

  def findByPlayer(player: String): Future[List[Quest]] = db.run {
    Quests.filter(_.name === name).to[List].result
  }

  /*
    def delete(id: String): Future[Int] = {
      val q = Quests.filter(_.id === id)

      val interaction = for {
        quest <- q.result
        _             <- itemRepo._deleteAllInQuest(quest.id)
        questsDeleted <- quest.delete
      } yield questsDeleted

      db.run(interaction.transactionally)
    }
  */

  def addItem(quest: String, item: Item): Future[String] = {
    val interaction = for {
      Some(quest) <- Quests.filter(_.id === quest).result.headOption
      id <- itemRepo.insert(item)
    } yield id

    db.run(interaction.transactionally)
  }


  private class QuestsTable(tag: Tag) extends Table[Quest](tag, "QUESTS") {

    def id = column[String]("ID", O.AutoInc, O.PrimaryKey)
    def name = column[String]("NAME")
    def desc = column[String]("DESC")

    def * = (id, name, desc) <> (Quest.tupled, Quest.unapply)
    def ? = (id.?, name.?, desc.?).shaped.<>({ r =>
      r._1.map(_ => Quest.tupled((r._1.get, r._2.get, r._3.get)))
    }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))

  }
}
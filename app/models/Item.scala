package models

import javax.inject.Inject

import play.api.db.slick.DatabaseConfigProvider

import slick.driver.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global



case class Item(id: String, desc: String, quest: String, owner: String) {

  def patch(desc: Option[String], status: Option[ItemStatus.Value], quest: Option[String]): Item =
    this.copy(desc = desc.getOrElse(this.desc),
              quest = quest.getOrElse(this.quest))

}

object ItemStatus extends Enumeration {
  val ready = Value("ready")
  val set = Value("set")
  val go = Value("go")
}

class ItemRepo @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends Repo[Item] {
  import dbConfig.driver.api._

  private val Items = TableQuery[ItemsTable]

  def findById(id: String): Future[Item] = db.run {
    Items.filter(_.id === id).result.head
  }

  def findByOwner(owner: String): Future[List[Item]] = db.run {
    Items.filter(_.owner === owner).to[List].result
  }

  def findByQuestId(questId: String): Future[List[Item]] = db.run {
    Items.filter(_.quest === questId).to[List].result
  }

  def partialUpdate(id: String, color: Option[String], status: Option[ItemStatus.Value], quest: Option[String]): Future[Int] = db.run {
    val query = Items.filter(_.id === id)
    query.result.head.flatMap { item =>
      query.update(item.patch(color, status, quest))
    }
  }

  def all: DBIO[Seq[Item]] = Items.result

  def insert(Item: Item): DBIO[String] =
    Items.returning(Items.map(_.id)) += Item

  def _deleteAllInQuest(questId: String): DBIO[Int] =
    Items.filter(_.quest === questId).delete

  private class ItemsTable(tag: Tag) extends Table[Item](tag, "ITEM") {

    def id = column[String]("ID", O.AutoInc, O.PrimaryKey)
    def name = column[String]("NAME")
    def quest = column[String]("QUEST")
    def owner = column[Option[String]]("OWNER")

    def * = (id, name, quest) <> (Item.tupled, Item.unapply)
    def ? = (id.?, name.?, quest.?, owner.?).shaped.<>({ r => import r._; _1.map(_ => Item.tupled((_1.get, _2.get, _3.get, _4.get))) }, (_: Any) => throw new Exception("Inserting into ? projection not supported."))
  }

  implicit val itemStatusColumnType = MappedColumnType.base[ItemStatus.Value, String](
    _.toString, string => ItemStatus.withName(string))

}

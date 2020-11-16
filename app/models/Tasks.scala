package models

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext

import play.api.db.slick.{DatabaseConfigProvider => DBConfigProvider}

import models.Dao

/**
  * task テーブルへの Accessor
  */
@Singleton
class Tasks @Inject()(dbcp: DBConfigProvider)(implicit ec: ExecutionContext) extends Dao(dbcp) {

  import profile.api._
  import utility.Await

  val table = "task"

  /**
    * DB上に保存されている全てのタスクを取得する
    * @return
    */
  def list: Seq[Task] = Await.result(
    db.run(sql"SELECT id, title, description, is_done, username, created_at FROM #$table".as[Task])
  )
  def findByIDAndUsername(id: Int, username: String): Option[Task] = Await.result(
    db.run(
      sql"SELECT id, title, description, is_done, username, created_at FROM #$table WHERE id=#$id AND username='#$username'"
        .as[Task]
        .headOption
    )
  )

  def listByUser(username: String): Seq[Task] = Await.result(
    db.run(
      sql"SELECT id, title, description, is_done, username, created_at FROM #$table WHERE username='#$username'"
        .as[Task]
    )
  )

  def save(task: Task): Int = task match {
    case Task(0, title, description, is_done, username, _) =>
      Await.result(
        db.run(
          sqlu"INSERT INTO #$table (title, description, is_done, username) VALUES ('#$title', '#$description', '#$is_done', '#$username')"
        )
      )
    case Task(id, title, description, is_done, _, _) =>
      Await.result(
        db.run(
          sqlu"UPDATE #$table SET title='#$title', description='#$description', is_done='#$is_done' WHERE id = #$id"
        )
      )
  }

  def deletebyID(id: Int) = {
    Await.result(
      db.run(sqlu"DELETE FROM #$table WHERE id = #$id")
    )
  }

  def deletebyUsername(username: String) = {
    Await.result(
      db.run(sqlu"DELETE FROM #$table WHERE username = '#$username'")
    )
  }
  def updateStatus(id: Int, is_done: Boolean): Int = {
    Await.result(
      db.run(sqlu"UPDATE #$table SET is_done='#$is_done' WHERE id = #$id")
    )
  }
}

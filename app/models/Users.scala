package models

import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider => DBConfigProvider}

import scala.concurrent.ExecutionContext

/**
  * userTable テーブルへの Accessor
  */
@Singleton
class Users @Inject()(dbcp: DBConfigProvider)(implicit ec: ExecutionContext) extends Dao(dbcp) {

  import profile.api._
  import utility.Await

  val table = "userTable"

  /**
    * DB上に保存されている全てのタスクを取得する
    * @return
    */
//    多分いらない
  def list: Seq[User] = Await.result(
    db.run(sql"SELECT username, password FROM #$table".as[User])
  )
  def findByUsername(username: String): Option[User] = Await.result(
    db.run(sql"SELECT username, password FROM #$table WHERE username='#$username'".as[User].headOption)
  )

  def save(user: User): Int = {
    Await.result(
      db.run(sqlu"INSERT INTO #$table (username, password) VALUES ('#${user.username}', '#${user.password}')")
    )
  }

  def delete(username: String) = {
    Await.result(
      db.run(sqlu"DELETE FROM #$table WHERE username = '#$username'")
    )
  }
  def updatePassword(user: User): Int = {
    Await.result(
      db.run(sqlu"UPDATE #$table SET password='#${user.password}' WHERE username = '#${user.username}'")
    )
  }
}

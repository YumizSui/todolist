package models.formapp

import javax.inject.{Inject, Singleton}
import scala.concurrent.ExecutionContext
import play.api.db.slick.{DatabaseConfigProvider => DBConfigProvider}

import models.Dao

/**
  * enquete dao
  */
@Singleton
class Enquetes @Inject()(dbcp: DBConfigProvider)(implicit ec: ExecutionContext) extends Dao(dbcp) {

  import profile.api._
  import utility.Await

  val table = "enquete"

  def list: Seq[Enquete] = Await.result(
    db.run(sql"SELECT id, name, gender, message, created_at FROM #$table ORDER BY id".as[Enquete])
  )

  def findByID(id: Int): Option[Enquete] = Await.result(
    db.run(sql"SELECT id, name, gender, message, created_at FROM #$table WHERE id=#$id".as[Enquete].headOption)
  )

  def save(enquete: Enquete): Int = enquete match {
    case Enquete(0, name, gender, message, _) =>
      Await.result(
        db.run(sqlu"INSERT INTO #$table (name, gender, message) VALUES ('#$name', '#$gender', '#$message')")
      )
    case Enquete(id, name, gender, message, _) =>
      Await.result(
        db.run(sqlu"UPDATE #$table SET name=#$name, gender=#$gender, message=#$message WHERE id = #$id")
      )
  }

}

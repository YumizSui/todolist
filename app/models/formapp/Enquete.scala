package models.formapp

import java.sql.Timestamp

import models.DomainModel

/**
  * Domain model of enquete
  * @param id        ID
  * @param name      氏名
  * @param gender    性別
  * @param message   メッセージ
  * @param createdAt 作成日時
  */
case class Enquete(id: Int, name: String, gender: String, message: String, createdAt: Timestamp)

object Enquete extends DomainModel[Enquete] {
  import slick.jdbc.GetResult
  override implicit def getResult: GetResult[Enquete] = GetResult(
    r => Enquete(r.nextInt, r.nextString, r.nextString, r.nextString, r.nextTimestamp)
  )

  def apply(name: String, gender: String, message: String): Enquete = Enquete(0, name, gender, message, null)
}

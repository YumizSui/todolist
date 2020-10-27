package models

import java.sql.Timestamp

/**
  * Domain model of task
  * @param id          ID
  * @param title       タスク名
  * @param description タスクの説明
  * @param isDone      完了状態
  * @param createdAt   作成日時
  */
case class Task(id: Int, title: String, description: String, isDone: Boolean, createdAt: Timestamp)

object Task extends DomainModel[Task] {
  import slick.jdbc.GetResult
  implicit def getResult: GetResult[Task] = GetResult(
    r => Task(r.nextInt, r.nextString, r.nextString, r.nextBoolean, r.nextTimestamp)
  )

  def apply(title: String, description: String, isDone: Boolean): Task = Task(0, title, description, isDone, null)
}

package models

/**
  * Domain model of account
  * @param username       ユーザ名
  * @param password       パスワード
  */
case class User(username: String, password: String)

object User extends DomainModel[User] {
  import slick.jdbc.GetResult
  implicit def getResult: GetResult[User] = GetResult(
    r => User(r.nextString, r.nextString)
  )
}

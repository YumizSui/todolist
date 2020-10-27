package utility

import scala.concurrent.{Future, Await => ScalaAwait}
import scala.concurrent.duration._

/**
  * Future 周りの処理
  */
object Await {

  val defaultAwaitDuration: Duration = 10.seconds

  /**
    * Future を unwrap する
    * @param future
    * @return Futureの中身を返す
    */
  def result[T](future: Future[T]): T = ScalaAwait.result(future, defaultAwaitDuration)

}

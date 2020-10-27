package models

trait DomainModel[T] {
  import slick.jdbc.GetResult
  def getResult: GetResult[T]
}

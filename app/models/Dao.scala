package models

import play.api.db.slick.{DatabaseConfigProvider => DBConfigProvider, HasDatabaseConfigProvider => HasDBConfigProvider}
import slick.jdbc.JdbcProfile

class Dao(protected val dbConfigProvider: DBConfigProvider) extends HasDBConfigProvider[JdbcProfile]

val projectConfig =
    com.typesafe.config.ConfigFactory.parseFile(new File("conf/application.conf"))

name := projectConfig.getString("app.name")
version := projectConfig.getString("app.version")
scalaVersion := "2.13.3"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

val playSlickVersion = "4.0.2"
libraryDependencies ++= Seq(
  guice,
  cacheApi,
  "com.typesafe.play" %% "play-slick"            % playSlickVersion,
  "com.typesafe.play" %% "play-slick-evolutions" % playSlickVersion,
  "com.h2database"    %  "h2"                    % "1.4.200"
)

scalafmtOnCompile := true

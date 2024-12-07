ThisBuild / name         := "fpinscala"
ThisBuild / version      := "0.1.0"
ThisBuild / scalaVersion := "3.5.2"

ThisBuild / libraryDependencies ++= Seq(
	"org.scalatest"  %% "scalatest"  % "3.2.19" % "test",
	"com.lihaoyi" %% "pprint" % "0.9.0"
)

lazy val root =
	project
		.in(file("."))

scalacOptions ++= Seq(
	"-encoding", "utf8",
	"-feature",
	"-language:implicitConversions",
	"-language:existentials",
	"-unchecked",
	"-Werror",
	"-deprecation"
)

Compile / run / fork := true
Compile / run / connectInput := true
Compile / run / javaOptions += "-Xmx4G"

ThisBuild / watchBeforeCommand := Watch.clearScreen

ThisBuild / shellPrompt := {
	(state: State) =>
		s"sbt:${(ThisBuild / name).value}:" +
			s"${Project.extract(state).currentProject.id}" +
			s"${scala.Console.CYAN}>${scala.Console.RESET}"
}

lazy val root =
	project
		.in(file("."))
		.settings( scalaVersion := "3.2.2"
						, name    := "fpinscala"
						, version := "0.1.0"
						, libraryDependencies ++= Seq(
								"org.scalatest"  %% "scalatest"  % "3.2.15" % "test",
								"com.lihaoyi" %% "pprint" % "0.8.1"
							)
						)

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
Compile / run / javaOptions += "-Xmx4G"

import Dependencies._

name := "geotrellis-vector"
libraryDependencies ++= Seq(
  jts,
  sprayJson,
  apacheMath,
  spire,
  scalatest   % "test",
  scalacheck  % "test"
)

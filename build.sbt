name := "Proteus"

scalaVersion := "2.12.18"
val spinalVersion = "1.15.0"

fork := true

libraryDependencies ++= Seq(
  "com.github.spinalhdl" %% "spinalhdl-core" % spinalVersion,
  "com.github.spinalhdl" %% "spinalhdl-lib" % spinalVersion,
  compilerPlugin("com.github.spinalhdl" %% "spinalhdl-idsl-plugin" % spinalVersion),
  "com.lihaoyi" %% "ujson" % "4.4.3"
)

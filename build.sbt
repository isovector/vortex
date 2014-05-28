name := "vortex"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies += "org.spire-math" %% "spire" % "0.7.4"

Twirl.settings

Twirl.twirlImports := Seq("latex.latex._", "spire.implicits._")

Twirl.twirlSourceCharset := java.nio.charset.Charset.forName("ISO-8859-1")

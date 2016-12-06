enablePlugins(ScalaJSPlugin, WorkbenchPlugin)

name := "pigment"

scalaVersion := "2.12.1"

val circeVersion = "0.6.1"
libraryDependencies ++= (
  "com.github.fdietze" %%% "pharg" % "0.1.0-SNAPSHOT" ::
  "com.github.fdietze" %%% "vectory" % "0.1.0-SNAPSHOT" ::
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.3" ::
  "me.chrons" %%% "diode" % "1.1.0" ::
  "me.chrons" %%% "diode-react" % "1.1.0" ::
  // "me.chrons" %%% "boopickle" % "1.2.5" ::
  "io.circe" %%% "circe-core" % circeVersion ::
  "io.circe" %%% "circe-generic" % circeVersion ::
  "io.circe" %%% "circe-parser" % circeVersion ::
  "com.github.fdietze" %%% "scalajs-react-d3-force-layout" % "0.1.0-SNAPSHOT" ::
  Nil
)

persistLauncher := true

val reactVersion = "15.4.1"
jsDependencies ++= Seq(
  "org.webjars.bower" % "react" % reactVersion
    / "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % reactVersion
    / "react-dom.js"
    minified "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % reactVersion
    / "react-dom-server.js"
    minified "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer"
)

scalacOptions ++=
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  Nil

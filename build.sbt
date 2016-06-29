enablePlugins(ScalaJSPlugin)

name := "pigment"

scalaVersion := "2.11.8"

libraryDependencies ++= (
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
  "me.chrons" %%% "diode" % "1.0.0" ::
  "me.chrons" %%% "diode-react" % "1.0.0" ::
  Nil
)

persistLauncher := true

jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.1.0"
    / "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.1.0"
    / "react-dom.js"
    minified "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.1.0"
    / "react-dom-server.js"
    minified "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer"
)

import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

name := "pigment"

scalaVersion := "2.11.8" // or any other Scala version >= 2.10.2

// core = essentials only. No bells or whistles.
libraryDependencies ++= (
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
  "me.chrons" %%% "diode" % "1.0.0" ::
  "me.chrons" %%% "diode-react" % "1.0.0" ::
  "org.singlespaced" %%% "scalajs-d3" % "0.3.3" ::
  "com.assembla.scala-incubator" %%% "graph-core" % "1.11.0" ::
  Nil
)

persistLauncher := true

// React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
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

// workbench
workbenchSettings

bootSnippet := "pigment.Main().main();"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

// scalaxy - rewrite collection code to while loops
scalacOptions += "-Xplugin-require:scalaxy-streams"

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

scalacOptions ++=
  "-encoding" :: "UTF-8" ::
  "-unchecked" ::
  "-deprecation" ::
  "-explaintypes" ::
  "-feature" ::
  "-language:_" ::
  "-Xlint:_" ::
  "-Ywarn-unused" ::
  // "-Xdisable-assertions" ::
  // "-optimize" ::
  // "-Yopt:_" :: // enables all 2.12 optimizations
  // "-Yinline" :: "-Yinline-warnings" ::
  Nil

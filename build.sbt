import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

name := "pigment"

scalaVersion := "2.11.8"

libraryDependencies ++= (
  "com.github.japgolly.scalajs-react" %%% "core" % "0.11.1" ::
  "me.chrons" %%% "diode" % "1.0.0" ::
  "me.chrons" %%% "diode-react" % "1.0.0" ::
  "com.github.fdietze" %%% "scalajs-react-d3-force-layout" % "0.1.0-SNAPSHOT" ::
  Nil
)

persistLauncher := true

jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.2.1"
    / "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.2.1"
    / "react-dom.js"
    minified "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.2.1"
    / "react-dom-server.js"
    minified "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer"
)

// workbench
workbenchSettings

bootSnippet := "pigment.Main().main();"

// updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)
refreshBrowsers <<= refreshBrowsers.triggeredBy(fastOptJS in Compile)

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

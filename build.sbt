import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

name := "rainbow"

scalaVersion := "2.11.8" // or any other Scala version >= 2.10.2

// core = essentials only. No bells or whistles.
libraryDependencies ++= (
  "com.github.japgolly.scalajs-react" %%% "core" % "0.10.4" ::
  "me.chrons" %%% "diode" % "0.5.2" ::
  "me.chrons" %%% "diode-react" % "0.5.2" ::
  Nil
)

// execute js with node
scalaJSUseRhino in Global := false

// React JS itself (Note the filenames, adjust as needed, eg. to remove addons.)
jsDependencies ++= Seq(

  "org.webjars.bower" % "react" % "15.0.2"
    / "react-with-addons.js"
    minified "react-with-addons.min.js"
    commonJSName "React",

  "org.webjars.bower" % "react" % "15.0.2"
    / "react-dom.js"
    minified "react-dom.min.js"
    dependsOn "react-with-addons.js"
    commonJSName "ReactDOM",

  "org.webjars.bower" % "react" % "15.0.2"
    / "react-dom-server.js"
    minified "react-dom-server.min.js"
    dependsOn "react-dom.js"
    commonJSName "ReactDOMServer"
)

// workbench
workbenchSettings

bootSnippet := "rainbow.Main().main();"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

// scalaxy - rewrite collection code to while loops
scalacOptions += "-Xplugin-require:scalaxy-streams"

scalacOptions in Test ~= (_ filterNot (_ == "-Xplugin-require:scalaxy-streams"))

scalacOptions in Test += "-Xplugin-disable:scalaxy-streams"

autoCompilerPlugins := true

resolvers += Resolver.sonatypeRepo("snapshots")

addCompilerPlugin("com.nativelibs4java" %% "scalaxy-streams" % "0.3.4")

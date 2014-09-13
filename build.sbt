import com.github.retronym.SbtOneJar._

name := "ScalaChem"

version := "1.0"

exportJars := true

oneJarSettings

libraryDependencies += "com.github.scopt" %% "scopt" % "3.2.0"

libraryDependencies += "org.apache.commons" % "commons-io" % "1.3.2"

libraryDependencies += "com.google.code.gson" % "gson" % "2.2"

resolvers += Resolver.sonatypeRepo("public")

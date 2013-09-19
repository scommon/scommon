
javacOptions ++= Seq("-Xlint:none", "-XDignore.symbol.file")

libraryDependencies += "commons-io" % "commons-io" % "2.4"

libraryDependencies += "commons-lang" % "commons-lang" % "2.6"

libraryDependencies += "commons-collections" % "commons-collections" % "3.2.1"

libraryDependencies += "commons-configuration" % "commons-configuration" % "1.9"

libraryDependencies += "org.objenesis" % "objenesis" % "2.0"

libraryDependencies += "org.javassist" % "javassist" % "3.18.0-GA"

libraryDependencies += "com.google.inject" % "guice" % "3.0"

libraryDependencies += "com.google.code.findbugs" % "jsr305" % "1.3.+"



libraryDependencies += "com.typesafe" % "config" % "1.0.2"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"

libraryDependencies += "junit" % "junit" % "4.11" % "test"

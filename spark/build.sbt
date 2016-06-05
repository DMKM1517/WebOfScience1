name := "AD"

version := "1.0"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq("org.apache.spark" %% "spark-core" % "1.6.1" ,
  "org.apache.spark" %% "spark-mllib" % "1.6.1" ,
  "org.apache.spark" %% "spark-sql" % "1.6.1" ,
  "org.apache.spark" %% "spark-hive" % "1.6.1" ,
  "org.apache.spark" %% "spark-streaming" % "1.6.1",
  "com.databricks" % "spark-csv_2.10" % "1.4.0",
  "org.postgresql" % "postgresql" % "9.4-1201-jdbc41",
  "mysql" % "mysql-connector-java" % "5.1.34"

)

libraryDependencies += "com.rockymadden.stringmetric" % "stringmetric-core_2.10" % "0.27.3"

libraryDependencies += "org.deeplearning4j" % "deeplearning4j-scaleout" % "0.4-rc3.8"

mainClass := some("ADMain")
mainClass in Compile := some("ADMain")
assemblyJarName := "author-disambiguation.jar"



assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
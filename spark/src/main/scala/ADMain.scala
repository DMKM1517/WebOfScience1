import WriteToCSV._
import org.apache.spark.sql.DataFrame
import org.apache.spark.{SparkConf, SparkContext}

/**
  * Created by krishna on 12/03/16.
  */
object ADMain {


  // Create A spark context
  val sc = {
    val jarloc = "/Users/krishna/IdeaProjects/AD/target/scala-2.10/ad_2.10-1.0.jar/mysql-connector-java-5.1.12.jar"

    val conf = new SparkConf().setAppName("AuthorDisambiguation")
      .setSparkHome("/Users/krishna/spark")
      .setMaster("local[4]")
      .setJars(List(jarloc))
    new SparkContext(conf)
  }

  def main(args: Array[String]) {

    // Feature Matrix
    val feature_matrix:DataFrame = Features.features(sc)


  }

}

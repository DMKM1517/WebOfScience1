import org.apache.spark.{SparkConf, SparkContext}



/**
  * Created by krishna on 27/03/16.
  */
object test1 {

  val conf = new SparkConf().setAppName("Test Case")
    .setSparkHome("/Users/krishna/spark")
    .setMaster("local[4]")
  val sc = new SparkContext(conf)

  val sqlContext = new org.apache.spark.sql.SQLContext(sc)
  import sqlContext.implicits._

  val df = sc.parallelize(Seq(
    (1L, 3.0, "a"), (2L, -1.0, "b"), (3L, 0.0, "c")
  )).toDF()

  case class Foobar(foo: Double, bar: Double)

  def main(args: Array[String]) {

    def authorDistance(authorlist : List[String],author :String) =
    {
      val op = authorlist.filter(_ !=author)
      println(op)
    }
    authorDistance(List("b","a"),"a")

  }
}

import java.util.Properties

import PreProcessingUtil._
import org.apache.spark.SparkContext
import org.apache.spark.sql.functions.{col, _}
import org.apache.spark.sql.hive.HiveContext
import org.apache.spark.sql.{DataFrame, SQLContext}


/*
* Summary
* Articles Records : 3,18,591 rows
* Columns id,title,journal,year,kw,authors,d,co-author,phonetic,sigID
* */

object Features {

  private val AppName = "Data PreProcessing"

  def features(sc: SparkContext): DataFrame = {

    val sqlContext = new SQLContext(sc)
    val sqlContext1: SQLContext = new HiveContext(sc)

    // id, subject
    val df_assoication = sqlContext.read.format("jdbc").option("url", "jdbc:mysql://localhost:8889/dmkm_articles")
      .option("driver", "com.mysql.jdbc.Driver")
      .option("dbtable", "subject_asociations")
      .option("user", "root")
      .option("password", "dmkm1234").load()
    //.limit(10)

    // df_assoication.show(3)

    // Authors - id,d,author
    val df_author = sqlContext1.read.format("jdbc").option("url", "jdbc:mysql://localhost:8889/dmkm_articles")
      .option("driver", "com.mysql.jdbc.Driver")
      .option("dbtable", "articles_authors")
      .option("user", "root")
      .option("password", "dmkm1234").load()
    //  .limit(10)

    // Articles - id, authors, title, journal, type, numrefs, times_cited, doi, year, volume, issue, BE, EP, abbr,ut, nsr
    val df_article = sqlContext.read.format("jdbc").option("url", "jdbc:mysql://localhost:8889/dmkm_articles")
      .option("driver", "com.mysql.jdbc.Driver")
      .option("dbtable", "articles")
      .option("user", "root")
      .option("password", "dmkm1234").load()
    //.limit(10)

    // Keywords - id, type_keyw, keyword
    val df_keyword = sqlContext1.read.format("jdbc").option("url", "jdbc:mysql://localhost:8889/dmkm_articles")
      .option("driver", "com.mysql.jdbc.Driver")
      .option("dbtable", "articles_keywords_clean_sel")
      .option("user", "root")
      .option("password", "dmkm1234").load()
    // .limit(10)
    val df_keyword1 = df_keyword.groupBy(col("id")).agg(concat_ws(",", collect_list(col("keyword"))).alias("keyword"))


    // Institution - id, d1, d2, institution
    val insti = sqlContext1.read.format("jdbc").option("url", "jdbc:mysql://localhost:8889/dmkm_articles")
      .option("driver", "com.mysql.jdbc.Driver")
      .option("dbtable", "articles_institutions")
      .option("user", "root")
      .option("password", "dmkm1234").load().select("id", "d1", "institution")
    // .limit(10)

    val insti1 = insti.groupBy(col("id"), col("d1")).agg(concat_ws(",", collect_list(col("institution"))).alias("institution"))

    // Join df_author + df_article
    val transform0 = df_author.as("author").join(df_article.as("article"), col("author.id") <=> col("article.id"), "left_outer")
      .drop(col("article.id")).drop(col("article. authors")).drop(col("type")).drop(col("numrefs")).drop(col("times_cited"))
      .drop(col("doi")).drop(col("volume")).drop(col("issue")).drop(col("BP")).drop(col("EP")).drop(col("abbr"))
      .drop(col("ut")).drop(col("nsr")).drop(col("authors"))

    // Join transform0 + df_keyword1
    val transform1 = transform0.as("trans0").join(df_keyword1.as("keyword"), col("trans0.id") <=> col("keyword.id"), "leftouter")
      .drop(col("keyword.id"))

    // Join transform1 + insti1
    val transform2 = transform1.as("t1").
      join(insti1.as("insti"),
        (col("t1.id") <=> col("insti.id"))
          &&
          (col("d") <=> col("d1")),
        "leftouter").drop(col("d1")).drop(col("insti.id"))

    // id2GT + authorFullGT
    val groundtruth = DataGroundTruth.data(sc)

    // author - remove accent, lower, trim
    val transform3 = transform2.withColumn("author_pro", textPreprocess(stringNormalizer(col("author"))))

    // co-author
    val transform4 = transform3.groupBy(col("id")).agg(concat_ws(",", collect_list(col("author_pro"))).alias("coauthor_all"))

    // Join transform3 + transform4
    val transform5 = transform3.as("t3").join(transform4.as("t4"), col("t3.id") <=> col("t4.id"), "leftouter").drop(col("t4.id"))


    // Generate Co-Authors
    val transform6 = transform5.withColumn("coauthor", authorR(col("coauthor_all"), col("author_pro")))
    //transform6.show(3)

    // Generate First Name
    val transform7 = transform6.withColumn("first_name", textPreprocess(authorUDF1(col("author"))))
    //transform7.show(3)

    // Generate Initials
    val transform8 = transform7.withColumn("initial", textPreprocess(authorUDF2(col("author"))))
    //transform8.show(3)

    // Get Subjects
    val transform9 = transform8.as("t9").join(df_assoication.as("sub"), col("t9.id") <=> col("sub.id"), "leftouter")
      .drop(col("sub.id"))
    //transform9.show(3)

    // Double NysiisAlgorithm
    val transform10 = transform9.withColumn("phonetic", stringN(col("first_name")))
    //transform10.show(3)

    // Ground truth data - Current Right Outer
    val transform11 = transform10.as("t10").join(groundtruth.as("gt"),
      (col("t10.id") <=> col("gt.id2GT")) &&
        (col("t10.d") <=> col("gt.dGT")),
      "rightouter")
      .drop(col("gt.id2GT"))
      .drop(col("gt.dGT"))
    //transform11.show(3)

    // Generate Signature
    val transform12 = transform11.withColumn("sigID", monotonicallyIncreasingId)

    val prop = new Properties()
    prop.setProperty("user", "root")
    prop.setProperty("password", "dmkm1234")

    transform12.write.jdbc("jdbc:mysql://localhost:8889/dmkm_articles", "signature_combination", prop)
    // transform14.show(3)


    return transform12
  }


}



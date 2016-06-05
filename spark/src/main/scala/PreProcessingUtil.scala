import com.rockymadden.stringmetric.phonetic._
import com.rockymadden.stringmetric.similarity.JaroWinklerMetric
import org.apache.commons.lang3.StringUtils
import org.apache.spark.sql.Column
import org.apache.spark.sql.functions._

/**
  * Created by krishna on 16/03/16.
  */
object PreProcessingUtil {

  // Author Pre Processing
  def auth1(name : String):String ={
    val str1 = name.split(" ").head
    return str1
  }

  def auth2(name : String):String ={
    val pattern = "[A-Z]+".r
    val str2 = pattern.findAllMatchIn(name).mkString("").drop(1)
    return str2
  }

  val authorUDF1 = udf((s: String) => auth1(s))
  val authorUDF2 = udf((s: String) => auth2(s))





  // Remove Accents
  val stringNormalizer = udf((s: String) => StringUtils.stripAccents(s))

  def textPreprocess(c: Column) ={
    lower(trim(c))
  }



  // Lower , Remove Punctuations , Split by ;
  def splitAuthor(c: Column) = split(
    regexp_replace(

      (trim(c)), "[^a-zA-Z0-9,\\s]", ""), ";"
    //regexp_replace(lower(trim(c)), "[^a-zA-Z0-9;\\s]", ""), ";"
  )

  // My Custom trim function
  val custT = udf((s: Seq[String]) => custTrim(s))
  def custTrim(name : Seq[String]) ={
    name.map(s => s.trim)
  }

  val ArrayFix = udf(
    (s: String) => fixArray(s)
  )


  def fixArray(s : String) = {
    s.drop(1).drop(1).dropRight(1).split(", u").map(_.drop(1).dropRight(1).trim())
  }

  val authorD  = udf((s: Seq[String]) => authorDistance(s))
  def authorDistance(name : Seq[String]) ={ name.zipWithIndex.map(_._2)}

  // Zips ID and Author together
  val zip = udf((xs: Seq[Int], ys: Seq[String]) => xs.zip(ys))

  val authorR  = udf((al: String,a:String) => authorRem(al,a))

  def authorRem(authorlist : String,author :String) = {
    authorlist.split(",").filter(_ !=author).mkString(",")
  }

  val gtC  = udf((a1: Int,a2:Int) => gtCompare(a1,a2))

  def gtCompare(id1: Int, id2: Int ): Int = {
    if(id1==id2)
      return 1
    else
      return 0
  }


  val stringN = udf((s: String) => NysiisAlgorithm.compute(s))
  val stringRN = udf((s: String) => RefinedNysiisAlgorithm.compute(s))
  val stringRS = udf((s: String) => RefinedSoundexAlgorithm.compute(s))
  val stringM = udf((s: String) => MetaphoneAlgorithm.compute(s))
  val stringS = udf((s: String) => SoundexAlgorithm.compute(s))
  val stringSimilarity = udf((s1: String,s2:String) => JaroWinklerMetric.compare(s1,s2))


}

/*
// Remove duplicate co-authors
  val authorR  = udf((al: Seq[String],a:String) => authorRem(al,a))
  def authorRem(authorlist : Seq[String],author :String) = {
    authorlist.filter(_ !=author)
  }
 */
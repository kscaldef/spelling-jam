import scala.io.Source

object Checker {
  val Alphabet = 'a' to 'z'
  
  def words(filename: String): Iterator[String] =
    (Source fromFile filename getLines) flatMap { _ split """[^a-zA-Z]""" } map { _.toLowerCase }
  
  def wc(words: Iterator[String]): Map[String, Int] = {
    words.foldLeft(Map[String, Int]()) { (acc, word) =>
      acc.updated(word, acc.getOrElse(word, 0) + 1)
    }
  }
  
  def permute(word: String): Map[String, Double] = {
    val splits = for (i <- 0 until word.length)
      yield (word.substring(0, i), word.substring(i))
    
    val deletes = for ((front, back) <- splits; if !back.isEmpty)
      yield front + back.substring(1) -> 0.1d
    
    val transposes = for ((front, back) <- splits; if back.length > 1)
      yield front + back(1) + back(0) + back.substring(2) -> 0.1d
    
    val replaces = for ((front, back) <- splits; c <- Alphabet; if !back.isEmpty)
      yield front + c + back.substring(1) -> 0.1d
    
    val inserts = for ((front, back) <- splits; c <- Alphabet)
      yield front + c + back -> 0.1d
    
    (deletes ++ transposes ++ replaces ++ inserts).foldLeft(Map[String, Double]()) {
      case (acc, (word, p)) =>
        acc.updated(word, 1d - (1d - acc.getOrElse(word, 0d)) * (1d - p))
    }
  }
  
  def recPermute(word: String, iterations: Int): Map[String, Double] = {
    val once = permute(word)
    
    if (iterations == 1)
      once
    else {
      val maps: Iterable[Map[String, Double]] = once map {
        case (word, p) =>
          recPermute(word, iterations - 1) mapValues { _ * p }
      }
      
      maps.foldLeft(Map[String, Double]()) { (acc, map) =>
        map.foldLeft(acc) {
          case (acc, (word, p)) =>
            acc.updated(word, 1d - (1d - acc.getOrElse(word, 0d)) * (1d - p))
        }
      }
    }
  }
  
  def scored(dict: Map[String, Int], word: String): Map[String, Double] = {
    val total = dict.values.sum
    
    recPermute(word, 2) map {
      case (word, p) => word -> (1d - p * (dict.getOrElse(word, 0).toDouble / total))      // P(w|c)P(c)
    }
  }
  
  def correct(dict: Map[String, Int], word: String): List[String] = {
    val candidates = scored(dict, word).toList
    candidates sortBy { _._2 } map { _._1 }
  }

  def test(dict: Map[String, Int], filename: String) {
    import java.util.Date

    val testData: Iterator[(String, Seq[String])] =
      (Source fromFile filename getLines) filter (!_.startsWith("#")) map ( _ split """\s+""" ) map ( words => (words.head, words.tail) )

    val start = new Date
    var total, right, incorrect = 0

    testData.foreach { 
      case (word, errors) =>
        errors.foreach { error =>
          total += 1
          if (correct(dict, error).headOption.getOrElse("") == word) 
            right += 1
          else
            incorrect += 1
        }
    }

    val end = new Date
    println("%s total, %s correct, %s incorrect, %s%% accuracy; in %s ms".format(total, right, incorrect, 100d * right / total, end.getTime - start.getTime))
  }
}

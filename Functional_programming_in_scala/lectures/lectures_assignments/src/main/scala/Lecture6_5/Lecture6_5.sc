import scala.io.Source

object Main{
    val in = Source.fromURL("https://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt")
    val words = in.getLines.toList

    val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
    )

    /** Invert the mnem map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
    val charCode: Map[Char, Char] = for {
      (digit, str) <- mnem
      ltr <- str
    } yield ltr -> digit

    val charCode2: Map[Char, Char] = (mnem foldLeft Map[Char, Char]()) (deconstruct)

    def deconstruct(map: Map[Char, Char], pair: (Char, String)): Map[Char, Char] = {
      val (digit, str) = pair
      map ++ (str map (_ -> digit)).toMap
    }

    /** Maps a word to the digit string it can represent, e.g. "Java" -> "5282" */
    def wordCode(word: String): String = word.toUpperCase map (ltr => if(ltr.isLetter) charCode(ltr) else ' ')

    def wordCode2(word: String): String = for {
      ltr <- word.toUpperCase
    } yield charCode(ltr)

    def construct(map: Map[String, Seq[String]], word: String): Map[String, Seq[String]] = {
      val code = wordCode(word)
      map + (code -> (map get code match {
        case Some(seq) => seq ++ List(word)
        case None => List(word)
      }))
    }

    /**
      * A map from digit strings to the words that represent them
      * e.g. "5282" -> List("Java", "Kata", "Lava", ...)
      * Note: a missing number should map to the empty set e.g. "1111" -> List()
      */
    val wordsForNum: Map[String, Seq[String]] = words.toList groupBy wordCode withDefaultValue Seq()

    val wordsForNum2: Map[String, Seq[String]] = (words foldLeft Map[String, Seq[String]]()) (construct)

    /** return all ways to encode a number as a list of words */
    def encode(number: String): Set[List[String]] = {
      if(number.isEmpty) Set(List())
      else {
        for{
          split <- 1 to number.length
          word <- wordsForNum(number take split)
          rest <- encode(number drop split)
        } yield word::rest
      }.toSet
    }

    encode("7225247386")

  def translate(number: String): Set[String] = encode(number) map (_.mkString(" "))

  translate("7225247386")
}
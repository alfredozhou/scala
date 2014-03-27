package forcomp

object testing {
  type Word = String
  type Occurrences = List[(Char, Int)]
  type Sentence = List[Word]
  val dictionary: List[Word] = loadDictionary     //> dictionary  : List[forcomp.testing.Word] = List(Aarhus, Aaron, Ababa, aback,
                                                  //|  abaft, abandon, abandoned, abandoning, abandonment, abandons, abase, abased
                                                  //| , abasement, abasements, abases, abash, abashed, abashes, abashing, abasing,
                                                  //|  abate, abated, abatement, abatements, abater, abates, abating, Abba, abbe, 
                                                  //| abbey, abbeys, abbot, abbots, Abbott, abbreviate, abbreviated, abbreviates, 
                                                  //| abbreviating, abbreviation, abbreviations, Abby, abdomen, abdomens, abdomina
                                                  //| l, abduct, abducted, abduction, abductions, abductor, abductors, abducts, Ab
                                                  //| e, abed, Abel, Abelian, Abelson, Aberdeen, Abernathy, aberrant, aberration, 
                                                  //| aberrations, abet, abets, abetted, abetter, abetting, abeyance, abhor, abhor
                                                  //| red, abhorrent, abhorrer, abhorring, abhors, abide, abided, abides, abiding,
                                                  //|  Abidjan, Abigail, Abilene, abilities, ability, abject, abjection, abjection
                                                  //| s, abjectly, abjectness, abjure, abjured, abjures, abjuring, ablate, ablated
                                                  //| , ablates, ablating, abl
                                                  //| Output exceeds cutoff limit.

  def combinations(occurrences: Occurrences): List[Occurrences] = {
    def toCharList(occurrence: Occurrences): List[Char] = occurrence.map(e => (e._1.toString * e._2).toList).flatten

    def powerList(word: List[Char]): List[List[Char]] = {
      def power(s: List[Char], acc: List[List[Char]]): List[List[Char]] = s match {
        case Nil => acc
        case a :: as => power(as, acc ::: (acc map (a :: _)))
      }
      power(word, Nil :: Nil)
    }

    val uniqcomboStrings = (powerList(toCharList(occurrences)) map (e => e.mkString)).toSet.toList

    uniqcomboStrings.map(x => wordOccurrences(x))
  }                                               //> combinations: (occurrences: forcomp.testing.Occurrences)List[forcomp.testing
                                                  //| .Occurrences]

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary groupBy wordOccurrences withDefaultValue List()
                                                  //> dictionaryByOccurrences  : Map[forcomp.testing.Occurrences,List[forcomp.test
                                                  //| ing.Word]] = <lazy>
  def wordOccurrences(w: Word): Occurrences =
    w.groupBy((x: Char) => x.toLower).toList.map(e => (e._1, e._2.length)).sortWith((e1, e2) => (e1._1 < e2._1))
                                                  //> wordOccurrences: (w: forcomp.testing.Word)forcomp.testing.Occurrences
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.reduce((a1, a2) => a1 ++: a2))
                                                  //> sentenceOccurrences: (s: forcomp.testing.Sentence)forcomp.testing.Occurrenc
                                                  //| es

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    def minus(input: Map[Char, Int], pair: (Char, Int)): Map[Char, Int] = {
    	println(input)
      val (yChar, yInt) = pair
      val occ = input(yChar) - yInt
      if (occ > 0)
        input.updated(yChar, occ)
      else if (occ == 0)
        input - yChar
      else
      	throw new Error("Cannot be negative")
    }
    ((y foldLeft x.toMap)(minus)).toList.sortWith(_._1 < _._1)
  }                                               //> subtract: (x: forcomp.testing.Occurrences, y: forcomp.testing.Occurrences)f
                                                  //| orcomp.testing.Occurrences

  val lard = wordOccurrences("ilruuxz")           //> lard  : forcomp.testing.Occurrences = List((i,1), (l,1), (r,1), (u,2), (x,1
                                                  //| ), (z,1))
  val empty = wordOccurrences("rill")             //> empty  : forcomp.testing.Occurrences = List((i,1), (l,2), (r,1))
  subtract(lard, empty)                           //> Map(x -> 1, u -> 2, i -> 1, l -> 1, r -> 1, z -> 1)
                                                  //| Map(x -> 1, u -> 2, l -> 1, r -> 1, z -> 1)
                                                  //| java.lang.Error: Cannot be negative
                                                  //| 	at forcomp.testing$$anonfun$main$1.minus$1(forcomp.testing.scala:40)
                                                  //| 	at forcomp.testing$$anonfun$main$1$$anonfun$subtract$1$1.apply(forcomp.t
                                                  //| esting.scala:42)
                                                  //| 	at forcomp.testing$$anonfun$main$1$$anonfun$subtract$1$1.apply(forcomp.t
                                                  //| esting.scala:42)
                                                  //| 	at scala.collection.LinearSeqOptimized$class.foldLeft(LinearSeqOptimized
                                                  //| .scala:111)
                                                  //| 	at scala.collection.immutable.List.foldLeft(List.scala:76)
                                                  //| 	at forcomp.testing$$anonfun$main$1.subtract$1(forcomp.testing.scala:42)
                                                  //| 
                                                  //| 	at forcomp.testing$$anonfun$main$1.apply$mcV$sp(forcomp.testing.scala:47
                                                  //| )
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.sca
                                                  //| Output exceeds cutoff limit.

}
package forcomp


object Anagrams {

	/** A word is simply a `String`. */
	type Word = String

			/** A sentence is a `List` of words. */
			type Sentence = List[Word]

					/** `Occurrences` is a `List` of pairs of characters and positive integers saying
					 *  how often the character appears.
					 *  This list is sorted alphabetically w.r.t. to the character in each pair.
					 *  All characters in the occurrence list are lowercase.
					 *
					 *  Any list of pairs of lowercase characters and their frequency which is not sorted
					 *  is **not** an occurrence list.
					 *
					 *  Note: If the frequency of some character is zero, then that character should not be
					 *  in the list.
					 */
					type Occurrences = List[(Char, Int)]

							/** The dictionary is simply a sequence of words.
							 *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
							 */
							val dictionary: List[Word] = loadDictionary

							/** Converts the word into its character occurrence list.
							 *
							 *  Note: the uppercase and lowercase version of the character are treated as the
							 *  same character, and are represented as a lowercase character in the occurrence list.
							 *
							 *  Note: you must use `groupBy` to implement this method!
							 */
							def wordOccurrences(w: Word): Occurrences = w.toLowerCase().groupBy(_.toChar).mapValues(_.size).toList.sortBy(_._1);
					/** Converts a sentence into its character occurrence list. */
					def sentenceOccurrences(s: Sentence): Occurrences =wordOccurrences(s.foldLeft("")(_++_));

					/** The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
					 *  the words that have that occurrence count.
					 *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
					 *
					 *  For example, the word "eat" has the following character occurrence list:
					 *
					 *     `List(('a', 1), ('e', 1), ('t', 1))`
					 *
					 *  Incidentally, so do the words "ate" and "tea".
					 *
					 *  This means that the `dictionaryByOccurrences` map will contain an entry:
					 *
					 *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
					 *
					 */
					lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences(_));

					/** Returns all the anagrams of a given word. */
					def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences.getOrElse(wordOccurrences(word),List());

					/** Returns the list of all subsets of the occurrence list.
					 *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
					 *  is a subset of `List(('k', 1), ('o', 1))`.
					 *  It also include the empty subset `List()`.
					 *
					 *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
					 *
					 *    List(
					 *      List(),
					 *      List(('a', 1)),
					 *      List(('a', 2)),
					 *      List(('b', 1)),
					 *      List(('a', 1), ('b', 1)),
					 *      List(('a', 2), ('b', 1)),
					 *      List(('b', 2)),
					 *      List(('a', 1), ('b', 2)),
					 *      List(('a', 2), ('b', 2))
					 *    )
					 *
					 *  Note that the order of the occurrence list subsets does not matter -- the subsets
					 *  in the example above could have been displayed in some other order.
					 */
					def tempFunc1(a : (Char, Int)):List[Occurrences]={
						var list :List[Occurrences]  = List();
					for(i<-1 to a._2){
						var temp = List((a._1,i));
						list=temp::list;
					}
					return List()::list; 
					}
					def tempFunc2(o1 : List[List[(Char,Int)]], o2: List[List[(Char,Int)]]):List[Occurrences]={
						var list : List[Occurrences]=List();
					for(i<-0 to o1.size-1){
						for(j<-0 to o2.size-1){
							var temp : Occurrences = o1(i)++o2(j);
						list = temp::list;
						}
					}
					list

					}


					//  def autoTemp (o : Occurrences) : List[Occurrences]={
					//    if(o.size>1){
					//      
					//    }
					//    else{
					//      return 
					//    }
					//  }
					def combinations(occurrences: Occurrences): List[Occurrences] = {
						if(occurrences.isEmpty){
							return List(List());
						}
						if(occurrences.size<2){
							return tempFunc1(occurrences(0));
						}
						else{
							return tempFunc2(tempFunc1(occurrences.head),combinations(occurrences.tail));
						}
					}

					/** Subtracts occurrence list `y` from occurrence list `x`.
					 *
					 *  The precondition is that the occurrence list `y` is a subset of
					 *  the occurrence list `x` -- any character appearing in `y` must
					 *  appear in `x`, and its frequency in `y` must be smaller or equal
					 *  than its frequency in `x`.
					 *
					 *  Note: the resulting value is an occurrence - meaning it is sorted
					 *  and has no zero-entries.
					 */

					def subtract(x: Occurrences, y: Occurrences): Occurrences = {
						if(x.isEmpty)
							return x;
						var m1= x.toMap;
						var m2= y.toMap;

						var r = m2.foldLeft(m1){(a,b)=>{
							var temp = m1.apply(b._1);
							var value = temp-b._2;
							if(value<=0){
								a-b._1;
							}
							else{
								a-b._1;
								a.updated(b._1, value);
							}


						}
						}
						r.toList.sortBy(_._1);
					}

					/** Returns a list of all anagram sentences of the given sentence.
					 *
					 *  An anagram of a sentence is formed by taking the occurrences of all the characters of
					 *  all the words in the sentence, and producing all possible combinations of words with those characters,
					 *  such that the words have to be from the dictionary.
					 *
					 *  The number of words in the sentence and its anagrams does not have to correspond.
					 *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
					 *
					 *  Also, two sentences with the same words but in a different order are considered two different anagrams.
					 *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
					 *  `List("I", "love", "you")`.
					 *
					 *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
					 *
					 *    List(
					 *      List(en, as, my),
					 *      List(en, my, as),
					 *      List(man, yes),
					 *      List(men, say),
					 *      List(as, en, my),
					 *      List(as, my, en),
					 *      List(sane, my),
					 *      List(Sean, my),
					 *      List(my, en, as),
					 *      List(my, as, en),
					 *      List(my, sane),
					 *      List(my, Sean),
					 *      List(say, men),
					 *      List(yes, man)
					 *    )
					 *
					 *  The different sentences do not have to be output in the order shown above - any order is fine as long as
					 *  all the anagrams are there. Every returned word has to exist in the dictionary.
					 *
					 *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
					 *  so it has to be returned in this list.
					 *
					 *  Note: There is only one anagram of an empty sentence.
					 */

					def sentenceAnagrams(sentence: Sentence): List[Sentence] ={
						var occ : Occurrences = sentenceOccurrences(sentence);
						var r = subSetAnalysis(occ);
						if(r.isEmpty){
						  return List(Nil);
						}
						else{
						  System.out.println(r.size)
						  return r;
						}
					}
					
	

					def subSetAnalysis (  origin : Occurrences) : List[Sentence]={
						var combs : List[Occurrences] = combinations(origin);
					var result : List[List[Word]]=List();
					//if(combs.size!=1){
						for(comb <- combs){
							var remaining = subtract(origin, comb);
							//System.out.println("combination : " + comb.toString() + "    remaining : "+remaining.toString() );
							var words = dictionaryByOccurrences.getOrElse(comb,List());
						//	System.out.println(words);
							if(!words.isEmpty){
								if(remaining.size!=0){
									var remainWords = subSetAnalysis(remaining);
									if(!remainWords.isEmpty){
										result= tempWordCombination(words,remainWords).++(result);
									}
								}
								else{
									result = tempWordCombination(words,List()).++(result);
								}
							}
						}

					//}
					//System.out.println(result.toString());
					return result;
					}

					def tempWordCombination(list1 : List[Word], list2 : List[List[Word]]) : List[List[Word]]={
						var result : List[List[Word]] = List();
					for(word1 <- list1){
						var temp = word1::List();
						if(!list2.isEmpty){
							for(wordList <- list2){
								temp = word1::wordList;
								result = temp::result;
							}
						}
						else{
							result = temp::result;
						}

					}
					return result;
					}


}

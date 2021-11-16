package fr.esiee.fpic.labs:
    import scala.util.Random
    import scala.io.StdIn.readLine

    def clear() = print("\u001b[2J")

    class Word(val str:String=""){
        val index = for (i<-0 to str.length-1) yield i
        val letterOrder = for (i<-index) yield (str(i),i) 
        //index.foldLeft(List[(Char,Int)]()) ((xs:List[(Char, Int)], x:Int)=> xs:::List((str(x),x)))
        val letters = for (i<-index) yield (str(i),-1) 
        //index.foldLeft(List[(Char,Int)]()) ((xs:List[(Char, Int)], x:Int)=> xs:::List((str(x),-1)))
        val mask = (for (i<-index) yield (if i==0 then str(i) else "_")).foldLeft("")(_+_)
        //index.foldLeft(List[String]()) ((xs:List[Matchable], x:Int)=> if x==0 then xs:::List(str(x)) else xs:::List('_')).foldLeft("")(_+_)

        override def toString: String = str

        def isInclude(toInclude:Set[(Char, Int)]): Boolean =
            val wordSet = letterOrder.toSet.union(letters.toSet)
            toInclude.diff(wordSet)==Set()

        def isExclude(toExclude:Set[(Char, Int)]): Boolean= 
            val wordSet = letterOrder.toSet.union(letters.toSet)
            toExclude.diff(wordSet)==toExclude
    }

    class Dict(fromFile: String="", fromList: List[Word]=List(), enc:String = "UTF-8"){
        val it = if fromFile!="" then scala.io.Source.fromFile(fromFile, enc).getLines.toList else List() 
        val all_words = if it!=List() then (for (w<-it) yield Word(w)).toList else fromList

        def selectWords(size:Int=0, toInclude:Set[(Char, Int)]=Set(), toExclude:Set[(Char, Int)]=Set()): Dict = 
            val sizeFilter = size match {
                case n:Int if n>0 => Dict(fromList=(for (w <- all_words; if w.str.length == size) yield w).toList)
                case _ => this
            }
            
            val includeFilter = toInclude match {
                case s:Set[(Char, Int)]  if s!=Set() => Dict(fromList=(for (w <- sizeFilter.all_words; if w.isInclude(toInclude)) yield w).toList)
                case _ => sizeFilter
            }

            toExclude match {
                case s:Set[(Char, Int)]  if s!=Set() => Dict(fromList=(for (w <- includeFilter.all_words; if w.isExclude(toExclude)) yield w).toList)
                case _ => includeFilter
            }

        def getRandomWord(n:Int=7, toInclude:Set[(Char, Int)]=Set(), toExclude:Set[(Char, Int)]=Set()): Word = 
            val random = new Random
            val filter = selectWords(n, toInclude, toExclude).all_words
            filter.length  match {
                case v:Int if v>0 => filter(random.nextInt(filter.length)) 
                case _ => Word()
            }

        def isValidWord(myWord:String): Boolean =
            all_words.find(_.str==myWord) match {
                case Some(_) => true
                case None => false
            }

        def getSpecificWord(str:String): Word =
            isValidWord(str) match
                case true => (for (w<-all_words ; if w.str==str) yield w).head
                case false => Word()

        override def toString : String = s"Dict(${all_words.length})"
    }

    class Proposal(val answer:Word, val correct:Word){
        val color = this.evalProposal()
        val coloredAnswer = for (i<-answer.index) yield(s"${color(i)}${answer.str(i)}")
        //answer.index.foldLeft(List[String]()) ((xs:List[String], x:Int)=> xs:::List(s"${color(x)}${answer.str(x)}"))
        val isCorrect = answer.str == correct.str
        val goodLetters = for (i<-answer.index ; if color(i)!=Console.BLUE) yield(if color(i)==Console.RED then answer.letterOrder(i) else answer.letters(i))
        //answer.index.foldLeft(List[(Char,Int)]()) ((xs:List[(Char,Int)], x:Int)=> if color(x)==Console.RED then xs:::List(answer.letterOrder(x)) else if colox(x)==Console.YELLOW then xs:::List(answer.letters(x)) else xs)
        val badLetters = for (i<-answer.index ; if color(i)!=Console.RED) yield(if !(correct.str.contains(answer.str(i))) then answer.letters(i) else answer.letterOrder(i))
        //answer.index.foldLeft(List[(Char,Int)]()) ((xs:List[(Char,Int)], x:Int)=> if color(x)==Console.RED then xs else if !(correct.str.contains(answer.str(x))) then xs:::List(answer.letters(x)) else xs:::List(answer.letterOrder(x)))

        def evalProposal():List[String] = 
            answer.index.foldLeft(List[String]())((xs:List[String], x:Int)=> xs:::List(this.setColor(x)))

        def setColor(i:Int):String = 
            val letter = answer.str(i)
            val countLetter = (s:String) => s.count(_==letter)
            if letter == correct.str(i) then Console.RED 
            else if correct.str.contains(letter) then
                val sameLetters = for (j <- answer.index; if answer.str(j)==letter & (answer.str(j)==correct.str(j) | j<i)) yield j
                //answer.index.foldLeft(List[Int]()) ((xs:List[Int], j:Int)=> if answer.str(j)==letter & (answer.str(j)==correct.str(j) | j<i) then xs:::List(j) else xs)
                if countLetter(correct.str)-sameLetters.length<=0 then Console.BLUE else Console.YELLOW 
            else Console.BLUE

        override def toString: String =
            coloredAnswer.toList.foldLeft("")(_+_)+Console.RESET
    }

    def make_proposal(retry:Int, player:Map[String,Dict], dict_machine:Dict, correct:Word): Unit =
        val answer = retry match {
            case r: Int if r>=0 => readLine("Votre proposition ? ").toUpperCase()
            case _ => "Vous avez perdu."
        }

        if answer=="Vous avez perdu." then 
            println(s"Tentative maximal atteinte. Le mot est ${correct.str}. DEFAITE...")
            return()
        else if player("dico").isValidWord(answer)==false then 
            println(s"${answer} n'est pas dans le dictionnaire du jeu. Entrer autre chose.")
            make_proposal(retry, player, dict_machine, correct)
        else if answer.length != correct.str.length then
            println(s"${answer} n'est pas de ${correct.str.length} lettres. Entrer un mot de cette longueur.") 
            make_proposal(retry, player, dict_machine, correct)
        else 
            val proposal =  Proposal(player("dico").getSpecificWord(answer), correct)
            proposal.isCorrect match
                case true => println(s"Le mot est bien ${proposal}. VICTOIRE!!!")
                    return()
                case false => 
                    val proposal2 = Proposal(dict_machine.getRandomWord(), correct)
                    proposal2.isCorrect match {
                        case true => println(s"La proposition ${proposal2} de l'ordinateur est correcte. DEFAITE...")
                            return()
                        case false =>
                            val update2 = dict_machine.selectWords(toInclude=proposal2.goodLetters.toSet, toExclude=proposal2.badLetters.toSet)
                            clear()
                            println(proposal2)
                            val update = Map("dico"->player("dico"), "answer"->Dict(fromList=player("answer").all_words:::List(proposal.answer)))
                            val histo = for (w<-update("answer").all_words) yield Proposal(w,correct)
                            //update("answer").all_words.foldLeft(List[Word]()) ((xs:List[Object], w:Word)=> xs:::List(Proposal(w,correct)))
                            println(s"Encore ${retry-1} essais.")
                            histo foreach println
                            make_proposal(retry-1, update, update2, correct) 
                    }  

    val main_dict = Dict("ods8.txt")
    @main
    def start_game() = 
        val retry = 7
        val word_size = 7
        val solution = main_dict.getRandomWord(word_size)
        val human = Map("dico"->main_dict,"answer"->Dict())
        val machine = main_dict.selectWords(word_size, solution.letterOrder.slice(0,1).toSet)
        println(s"Mot de ${solution.str.length} lettres : ${solution.mask}")
        make_proposal(retry, human, machine, solution)

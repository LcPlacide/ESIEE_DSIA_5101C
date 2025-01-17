import scala.util.Random
import scala.io.StdIn.readLine
import scala.annotation.tailrec
import math.max

class Word(val str:String=""){
    lazy val length = str.length
    lazy val index = (0 to length-1).toList
    lazy val (letterOrder, letters, mask) = index.foldLeft((List(), List(), "")) 
        ( (t:(List[(Char, Int)], List[(Char, Int)], String), x:Int) => 
            ( t(0):::List((str(x),x)), t(1):::List((str(x),-1)), if x==0 then t(2)+str(x) else t(2)+"*" ) )

    override def toString: String = str

    def isInclude(toInclude:Set[(Char, Int)]): Boolean =
        val wordSet = (letterOrder:::letters).toSet
        toInclude.diff(wordSet)==Set()

    def isExclude(toExclude:Set[(Char, Int)]): Boolean= 
        val wordSet = (letterOrder:::letters).toSet
        toExclude.diff(wordSet)==toExclude

    def contains(letters:Set[(Char, Int, String)]): Boolean =
        letters.foldLeft(true) ( (occ:Boolean, letter:(Char, Int, String)) => 
            if letter(2).equals(">=") then occ&(str.count(_==letter(0))>=letter(1)) else occ&(str.count(_==letter(0))<letter(1)) )

}


class Dict(fromFile: String="", fromList: List[Word]=List(), enc:String = "UTF-8"){
    val it = if fromFile!="" then scala.io.Source.fromFile(fromFile, enc).getLines.toList else List() 
    val allWords = if it!=List() then it.map(Word(_)).toList else fromList
    val size = allWords.length

    def addWord(toAdd:Word): Dict = 
        return Dict(fromList=allWords:::List(toAdd), enc=enc)

    def selectWords(size:Int=0, toInclude:Set[(Char, Int)]=Set(), toExclude:Set[(Char, Int)]=Set(), howMany:Set[(Char, Int, String)]=Set()): Dict = 
        val sizeFilter = size match {
            case n:Int if n>0 => Dict(fromList=(for (w <- allWords; if w.length == size) yield w).toList)
            case _ => this
        }
            
        val includeFilter = toInclude match {
            case s:Set[(Char, Int)]  if s!=Set() => Dict(fromList=(for (w <- sizeFilter.allWords; if w.isInclude(toInclude)) yield w).toList)
            case _ => sizeFilter
        }

        val excludeFilter = toExclude match {
            case s:Set[(Char, Int)]  if s!=Set() => Dict(fromList=(for (w <- includeFilter.allWords; if w.isExclude(toExclude)) yield w).toList)
            case _ => includeFilter
        }

        howMany match {
            case s:Set[(Char, Int, String)]  if s!=Set() => Dict(fromList=(for (w <- excludeFilter.allWords; if w.contains(howMany)) yield w).toList)
            case _ => excludeFilter
        }

    def getRandomWord(n:Int=0, toInclude:Set[(Char, Int)]=Set(), toExclude:Set[(Char, Int)]=Set(), howMany:Set[(Char, Int, String)]=Set()): Word = 
        val random = new Random
        val filter = selectWords(n, toInclude, toExclude, howMany).allWords
        filter.length  match
            case v:Int if v>0 => filter(random.nextInt(filter.length)) 
            case _ => Word()

    def isValidWord(myWord:String): Boolean =
        allWords.find(_.str==myWord) match
            case Some(_) => true
            case None => false

    def getSpecificWord(str:String): Word =
        isValidWord(str) match
            case true => (for (w<-allWords ; if w.str==str) yield w).head
            case false => Word()

    override def toString : String = s"Dict(${size})"
}


class Proposal(val answer:Word, val correct:Word, hide:Boolean=false){
    val color = this.evalProposal()
    val coloredAnswer = answer.index.foldLeft("") ( (cs:String, x:Int) => s"${cs+color(x)}${if hide then answer.mask(x) else answer.str(x)}" )          
    val isCorrect = answer.str == correct.str
    lazy val goodLetters = for (i<-answer.index ; if color(i)!=Console.BLUE) yield(if color(i)==Console.RED then answer.letterOrder(i) else answer.letters(i))
    lazy val minLetterCount = for (t1<-goodLetters ; if t1(1)==(-1)) yield (t1(0), (for (t2<-goodLetters ; if t2(0)==t1(0)) yield t2).length, ">=")
    lazy val maxLetterCount = for (t1<-minLetterCount ; maxCount = (for (t2<-answer.letterOrder ; if t2(0)==t1(0)) yield t2).length ; if maxCount>t1(1)) yield (t1(0), maxCount, "<")
    lazy val countMisplacedLetters = for (t<-minLetterCount++maxLetterCount ; if t(1)>1) yield t
    lazy val badLetters = answer.index.foldLeft(List()) ( (xs:List[(Char,Int)], x:Int) => 
                                                            if color(x)==Console.RED then xs 
                                                            else if !(correct.str.contains(answer.str(x))) then xs:::List(answer.letters(x)) 
                                                            else if (for (o<-answer.letterOrder ; if o(0)==answer.str(x) && color(o(1))==Console.YELLOW) yield(o)).length==0 then
                                                                xs:::(for (i<-answer.index ; if color(i)!=Console.RED) yield(answer.str(x),i)).toList
                                                            else xs:::List(answer.letterOrder(x)) )

    def evalProposal():List[String] = 
        answer.index.foldLeft(List[String]())((xs:List[String], x:Int)=> xs:::List(this.setColor(x)))

    def setColor(i:Int):String = 
        val letter = answer.str(i)
        val countLetter = (s:String) => s.count(_==letter)
        if letter == correct.str(i) then Console.RED 
        else if correct.str.contains(letter) then
            val sameLetters = for (j <- answer.index; if answer.str(j)==letter & (answer.str(j)==correct.str(j) | j<i)) yield j
            if countLetter(correct.str)-sameLetters.length<=0 then Console.BLUE else Console.YELLOW 
        else Console.BLUE

    override def toString: String = coloredAnswer+Console.RESET
}


def clear() = print("\u001b[2J\n")

@tailrec
def make_proposal(retry:Int, playerAnswers:Map[String,Dict], machineAnswers:Map[String,Dict], correct:Word, hideMachine:Boolean=true): Unit =
    val currentPlayerAnswer = retry match {
        case r: Int if r>0 => readLine(s"Your proposal ${(playerAnswers("last").size+1)} on ${(playerAnswers("last").size+retry)} ? ").toUpperCase()
        case _ => "You lost."
    }

    if currentPlayerAnswer=="You lost." then 
        println(s"No more tries available. The word was ${correct.str}. YOU LOST...\n\n")
        return()
    else if playerAnswers("next").isValidWord(currentPlayerAnswer)==false then 
        println(s"${currentPlayerAnswer} is not in the dictionary of the game. Enter something else.")
        make_proposal(retry, playerAnswers, machineAnswers, correct, hideMachine)
    else if currentPlayerAnswer.length != correct.length then
        println(s"${currentPlayerAnswer} is too long or too short (${correct.length}-letter word required). Enter something else.") 
        make_proposal(retry, playerAnswers, machineAnswers, correct, hideMachine)
    else 
        val playerProposal =  Proposal(playerAnswers("next").getSpecificWord(currentPlayerAnswer), correct)
        playerProposal.isCorrect match
            case true => println(s"\n\nYou found the good word ${playerProposal}. YOU WIN!!!\n\n")
                return()
            case false => 
                val machineProposal = Proposal(machineAnswers("next").getRandomWord(), correct)
                machineProposal.isCorrect match 
                    case true => println(s"\n\nThe AI found the good word ${machineProposal}. YOU LOST...\n\n")
                        return()
                    case false =>
                        val machineUpdate = Map("next"->machineAnswers("next").selectWords(toInclude=machineProposal.goodLetters.toSet, 
                                                                                            toExclude=machineProposal.badLetters.toSet,
                                                                                            howMany=machineProposal.countMisplacedLetters.toSet),
                                                "last"->machineAnswers("last").addWord(machineProposal.answer))
                        val playerUpdate = Map("next"->playerAnswers("next"), 
                                               "last"->playerAnswers("last").addWord(playerProposal.answer))
                        clear()
                        val sep = max(machineProposal.answer.length, "PLAYER:".length)
                        (0 to machineUpdate("last").size-1).foldLeft(println(s"Previous proposals:\n\n   PLAYER:${" "*(3+max(0,sep-"PLAYER:".length))}MACHINE:")) ((xs:Unit, i:Int) => 
                            println(s"${i+1}: ${Proposal(playerUpdate("last").allWords(i),correct)}${" "*(3+max(0,sep-machineProposal.answer.length))}${Proposal(machineUpdate("last").allWords(i), correct, hideMachine)}"))
                        println("")
                        make_proposal(retry-1, playerUpdate, machineUpdate, correct, hideMachine) 


val mainDict = Dict("ods8.txt")
@main
def start_game() = 
    val retry = 6
    val wordSize = 7
    val hideMachine = false
    val solution = mainDict.getRandomWord(wordSize)
    val playerAnswers = Map("next"->mainDict, "last"->Dict())
    val machineAnswers = Map("next"->mainDict.selectWords(wordSize, Set(solution.letterOrder(0))), "last"->Dict())
    println(s"\nWe are looking for a ${solution.length}-letter word starting with : ${solution.mask}\n")
    make_proposal(retry, playerAnswers, machineAnswers, solution, hideMachine)
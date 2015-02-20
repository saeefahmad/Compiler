package edu.utsa.cs5363

import scala.io.Source
import scala.collection.mutable.Stack
import RegExMatch._
import ParsingTable._

object Parser {
  def parse (sourceCode: Array[String]): Unit = {
    var tokenStack = new Stack[String]
    var pointer = 0
    var terminal: String = ""
    val inputSourceCode: Array[String] = sourceCode :+ "$"
    var catchDerivation: Array[String] = Array()
    tokenStack.push("$")
    tokenStack.push(NonTerminal.startSymbol())
    terminal = RegExMatch.regExMatcher(inputSourceCode(pointer))
    var sourceLength: Int = inputSourceCode.length
    
    while (sourceLength > 0) {     
      if (tokenStack.head == "$" && terminal == "$" 
          && tokenStack.length == 1) {        
        Console.println("Program successfully compiled !!!")
        return
      }
      
      if (isNonTerminal(tokenStack.head) == "<NT>") {
        catchDerivation = NonTerminal.deriveNonTerminal(tokenStack.head, terminal)
        if (catchDerivation == null) {
          tokenStack.pop()
        } else if (catchDerivation.length > 0) {
          tokenStack.pop()
          var i: Int = catchDerivation.length
          while(i > 0) {
            tokenStack.push(catchDerivation(i-1))
            i = i - 1
          }
        } else {
          throw SyntaxError("Parse Error !!!")
        }  
      } else if (terminal == tokenStack.head) {
        tokenStack.pop()
        pointer = pointer + 1
        terminal = RegExMatch.regExMatcher(inputSourceCode(pointer))
      } else {
        throw SyntaxError("Parse Error !!!")
      }      
    }
  }
  
  def makeParseTree (sourceCode: Array[String], fileName: String): Unit = {
    var inputStack = new Stack[String]
    var inputStackRaw = new Stack[String]
    var i = sourceCode.length
    while (i > 0) {
      inputStack.push(RegExMatch.regExMatcher(sourceCode(i - 1)))
      inputStackRaw.push(sourceCode(i - 1))
      i = i - 1
    }
    var tree = ParseTree.parseTree(inputStack, fileName, inputStackRaw)
    Console.println("Parse tree generated successfully !!!")
  }
  
  def isNonTerminal (str: String): String = {
    return RegExMatch.regExNonTermCheck(str)
  }
}
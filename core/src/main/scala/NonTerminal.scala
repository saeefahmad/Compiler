package edu.utsa.cs5363
import scala.io.Source
import ParsingTable._

object NonTerminal {
  val program = "<program>"
  val declarations = "<declarations>" 
  val identType = "<type>"
  val statementSequence = "<statementSequence>"
  val statement = "<statement>"
  val assignment = "<assignment>"
  val assignmentPrime = "<assignmentPrime>"
  val ifStatement = "<ifStatement>"
  val elseClause = "<elseClause>"
  val whileStatement = "<whileStatement>"
  val writeInt = "<writeInt>"  
  val expression = "<expression>"
  val expressionPrime = "<expressionPrime>"
  val simpleExpression = "<simpleExpression>"
  val simpleExpressionPrime = "<simpleExpressionPrime>"
  val term = "<term>"  
  val termPrime = "<termPrime>"
  val factor = "<factor>"
    
  def deriveNonTerminal (str: String, inputStr: String): Array[String] = str match {    
    case `program` => ParsingTable.program(inputStr)
    case `declarations` => ParsingTable.declarations(inputStr)
    case `identType` => ParsingTable.identType(inputStr)
    case `statementSequence` => ParsingTable.statementSequence(inputStr)
    case `statement` => ParsingTable.statement(inputStr)
    case `assignment` => ParsingTable.assignment(inputStr)
    case `assignmentPrime` => ParsingTable.assignmentPrime(inputStr)
    case `ifStatement` => ParsingTable.ifStatement(inputStr)
    case `elseClause` => ParsingTable.elseClause(inputStr)
    case `whileStatement` => ParsingTable.whileStatement(inputStr)
    case `writeInt` => ParsingTable.writeInt(inputStr)
    case `expression` => ParsingTable.expression(inputStr)
    case `expressionPrime` => ParsingTable.expressionPrime(inputStr)
    case `simpleExpression` => ParsingTable.simpleExpression(inputStr)
    case `simpleExpressionPrime` => ParsingTable.simpleExpressionPrime(inputStr)
    case `term` => ParsingTable.term(inputStr)
    case `termPrime` => ParsingTable.termPrime(inputStr)
    case `factor` => ParsingTable.factor(inputStr)
    case default => throw SyntaxError("Parse Error !!!")  
  } 
  
  def deriveTerminal (str: String, inputStr: String): Array[String] = str match {    
    case `program` => ParsingTable.program(inputStr)
    case `declarations` => ParsingTable.declarations(inputStr)
    case `identType` => ParsingTable.identType(inputStr)
    case `statementSequence` => ParsingTable.statementSequence(inputStr)
    case `statement` => ParsingTable.statement(inputStr)
    case `assignment` => ParsingTable.assignment(inputStr)
    case `assignmentPrime` => ParsingTable.assignmentPrime(inputStr)
    case `ifStatement` => ParsingTable.ifStatement(inputStr)
    case `elseClause` => ParsingTable.elseClause(inputStr)
    case `whileStatement` => ParsingTable.whileStatement(inputStr)
    case `writeInt` => ParsingTable.writeInt(inputStr)
    case `expression` => ParsingTable.expression(inputStr)
    case `expressionPrime` => ParsingTable.expressionPrime(inputStr)
    case `simpleExpression` => ParsingTable.simpleExpression(inputStr)
    case `simpleExpressionPrime` => ParsingTable.simpleExpressionPrime(inputStr)
    case `term` => ParsingTable.term(inputStr)
    case `termPrime` => ParsingTable.termPrime(inputStr)
    case `factor` => ParsingTable.factor(inputStr)
    case default => throw SyntaxError("Parse Error !!!")  
  } 
  
  def startSymbol (): String = {
    return program
  }

}

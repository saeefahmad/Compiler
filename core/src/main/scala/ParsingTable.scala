package edu.utsa.cs5363

import scala.io.Source
import RegExMatch._

sealed abstract class Token

object ParsingTable {
  def parsingTable (nonTerm: String, input: String): Unit = {
    // do nothing 
  }
  
  // Production 1
  def program (input: String): Array[String] = {
    var product = Array("PROGRAM", "<declarations>", "BEGIN", "<statementSequence>", "END")
    input match {
      case "PROGRAM" => return product
      case default => throw SyntaxError("Parse Error !!!")       
    }
  }
    
  // Production 2
  def declarations (input: String): Array[String] = {
    var productVar = Array("VAR", "ident", "AS", "<type>", "SC", "<declarations>")
    var productEp = null
    input match {
      case "VAR" => return productVar
      case "BEGIN" => return null
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 3
  def identType (input: String): Array[String] = {
    var productInt = Array("INT")
    var productBool = Array("BOOL")
    input match {
      case "INT" => return productInt
      case "BOOL" => return productBool
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 4
  def statementSequence (input: String): Array[String] = {
    var product = Array("<statement>", "SC", "<statementSequence>")
    var productEp = null
    input match {
      case "END" => return productEp
      case "ident" => return product
      case "IF" => return product
      case "ELSE" => return productEp
      case "WHILE" => return product
      case "WRITEINT" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 5
  def statement (input: String): Array[String] = {
    var productAsgn = Array("<assignment>")
    var productIfSt = Array("<ifStatement>")
    var productWhileSt = Array("<whileStatement>")
    var productWriteInt = Array("<writeInt>")
    input match {
      case "ident" => return productAsgn
      case "IF" => return productIfSt
      case "WHILE" => return productWhileSt
      case "WRITEINT" => return productWriteInt
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 6
  def assignment (input: String): Array[String] = {
    var product = Array("ident", "ASGN", "<assignmentPrime>")
    input match {
      case "ident" => return product
      case default => throw SyntaxError("Parse Error !!!")      
    }
  }
    
  // Production 7
  def assignmentPrime (input: String): Array[String] = {
    var productExpr = Array("<expression>")
    var productReadInt = Array("READINT")
    input match {
      case "num" => return productExpr
      case "LP" => return productExpr
      case "ident" => return productExpr
      case "READINT" => return productReadInt
      case "boollit" => return productExpr
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 8
  def ifStatement (input: String): Array[String] = {
    var product = Array("IF", "<expression>", "THEN", "<statementSequence>", "<elseClause>", "END")
    input match {
      case "IF" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 9
  def elseClause (input: String): Array[String] = {
    var product = Array("ELSE", "<statementSequence>")
    var productEp = null
    input match {
      case "END" => return productEp
      case "ELSE" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 10
  def whileStatement (input: String): Array[String] = {
    var product = Array("WHILE", "<expression>", "DO", "<statementSequence>", "END")
    input match {
      case "WHILE" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 11
  def writeInt (input: String): Array[String] = {
    var product = Array("WRITEINT", "<expression>")
    input match {
      case "WRITEINT" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 12
  def expression (input: String): Array[String] = {
    var product = Array("<simpleExpression>", "<expressionPrime>")
    input match {
      case "num" => return product
      case "LP" => return product
      case "ident" => return product
      case "boollit" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 13
  def expressionPrime (input: String): Array[String] = {
    var product = Array("COMPARE", "<expression>")
    var productEp = null
    input match {
      case "COMPARE" => return product
      case "RP" => return productEp
      case "SC" => return productEp
      case "THEN" => return productEp
      case "DO" => return productEp
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 14
  def simpleExpression (input: String): Array[String] = {
    var product = Array("<term>", "<simpleExpressionPrime>")
    input match {
      case "num" => return product
      case "LP" => return product
      case "ident" => return product
      case "boollit" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 15
  def simpleExpressionPrime (input: String): Array[String] = {
    var product = Array("ADDITIVE", "<simpleExpression>")
    var productEp = null
    input match {
      case "ADDITIVE" => return product
      case "COMPARE" => return productEp
      case "RP" => return productEp
      case "SC" => return productEp
      case "THEN" => return productEp
      case "DO" => return productEp
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 16
  def term (input: String): Array[String] = {
    var product = Array("<factor>", "<termPrime>")
    input match {
      case "num" => return product
      case "LP" => return product
      case "ident" => return product
      case "boollit" => return product
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 17
  def termPrime (input: String): Array[String] = {
    var product = Array("MULTIPLICATIVE", "<term>")
    var productEp = null
    input match {
      case "MULTIPLICATIVE" => return product
      case "ADDITIVE" => return productEp
      case "COMPARE" => return productEp
      case "RP" => return productEp
      case "SC" => return productEp
      case "THEN" => return productEp
      case "DO" => return productEp
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 18
  def factor (input: String): Array[String] = {
    var productIdent = Array("ident")
    var productNum = Array("num")
    var productBoolit = Array("boollit")
    var productExpr = Array("LP", "<expression>", "RP")
    input match {
      case "num" => return productNum
      case "LP" => return productExpr
      case "ident" => return productIdent
      case "boollit" => return productBoolit
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  


}

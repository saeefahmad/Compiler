package edu.utsa.cs5363
import scala.io.Source
import scala.util.matching.Regex

object RegExMatch {
  val numPattern = "[1-9][0-9]*|0".r
  val boolPattern = "false|true".r
  val identPattern = "[A-Z][A-Z0-9]*".r
  val multiplicativePattern = "[*]|div|mod".r
  val additivePattern = "[+-]".r
  val comparisonPattern = "[=]|[!][=]|[<>]|[<][=]|[>][=]".r
  val assignmentPattern = "[:][=]".r
  val semicolonPattern = "[;]".r
  val leftParenthPattern = "[(]".r
  val rightParenthPattern = "[)]".r
  val nonTerminalPattern = "^<.*".r
  
  def regExMatcher (str: String): String = str match {    
    case numPattern() => return "num"
    case boolPattern() => return "boollit"
    case identPattern() => return "ident"
    case multiplicativePattern() => return "MULTIPLICATIVE"
    case additivePattern() => return "ADDITIVE"
    case comparisonPattern() => return "COMPARE"
    case assignmentPattern() => return "ASGN"
    case leftParenthPattern() => return "LP"
    case rightParenthPattern() => return "RP"
    case semicolonPattern() => return "SC"
    
    case "if" => return "IF"
    case "then" => return "THEN"
    case "else" => return "ELSE"
    case "begin" => return "BEGIN"
    case "end" => return "END"
    case "while" => return "WHILE"
    case "do" => return "DO"
    case "program" => return "PROGRAM"
    case "var" => return "VAR"
    case "as" => return "AS"    
    case "int" => return "INT"
    case "bool" => return "BOOL"
    case "writeInt" => return "WRITEINT"
    case "readInt" => return "READINT"
    case "$" => return "$"
    case default => throw SyntaxError("Parse Error !!!")  
  }
  
  def regExNonTermCheck (str: String): String = {
    str match {
    case nonTerminalPattern() => 
      return "<NT>" 
    case default => return ""   
    }
  }

}

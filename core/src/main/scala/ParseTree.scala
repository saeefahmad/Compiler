package edu.utsa.cs5363

import scala.io.Source
import scala.collection.mutable._
import java.io


object ParseTree {
  var nIndex: Int = 1
  var inputStringStack = new Stack[String]  
  var inputStringRawStack = new Stack[String]
  var tree = new StringBuffer()
  
  val prog = "PROGRAM"
  val num = "num" 
  val lp = "LP"
  val rp = "RP"
  val begin = "BEGIN"
  val end = "END"
  val variable = "VAR"
  val ident = "ident"
  val as = "AS"
  val sc = "SC"
  val int = "INT"  
  val bool = "BOOL"
  val asgn = "ASGN"
  val readint = "READINT"
  val ifterm = "IF"
  val elseterm = "ELSE"  
  val whileterm = "WHILE"
  val doterm = "DO"
  val writeint = "WRITEINT"
  val compare = "COMPARE"
  val mult = "MULTIPLICATIVE"
  val add = "ADDITIVE"
  val boollit = "boollit"
  val then = "THEN"
  
  
  def parseTree (inputStack: Stack[String], fileName: String, inputStackRaw: Stack[String]): Unit = {
    nIndex = 1
    inputStringStack = inputStack
    inputStringRawStack = inputStackRaw
    tree = new StringBuffer()
    tree = parseTreeTemplate(tree, 1)
    generateTree()
    tree = parseTreeTemplate(tree, 0) 
    CoreCompiler.writeOutputFile(fileName, tree.toString())
  }
  
  def generateTree (): Unit = {
    program(nIndex)
  }
  
  // Production 1
  def program (nodeIndex: Int): Unit = {
    var product = Array("PROGRAM", "<declarations>", "BEGIN", "<statementSequence>", "END")
    inputStringStack.head match {
      case `prog` => {
        tree = addNodeInParseTree(tree, "<program>", nodeIndex)
        programterminal(nodeIndex)
        declarations(nodeIndex)
        beginterminal(nodeIndex)
        statementSequence(nodeIndex)
        endterminal(nodeIndex)
      }
      case default => throw SyntaxError("Parse Error !!!")       
    }
  }
  
  // Production 2
  def declarations (parentIndex: Int): Unit = {
    var productVar = Array("VAR", "ident", "AS", "<type>", "SC", "<declarations>")
    var productEp = Array("&#949;")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<declarations>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `variable` => {
        variableterminal(index)
        identterminal(index)
        asterminal(index)
        identType(index)
        scterminal(index)
        declarations(index)      
        
      }
      case `begin` => {
        epsilon(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 3
  def identType (parentIndex: Int): Unit = {
    var productInt = Array("INT")
    var productBool = Array("BOOL")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<type>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `int` => {
        intterminal(index)
      }
      case `bool` => {
        boolterminal(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 4
  def statementSequence (parentIndex: Int): Unit = {
    var product = Array("<statement>", "SC", "<statementSequence>")
    var productEp = Array("&#949;")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<statementSequence>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `end` => { epsilon(index) }
      case `ident` => {
        statement(index)
        scterminal(index)
        statementSequence(index)        
      }
      case `ifterm` => {
        statement(index)
        scterminal(index)
        statementSequence(index)        
      }
      case `elseterm` => { epsilon(index) }
      case `whileterm` => {
        statement(index)
        scterminal(index)
        statementSequence(index)        
      }
      case `writeint` => {
        statement(index)
        scterminal(index)
        statementSequence(index)        
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 5
  def statement (parentIndex: Int): Unit = {
    var productAsgn = Array("<assignment>")
    var productIfSt = Array("<ifStatement>")
    var productWhileSt = Array("<whileStatement>")
    var productWriteInt = Array("<writeInt>")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<statement>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `ident` => {
        assignment(index)
      } 
      case `ifterm` => {
        ifStatement(index)
      } 
      case `whileterm` => {
        whileStatement(index) 
      }
      case `writeint` => {
        writeInt(index)
      } 
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  // Production 6
  def assignment (parentIndex: Int): Unit = {
    var product = Array("ident", "ASGN", "<assignmentPrime>")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<assignment>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `ident` => {
        identterminal(index)
        asgnterminal(index)
        assignmentPrime(index)
      }
      case default => throw SyntaxError("Parse Error !!!")      
    }
  }
    
  // Production 7
  def assignmentPrime (parentIndex: Int): Unit = {
    var productExpr = Array("<expression>")
    var productReadInt = Array("READINT")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<assignmentPrime>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `num` => { expression(index) } 
      case `lp` => { expression(index) } 
      case `ident` => { expression(index) } 
      case `readint` => { readintterminal(index) } 
      case `boollit` => { expression(index) } 
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 8
  def ifStatement (parentIndex: Int): Unit = {
    var product = Array("IF", "<expression>", "THEN", "<statementSequence>", "<elseClause>", "END")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<ifStatement>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `ifterm` => {
        ifterminal(index)
        expression(index)
        thenterminal(index)
        statementSequence(index)
        elseClause(index)
        endterminal(index)
        
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 9
  def elseClause (parentIndex: Int): Unit = {
    var product = Array("ELSE", "<statementSequence>")
    var productEp = Array("&#949;")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<elseClause>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `end` => { epsilon(index) }
      case `elseterm` => {
      elseterminal(index)
      statementSequence(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 10
  def whileStatement (parentIndex: Int): Unit = {
    var product = Array("WHILE", "<expression>", "DO", "<statementSequence>", "END")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<whileStatement>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `whileterm` => {
        whileterminal(index)
        expression(index)
        doterminal(index)
        statementSequence(index)
        endterminal(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 11
  def writeInt (parentIndex: Int): Unit = {
    var product = Array("WRITEINT", "<expression>")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<writeInt>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `writeint` => {
        writeintterminal(index)
        expression(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 12
  def expression (parentIndex: Int): Unit = {
    var product = Array("<simpleExpression>", "<expressionPrime>")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<expression>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `num` => {
        simpleExpression(index)
        expressionPrime(index)
      }
      case `lp` => {
        simpleExpression(index)
        expressionPrime(index)
      }
      case `ident` => {
        simpleExpression(index)
        expressionPrime(index)
      }
      case `boollit` => {
        simpleExpression(index)
        expressionPrime(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 13
  def expressionPrime (parentIndex: Int): Unit = {
    var product = Array("COMPARE", "<expression>")
    var productEp = Array("&#949;")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<expressionPrime>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `compare` => {
        compareterminal(index)
        expression(index)
      }
      case `rp` => { epsilon(index) }
      case `sc` => { epsilon(index) }
      case `then` => { epsilon(index) }
      case `doterm` => { epsilon(index) }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 14
  def simpleExpression (parentIndex: Int): Unit = {
    var product = Array("<term>", "<simpleExpressionPrime>")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<simpleExpression>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `num` => {
        term(index)
        simpleExpressionPrime(index)
      }
      case `lp` => {
        term(index)
        simpleExpressionPrime(index)
      }
      case `ident` => {
        term(index)
        simpleExpressionPrime(index)
      }
      case `boollit` => {
        term(index)
        simpleExpressionPrime(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 15
  def simpleExpressionPrime (parentIndex: Int): Unit = {
    var product = Array("ADDITIVE", "<simpleExpression>")
    var productEp = Array("&#949;")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<simpleExpressionPrime>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `add` => {
        additiveterminal(index)
        simpleExpression(index)
      }
      case `compare` => { epsilon(index) }
      case `rp` => { epsilon(index) }
      case `sc` => { epsilon(index) }
      case `then` => { epsilon(index) }
      case `doterm` => { epsilon(index) }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 16
  def term (parentIndex: Int): Unit = {
    var product = Array("<factor>", "<termPrime>")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<term>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `num` => {
        factor(index)
        termPrime(index)
      }
      case `lp` => {
        factor(index)
        termPrime(index)
      }
      case `ident` => {
        factor(index)
        termPrime(index)
      }
      case `boollit` => {
        factor(index)
        termPrime(index)
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 17
  def termPrime (parentIndex: Int): Unit = {
    var product = Array("MULTIPLICATIVE", "<term>")
    var productEp = Array("&#949;")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<termPrime>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `mult` => {
        multiplicativeterminal(index)
        term(index)
      }
      case `add` => { epsilon(index) }
      case `compare` => { epsilon(index) }
      case `rp` => { epsilon(index) }
      case `sc` => { epsilon(index) }
      case `then` => { epsilon(index) }
      case `doterm` => { epsilon(index) }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // Production 18
  def factor (parentIndex: Int): Unit = {
    var productIdent = Array("ident")
    var productNum = Array("num")
    var productBoolit = Array("boollit")
    var productExpr = Array("LP", "<expression>", "RP")
    nIndex = nIndex + 1
    var index = nIndex
    tree = addNodeInParseTree(tree, "<factor>", index)
    tree = relNodeInParseTree(tree, index, parentIndex)
    inputStringStack.head match {
      case `num` => {
        numterminal(index)
      }
      case `lp` => {
        lpterminal(index)
        expression(index)
        rpterminal(index)
      }
      case `ident` => {
        identterminal(index)
      } 
      case `boollit` => {
        boollitterminal(index)
      } 
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  // terminals    
  def programterminal (parentIndex: Int) {
    inputStringStack.head match {
      case `prog` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, prog, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
    
  }
  
  def numterminal (parentIndex: Int) = {
    inputStringStack.head match {
      case `num` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, num+":"+inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  def lpterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `lp` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def rpterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `rp` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }  
  
  def beginterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `begin` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, begin, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def endterminal (parentIndex: Int) = {
    inputStringStack.head match {
      case `end` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, end, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  def variableterminal (parentIndex: Int) = {
    inputStringStack.head match {
      case `variable` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, variable, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def identterminal (parentIndex: Int) = {
    inputStringStack.head match {
      case `ident` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, ident+":"+inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  } 
  
  def asterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `as` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, as, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def scterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `sc` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def intterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `int` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, int, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }  
  
  def boolterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `bool` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, bool, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def asgnterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `asgn` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def readintterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `readint` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, readint, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def ifterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `ifterm` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, ifterm, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def elseterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `elseterm` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, elseterm, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }  
  
  def whileterminal (parentIndex: Int) = {
    inputStringStack.head match {
      case `whileterm` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, whileterm, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def doterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `doterm` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, doterm, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def writeintterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `writeint` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, writeint, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def compareterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `compare` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def multiplicativeterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `mult` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def additiveterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `add` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, inputStringRawStack.head, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def boollitterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `boollit` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, boollit, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def thenterminal(parentIndex: Int) = {
    inputStringStack.head match {
      case `then` => {
        nIndex = nIndex + 1
        tree = addNodeInParseTree(tree, then, nIndex)
        tree = relNodeInParseTree(tree, nIndex, parentIndex)
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Parse Error !!!")  
    }
  }
  
  def epsilon(parentIndex: Int) = {   
    nIndex = nIndex + 1
    tree = addNodeInParseTree(tree, "&#949;", nIndex)
    tree = relNodeInParseTree(tree, nIndex, parentIndex)
  } 
  
  def addNodeInParseTree(tree: StringBuffer, token: String, parseTreeIndex: Int): StringBuffer = {
    tree.append(" n"+parseTreeIndex + " [label=\""+token+"\",fillcolor=\"/x11/white\",shape=box]").append(System.getProperty("line.separator"));
    return tree
  }
  
  def relNodeInParseTree(tree: StringBuffer, parseTreeIndex: Int, parentNodeIndex: Int): StringBuffer = {
    tree.append(" n"+parentNodeIndex+" -> n"+parseTreeIndex).append(System.getProperty("line.separator"));
    return tree
  }
  
  def parseTreeTemplate(tree: StringBuffer, action: Int): StringBuffer = {
    action match {
      case 1 => { 
        tree.append("digraph parseTree {").append(System.getProperty("line.separator"))
        tree.append(" ordering=out;").append(System.getProperty("line.separator"))
        tree.append(" node [shape = box, style = filled];").append(System.getProperty("line.separator"))
        return tree
      } 
      case 0 => {
        tree.append("}").append(System.getProperty("line.separator"))
        return tree
      }
      case default => {
        throw InternalError("Some Internal Error occured !!!") 
      }      
    }
  }


}

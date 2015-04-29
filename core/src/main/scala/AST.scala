package edu.utsa.cs5363
import scala.io.Source
import scala.collection.mutable._
import java.io
import scala.collection.mutable.ArrayBuffer

object AST {  
  var nIndex: Int = 1
  var inputStringStack = new Stack[String]  
  var inputStringRawStack = new Stack[String]  
  var astNodeStack = new Stack[ASTNodes]
  var tree = new StringBuffer()
  var AST = new ASTNodes()
  var declaredVariables: ArrayBuffer[String] = ArrayBuffer[String]()
  var declaredVariableType: ArrayBuffer[String] = ArrayBuffer[String]()
  
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
  val procedure = "PROCEDURE"
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
  val function = "function"
  
  
  def ASTInit (inputStack: Stack[String], fileName: String, inputStackRaw: Stack[String]): Unit = {
    nIndex = 1
    inputStringStack = inputStack
    inputStringRawStack = inputStackRaw
    tree = new StringBuffer()
    tree = ASTTemplate(tree, 1)    
    generateTree()
    var asTree: ASTNodes = AST
    var a = AST
    tree = traverTreeDFS(asTree, tree)
    tree = ASTTemplate(tree, 0) 
    CoreCompiler.writeOutputFile(fileName, tree.toString()) 
  }
  
  def traverTreeDFS (asTree: ASTNodes, treeBuffer: StringBuffer): StringBuffer = { 
    var treeBuild = treeBuffer
    var ast: ASTNodes = asTree
    astNodeStack.push(ast)
    while (!astNodeStack.isEmpty) {
      if (!astNodeStack.head.getVisit()) {  
        treeBuild = addNodeInAST(treeBuild, astNodeStack.head)
        if(astNodeStack.head.getIndex() > 1) {
          treeBuild = relNodeInAST(treeBuild, astNodeStack.head)
        } else {
        }
        astNodeStack.head.setVisit(true)
      }
      if (astNodeStack.head.getUnVisitedChild() != null) {
        astNodeStack.push(astNodeStack.head.getUnVisitedChild())
      } else {
        astNodeStack.head.setVisit(true)
        astNodeStack.pop()
      }
    }
    
    declaredVariables = ArrayBuffer[String]()
    declaredVariableType = ArrayBuffer[String]()
    return treeBuild
  }
  
  def generateTree (): Unit = {
    AST = program(new ASTNodes())
  }
  
  def makeAST (sourceCode: Array[String], fileName: String): ASTNodes = {
    var inputStack = new Stack[String]
    var inputStackRaw = new Stack[String]
    var i = sourceCode.length
    while (i > 0) {
      inputStack.push(RegExMatch.regExMatcher(sourceCode(i - 1)))
      inputStackRaw.push(sourceCode(i - 1)) 
      i = i - 1
    }
    var tree = ASTInit(inputStack, fileName, inputStackRaw)    
    Console.println("AST generated successfully !!!")
    return AST
  }
  
  // Production 1
  def program (astNode: ASTNodes): ASTNodes = {
    var node = astNode
    node.ASTNodes("program")   
    node.addIndex(nIndex)    
    inputStringStack.head match {
      case `prog` => {
        programterminal()        
        var decl = new ASTNodes()
        decl.ASTNodes("decl list")
        nIndex = nIndex + 1
        decl.addIndex(nIndex)
        decl.addParent(node)
        var declChild = declarations(decl)
        if(declChild != null) {
          node.addChild(declChild)
        }                
        beginterminal()        
        var stmtlist = new ASTNodes()
        stmtlist.ASTNodes("stmt list")
        nIndex = nIndex + 1
        stmtlist.addIndex(nIndex)
       
        stmtlist.addParent(node)
        var stmtChild = statementSequence(stmtlist)
        if(stmtChild != null) {
          node.addChild(stmtChild)
          if(declChild.getTypeOk() == false || stmtChild.getTypeOk() == false){
            node.setType("ERROR")
            node.setTypeOk(false)
          } else {
            node.setTypeOk(true)
          }
        }         
        endterminal()
        return node
      }
      case default => throw SyntaxError("Syntax Error !!!")       
    }
  }
  
  // Production 2
  def declarations (astNode: ASTNodes): ASTNodes = {
    var node = astNode    
    inputStringStack.head match { 
      case `variable` => {
        variableterminal()        
        var iden = new ASTNodes()
        iden.ASTNodes("decl: "+inputStringRawStack.head)
        nIndex = nIndex + 1
        iden.addIndex(nIndex)
        iden.addParent(node)      
        if(searchTypeIndex(inputStringRawStack.head) != -1) {
          throw SyntaxError("Sematic Error: duplicate variable names exist !!!")  
        }
        declaredVariables += inputStringRawStack.head
        identterminal()                
        asterminal()        
        var idType = new ASTNodes()
        idType.ASTNodes(inputStringStack.head)
        nIndex = nIndex + 1
        idType.addIndex(nIndex)
        idType.addParent(iden)
        idType.setType(inputStringStack.head)
        iden.addChild(idType)
        node.addChild(iden)              
        identType(node)             
        node.setTypeOk(true)
        scterminal()
        var decl = declarations(node) 
        if(decl != null) {
          return decl
        } else {
          return node
        }  
      }
      case `begin` => {
        return null
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  // Production 3
  def identType (astNode: ASTNodes): ASTNodes = {
    var node = astNode
    inputStringStack.head match {
      case `int` => {
        declaredVariableType += inputStringStack.head
        intterminal()
        return node
      }
      case `bool` => {
        declaredVariableType += inputStringStack.head
        boolterminal()
        return node
      }
      case `procedure` => {
        declaredVariableType += inputStringStack.head
        procedureterminal()
        return node
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  // Production 4
  def statementSequence (astNode: ASTNodes): ASTNodes = {
    var node = astNode 
    inputStringStack.head match {
      case `end` => { return null }
      case `ident` => {
        var stNode = new ASTNodes()
        stNode.addParent(node)
        var stmt = statement(stNode) 
        if(stmt != null) {
          node.addChild(stmt)
          if (stmt.getType() == "BOOL") {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
          }         
        }        
        scterminal()
        var stmtsq = statementSequence(node)   
        if(stmtsq != null) {
          if (stmt.getType() == "BOOL" && stmtsq.getTypeOk() == true) {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
            node.setType("Error")
          }  
          return stmtsq
        } else {
          return node
        }
      }
      case `ifterm` => {
        var stNode = new ASTNodes()
        stNode.addParent(node)
        var stmt = statement(stNode) 
        if(stmt != null) {
          node.addChild(stmt)
          if (stmt.getType() == "BOOL") {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
          }  
        }        
        scterminal()
        var stmtsq = statementSequence(node)   
        if(stmtsq != null) {
          if (stmt.getType() == "BOOL" && stmtsq.getTypeOk() == true) {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
            node.setType("Error")
          }
          return stmtsq
        } else {
          return node
        }        
      }
      case `elseterm` => { return null }
      case `whileterm` => {
        var stNode = new ASTNodes()
        stNode.addParent(node)
        var stmt = statement(stNode) 
        if(stmt != null) {
          node.addChild(stmt)
          if (stmt.getType() == "BOOL") {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
          }  
        }        
        scterminal()
        var stmtsq = statementSequence(node)   
        if(stmtsq != null) {
          if (stmt.getType() == "BOOL" && stmtsq.getTypeOk() == true) {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
            node.setType("Error")
          }
          return stmtsq
        } else {
          return node
        }      
      }
      case `writeint` => {
        var stNode = new ASTNodes()
        stNode.addParent(node)
        var stmt = statement(stNode) 
        if(stmt != null) {
          node.addChild(stmt)
          if (stmt.getType() == "BOOL") {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
          }  
        }        
        scterminal()
        var stmtsq = statementSequence(node)   
        if(stmtsq != null) {
          if (stmt.getType() == "BOOL" && stmtsq.getTypeOk() == true) {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
            node.setType("Error")
          }
          return stmtsq
        } else {
          return node
        }
      }
      case `function` => { // here we go
        var stNode = new ASTNodes()
        stNode.addParent(node)
        var stmt = statement(stNode) 
        if(stmt != null) {
          node.addChild(stmt)
          if (stmt.getType() == "BOOL") {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
          }  
        }        
        scterminal()
        var stmtsq = statementSequence(node)   
        if(stmtsq != null) {
          if (stmt.getType() == "BOOL" && stmtsq.getTypeOk() == true) {
            node.setTypeOk(true)
          } else {
            node.setTypeOk(false)
            node.setType("Error")
          }
          return stmtsq
        } else {
          return node
        }           
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  // Production 5
  def statement (astNode: ASTNodes): ASTNodes = {
    inputStringStack.head match {
      case `ident` => {
        return assignment(astNode)
      } 
      case `ifterm` => { 
        return ifStatement(astNode)
      } 
      case `whileterm` => { 
        return whileStatement(astNode) 
      }
      case `writeint` => { 
        return writeInt(astNode)
      } 
      case `function` => { 
        return procedure(astNode)
      } 
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  // Production 6
  def assignment (astNode: ASTNodes): ASTNodes = {
    var node = astNode 
    inputStringStack.head match {
      case `ident` => {
        var idnt = new ASTNodes()
        idnt.ASTNodes(inputStringRawStack.head) 
        var variable = inputStringRawStack.head        
        identterminal() 
        node.ASTNodes(inputStringRawStack.head)        
        nIndex = nIndex + 1
        node.addIndex(nIndex)        
        asgnterminal() 
        nIndex = nIndex + 1
        idnt.addIndex(nIndex)
        idnt.addParent(node)
        idnt.setType(declaredVariableType(searchTypeIndex(variable))) 
        node.addChild(idnt)                                
        var asgn = assignmentPrime(node)
        if(asgn != null) {
          return asgn
        } else {
          return node
        }
      }
      case default => throw SyntaxError("Syntax Error !!!")      
    }
  }
    
  // Production 7
  def assignmentPrime (astNode: ASTNodes): ASTNodes = {
    var node = astNode 
    inputStringStack.head match {
      case `num` => { 
        var asgn = new ASTNodes()
        asgn.addParent(node)
        var asgnExpr =  expression(asgn) 
        asgnExpr.addParent(node)
        node.addChild(asgnExpr)
        
        if((node.getChildren().head.getType() == "INT" && asgnExpr.getType() == "INT") || 
            (node.getChildren().head.getType() == "BOOL" && asgnExpr.getType() == "BOOL")) {
          node.setType("BOOL")
        } else {
          node.setType("ERROR")
        }
        
        return node 
        } 
      case `lp` => { 
        var asgn = new ASTNodes()
        asgn.addParent(node)
        var asgnExpr =  expression(asgn) 
        asgnExpr.addParent(node)
        node.addChild(asgnExpr)
        
         if((node.getChildren().head.getType() == "INT" && asgnExpr.getType() == "INT") || 
            (node.getChildren().head.getType() == "BOOL" && asgnExpr.getType() == "BOOL")) {
          node.setType("BOOL")
        } else {
          node.setType("ERROR")
        }
        
        return node
        } 
      case `ident` => { 
        var asgn = new ASTNodes()
        asgn.addParent(node)
        var asgnExpr =  expression(asgn) 
        asgnExpr.addParent(node)
        node.addChild(asgnExpr)
        
         if((node.getChildren().head.getType() == "INT" && asgnExpr.getType() == "INT") || 
            (node.getChildren().head.getType() == "BOOL" && asgnExpr.getType() == "BOOL")) {
          node.setType("BOOL")
        } else {
          node.setType("ERROR")
        }
        
        return node
        } 
      case `readint` => { 
        var rdint = node.getIdentifier()+inputStringRawStack.head
        node.ASTNodes(rdint)
        readintterminal() 
        
        if(node.getChildren().head.getType() == "INT" )  {
          node.setType("BOOL")
        } else {
          node.setType("ERROR")
        }
        
        return node
        } 
      case `boollit` => { 
        var asgn = new ASTNodes()
        asgn.addParent(node)
        var asgnExpr =  expression(asgn) 
        asgnExpr.addParent(node)
        node.addChild(asgnExpr)
        
        if((node.getChildren().head.getType() == "INT" && asgnExpr.getType() == "INT") || 
            (node.getChildren().head.getType() == "BOOL" && asgnExpr.getType() == "BOOL")) {
          node.setType("BOOL")
        } else {
          node.setType("ERROR")
        }
        
        return node
        } 
       case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 8
  def ifStatement (astNode: ASTNodes): ASTNodes = { 
    var node = astNode
    inputStringStack.head match {
      case `ifterm` => {
        node.ASTNodes(inputStringRawStack.head)        
        nIndex = nIndex + 1
        node.addIndex(nIndex)        
        ifterminal()
        var exp = new ASTNodes()
        exp.addParent(node)
        var expr = expression(exp)
        if(expr != null) {
          node.addChild(expr)
          if(expr.getType() == "BOOL") {
            node.setType("BOOL")
          } else {
            node.setType("ERROR")
          }
        }   
        thenterminal()                
        var stmtlist = new ASTNodes()
        stmtlist.ASTNodes("stmt list")
        stmtlist.addParent(node)
        nIndex = nIndex + 1
        stmtlist.addIndex(nIndex)          
        
        var stmtChild = statementSequence(stmtlist)
        stmtChild.addParent(node) 
        if(stmtChild != null) {
          node.addChild(stmtChild)
          
          if(stmtChild.getTypeOk() == true && expr.getType() == "BOOL") {
            node.setType("BOOL")
          } else {
            node.setType("ERROR")
          }
          
        }       
                
        var elsCls = elseClause(node)
        if(elsCls != null) {
          elsCls.addParent(node) 
          node.addChild(elsCls)
          if(elsCls.getTypeOk() == true) {
            node.setType("BOOL")
          } else {
            node.setType("ERROR")
          }
        }         
        
        endterminal()
        return node        
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 9
  def elseClause (astNode: ASTNodes): ASTNodes = {
    var node = astNode
    inputStringStack.head match {
      case `end` => { return null }
      case `elseterm` => {
      elseterminal()      
      var stmtlist = new ASTNodes()
      stmtlist.ASTNodes("stmt list")
      stmtlist.addParent(node)
      nIndex = nIndex + 1
      stmtlist.addIndex(nIndex)          
      
      var stmtChild = statementSequence(stmtlist)
      if(stmtChild != null) {
        node.addChild(stmtChild)
        return node
      } else {
        return null
      }  
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 10
  def whileStatement (astNode: ASTNodes): ASTNodes = {
    var node = astNode
    inputStringStack.head match {
      case `whileterm` => { 
        node.ASTNodes(inputStringRawStack.head)        
        nIndex = nIndex + 1
        node.addIndex(nIndex)        
        whileterminal()
        var exp = new ASTNodes()
        exp.addParent(node)
        var expr = expression(exp)
        if(expr != null) {
          node.addChild(expr)
          if(expr.getType() == "BOOL") {
            node.setType("BOOL")
          } else {
            node.setType("ERROR")
          }
        }   
        doterminal()        
        
        var stmtlist = new ASTNodes()
        stmtlist.ASTNodes("stmt list")
        stmtlist.addParent(node)
        nIndex = nIndex + 1
        stmtlist.addIndex(nIndex)          
        
        var stmtChild = statementSequence(stmtlist)
        stmtChild.addParent(node) 
        if(stmtChild != null) {
          node.addChild(stmtChild)
          if(stmtChild.getTypeOk() == true && expr.getType() == "BOOL") {
            node.setType("BOOL")
          } else {
            node.setType("ERROR")
          }
        }       
        endterminal()
        return node
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 11
  def writeInt (astNode: ASTNodes): ASTNodes = {  
    var node = astNode
    inputStringStack.head match {
      case `writeint` => {
        node.ASTNodes(inputStringRawStack.head)           
        nIndex = nIndex + 1
        node.addIndex(nIndex)
        writeintterminal()   
        var exp = new ASTNodes()
        exp.addParent(node)
        var expr = expression(exp)
        node.addChild(expr)
        if(expr.getType() == "BOOL" || expr.getType() == "INT") {
            node.setType("BOOL")
          } else {
            node.setType("ERROR")
          }
        return node
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 12
  def expression (astNode: ASTNodes): ASTNodes = { 
    var node = astNode     
    inputStringStack.head match {
      case `num` => {
        var exp = new ASTNodes() 
        var smExp = simpleExpression(exp)  
        var expPr = expressionPrime(smExp) 
        if(expPr != null) {
          expPr.addParent(node.getParent())          
          return expPr
        } else {
          smExp.addParent(node.getParent()) 
          return smExp
        }
      }
      case `lp` => {
        var exp = new ASTNodes() 
        var smExp = simpleExpression(exp) 
        var expPr = expressionPrime(smExp) 
        if(expPr != null) {
          expPr.addParent(node.getParent())
          return expPr
        } else {
          smExp.addParent(node.getParent()) 
          return smExp
        }
      }
      case `ident` => {
        var exp = new ASTNodes() 
        var smExp = simpleExpression(exp) 
        var expPr = expressionPrime(smExp) 
        if(expPr != null) {
          expPr.addParent(node.getParent())
          return expPr
        } else {
          smExp.addParent(node.getParent()) 
          return smExp
        }
      }
      case `boollit` => {
        var exp = new ASTNodes() 
        var smExp = simpleExpression(exp) 
        var expPr = expressionPrime(smExp) 
        if(expPr != null) {
          expPr.addParent(node.getParent())
          return expPr
        } else {
          smExp.addParent(node.getParent()) 
          return smExp
        }
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 13
  def expressionPrime (astNode: ASTNodes): ASTNodes = {
    var node = astNode 
    inputStringStack.head match {
      case `compare` => {
        var comp = new ASTNodes()
        comp.ASTNodes(inputStringRawStack.head)        
        nIndex = nIndex + 1
        comp.addIndex(nIndex)
        compareterminal()
        node.addParent(comp)
        comp.addChild(node)        
        var exp = new ASTNodes()
        var expPr = expression(exp)
        if(expPr != null) {
          expPr.addParent(comp)
          comp.addChild(expPr)
          
          if((node.getType() == "INT" && expPr.getType() == "INT") || 
              (node.getType() == "BOOL" && expPr.getType() == "BOOL")) {
            comp.setType("BOOL")
          } else {
            comp.setType("ERROR")
          }
          
          return comp
        } else {
          return node
        }
      }
      case `rp` => { return null }
      case `sc` => { return null }
      case `then` => { return null }
      case `doterm` => { return null }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 14
  def simpleExpression (astNode: ASTNodes): ASTNodes = {
    var node = astNode     
    inputStringStack.head match {
      case `num` => {     
        var smExp = new ASTNodes() 
        var trm = term(smExp)
        var smExpPr = simpleExpressionPrime(trm)
        if(smExpPr != null) {
          smExpPr.addParent(node.getParent()) 
          return smExpPr
        } else {
          trm.addParent(node.getParent())
          return trm
        }
      }
      case `lp` => {
        var smExp = new ASTNodes() 
        var trm = term(smExp)
        var smExpPr = simpleExpressionPrime(trm)
        if(smExpPr != null) {
          smExpPr.addParent(node.getParent()) 
          return smExpPr
        } else {
          trm.addParent(node.getParent())
          return trm
        }
      }
      case `ident` => {
        var smExp = new ASTNodes() 
        var trm = term(smExp)
        var smExpPr = simpleExpressionPrime(trm)
        if(smExpPr != null) {
          smExpPr.addParent(node.getParent()) 
          return smExpPr
        } else {
          trm.addParent(node.getParent())
          return trm
        }
      }
      case `boollit` => {
        var smExp = new ASTNodes() 
        var trm = term(smExp)
        var smExpPr = simpleExpressionPrime(trm)
        if(smExpPr != null) {
          smExpPr.addParent(node.getParent()) 
          return smExpPr
        } else {
          trm.addParent(node.getParent())
          return trm
        }
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 15
  def simpleExpressionPrime (astNode: ASTNodes): ASTNodes = {    
    var node = astNode
    inputStringStack.head match {
      case `add` => {
        var add = new ASTNodes() 
        add.ASTNodes(inputStringRawStack.head)        
        nIndex = nIndex + 1
        add.addIndex(nIndex)
        additiveterminal()
        node.addParent(add)
        add.addChild(node)        
        var smExpr = new ASTNodes()
        var smExprPr = simpleExpression(smExpr)
        if (smExprPr != null ) {
          smExprPr.addParent(add)
          add.addChild(smExprPr)
          
          if((node.getType() == "INT" && smExprPr.getType() == "INT")) {
            add.setType("INT")
          } else {
            add.setType("ERROR")
          }
          
          return add
        } else {
          return null
        }
      }
      case `compare` => { return null }
      case `rp` => { return null }
      case `sc` => { return null }
      case `then` => { return null }
      case `doterm` => { return null }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 16
  def term (astNode: ASTNodes): ASTNodes = {
    var node = astNode 
    
    inputStringStack.head match {
      case `num` => { 
        var trm = new ASTNodes() 
        var fctr = factor(trm) 
        var trmPr = termPrime(fctr) 
        if(trmPr != null) {
          trmPr.addParent(node.getParent())
          return trmPr
        } else {
          fctr.addParent(node.getParent())
          return fctr
        }
      }
      case `lp` => {
        var trm = new ASTNodes() 
        var fctr = factor(trm) 
        var trmPr = termPrime(fctr) 
        if(trmPr != null) {
          trmPr.addParent(node.getParent())
          return trmPr
        } else {
          fctr.addParent(node.getParent())
          return fctr
        }
      }
      case `ident` => { 
        var trm = new ASTNodes() 
        var fctr = factor(trm)
        var trmPr = termPrime(fctr) 
        if(trmPr != null) {
          trmPr.addParent(node.getParent())
          return trmPr
        } else {
          fctr.addParent(node.getParent())
          return fctr
        }
      }
      case `boollit` => {
        var trm = new ASTNodes() 
        var fctr = factor(trm) 
        var trmPr = termPrime(fctr) 
        if(trmPr != null) {
          trmPr.addParent(node.getParent())
          return trmPr
        } else {
          fctr.addParent(node.getParent())
          return fctr
        }
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 17
  def termPrime (astNode: ASTNodes): ASTNodes = {
    var node = astNode     
    inputStringStack.head match {
      case `mult` => { 
        var mul = new ASTNodes()
        mul.ASTNodes(inputStringRawStack.head)         
        nIndex = nIndex + 1
        mul.addIndex(nIndex)
        multiplicativeterminal()
        node.addParent(mul)
        mul.addChild(node)
        var trm = new ASTNodes() 
        trm.addParent(mul)
        var trmPr  = term(trm) 
        if (trmPr != null) {
          mul.addChild(trmPr)
          
          if((node.getType() == "INT" && trmPr.getType() == "INT")) {
            mul.setType("INT")
          } else {
            mul.setType("ERROR")
          }
          
          return mul
        } else {
          return node
        }
      }
      case `add` => { return null }
      case `compare` => { return null }
      case `rp` => { return null }
      case `sc` => { return null }
      case `then` => { return null }
      case `doterm` => { return null }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 18
  def factor (astNode: ASTNodes): ASTNodes = {
    var node = astNode     
    inputStringStack.head match {
      case `num` => { 
        node.ASTNodes(inputStringRawStack.head)
        nIndex = nIndex + 1
        node.addIndex(nIndex)
        numterminal()
        node.setType("INT") 
        return node
      }
      case `lp` => {
        lpterminal()
        expression(node)
        rpterminal()
        return null
      }
      case `ident` => { 
        node.ASTNodes(inputStringRawStack.head) 
        var variable = inputStringRawStack.head
        nIndex = nIndex + 1
        node.addIndex(nIndex)
        identterminal()
        node.setType(declaredVariableType(searchTypeIndex(variable)))
        return node 
      } 
      case `boollit` => {
        boollitterminal()
        return null
      } 
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // Production 19
  def procedure (astNode: ASTNodes): ASTNodes = {  
    var node = astNode
    inputStringStack.head match {
      case `function` => {
        node.ASTNodes(inputStringStack.head)           
        nIndex = nIndex + 1
        node.addIndex(nIndex)
        functionterminal() 
        var idnt = new ASTNodes()
        idnt.ASTNodes(inputStringRawStack.head)
        idnt.addParent(node)
        nIndex = nIndex + 1
        idnt.addIndex(nIndex) 
        node.addChild(idnt) 
        identterminal()
        lpterminal()
        var exp = new ASTNodes()
        exp.addParent(node)
        var expr = expression(exp)
        node.addChild(expr)
        if(expr.getType() == "BOOL" || expr.getType() == "INT" || expr.getType() == "PROCEDURE") {
          node.setType("BOOL")
        } else {
          node.setType("ERROR")
        }
        
        rpterminal()
        var stmtlist = new ASTNodes()
        stmtlist.ASTNodes("stmt list")
        stmtlist.addParent(node)
        nIndex = nIndex + 1
        stmtlist.addIndex(nIndex)          
        
        if(inputStringStack.head != "END") {
          var stmtChild = statementSequence(stmtlist)
          stmtChild.addParent(node) 
          if(stmtChild != null) {
            node.addChild(stmtChild)
            if(stmtChild.getTypeOk() == true && (expr.getType() == "BOOL" || expr.getType() == "INT" )) {
              node.setType("BOOL")
            } else {
              node.setType("ERROR")
            }
          } 
        }      
        endterminal()
        return node
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  // terminals    
  def programterminal () {
    inputStringStack.head match {
      case `prog` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
    
  }
  
  def numterminal () = {
    inputStringStack.head match {
      case `num` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  def lpterminal() = {
    inputStringStack.head match {
      case `lp` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def rpterminal() = {
    inputStringStack.head match {
      case `rp` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }  
  
  def beginterminal() = {
    inputStringStack.head match {
      case `begin` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def endterminal () = {
    inputStringStack.head match {
      case `end` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  def variableterminal () = {
    inputStringStack.head match {
      case `variable` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def identterminal () = {
    inputStringStack.head match {
      case `ident` => {    
        
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  def functionterminal () = {
    inputStringStack.head match {
      case `function` => {          
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  } 
  
  def asterminal() = {
    inputStringStack.head match {
      case `as` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def scterminal() = {
    inputStringStack.head match {
      case `sc` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def intterminal() = {
    inputStringStack.head match {
      case `int` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }  
  
  def procedureterminal() = {
    inputStringStack.head match {
      case `procedure` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def boolterminal() = {
    inputStringStack.head match {
      case `bool` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def asgnterminal() = {
    inputStringStack.head match {
      case `asgn` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def readintterminal() = {
    inputStringStack.head match {
      case `readint` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def ifterminal() = {
    inputStringStack.head match {
      case `ifterm` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def elseterminal() = {
    inputStringStack.head match {
      case `elseterm` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }  
  
  def whileterminal () = {
    inputStringStack.head match {
      case `whileterm` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def doterminal() = {
    inputStringStack.head match {
      case `doterm` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def writeintterminal() = {
    inputStringStack.head match {
      case `writeint` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def compareterminal() = {
    inputStringStack.head match {
      case `compare` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def multiplicativeterminal() = {
    inputStringStack.head match {
      case `mult` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def additiveterminal() = {
    inputStringStack.head match {
      case `add` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def boollitterminal() = {
    inputStringStack.head match {
      case `boollit` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def thenterminal() = {
    inputStringStack.head match {
      case `then` => {
        inputStringStack.pop()
        inputStringRawStack.pop()
      }
      case default => throw SyntaxError("Syntax Error !!!")  
    }
  }
  
  def addNodeInAST(tree: StringBuffer, astNode: ASTNodes): StringBuffer = {
    var nColor = 0
    
    astNode.getType() match {
      case "BOOL" => nColor = 2
      case "INT" => nColor = 3
      case "ERROR" => nColor = 4
      case default => 
    } 
    
    if (astNode.getIdentifier() == "decl list"){
      nColor = 1
    }
    if (astNode.getIdentifier() == "stmt list"){
      nColor = 1
    }    
    nColor match {
      case 1 => tree.append(" n"+astNode.getIndex() + " [label=\""+astNode.getIdentifier()+"\",fillcolor=\"/x11/white\",shape=none]").append(System.getProperty("line.separator"))
      case 2 => tree.append(" n"+astNode.getIndex() + " [label=\""+astNode.getIdentifier()+"\",fillcolor=\"/x11/lightgrey\",shape=box]").append(System.getProperty("line.separator"))
      case 3 => tree.append(" n"+astNode.getIndex() + " [label=\""+astNode.getIdentifier()+"\",fillcolor=\"/pastel13/3\",shape=box]").append(System.getProperty("line.separator"))
      case 4 => tree.append(" n"+astNode.getIndex() + " [label=\""+astNode.getIdentifier()+"\",fillcolor=\"/pastel13/1\",shape=box]").append(System.getProperty("line.separator"))
      case default => tree.append(" n"+astNode.getIndex() + " [label=\""+astNode.getIdentifier()+"\",fillcolor=\"/pastel13/2\",shape=box]").append(System.getProperty("line.separator"))
    }
    
    return tree
  }
  
  def relNodeInAST(tree: StringBuffer, astNode: ASTNodes): StringBuffer = {
    tree.append(" n"+astNode.getParent().getIndex()+" -> n"+astNode.getIndex()).append(System.getProperty("line.separator"));
    return tree
  }
  
  def ASTTemplate(tree: StringBuffer, action: Int): StringBuffer = {
    action match {
      case 1 => { 
        tree.append("digraph AST {").append(System.getProperty("line.separator"))
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
  
  def searchTypeIndex (str: String): Int = {
    var idx: Int = -1
    var i: Int = 0
    while(i < declaredVariables.length) {
      if(declaredVariables(i).equals(str))
      {
          idx = i;
          return idx
      }
      i = i + 1
    }
    return idx
  }
  
}

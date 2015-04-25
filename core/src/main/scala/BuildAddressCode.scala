package edu.utsa.cs5363
import scala.io.Source
import scala.collection.mutable._
import java.io
import scala.collection.mutable.ArrayBuffer
import scala.util.control._

object BuildAddressCode {  
  var nIndex = 1
  var entryIndex = 1
  var exitIndex = 1
  var rIndex = 0
  var typeError: Boolean = false
  var blockNodes: ListBuffer[AddressCodeBlocks] = new ListBuffer()
  var NumOfDeclaredVar: Int = 0
  
  def BuildAddressCodeInit (inputAST: ASTNodes, fileName: String): ListBuffer[AddressCodeBlocks] = {
    nIndex = 1
    rIndex = 0
    typeError = false
    NumOfDeclaredVar = 0
    blockNodes = new ListBuffer()
    var AddrBlock: StringBuffer = new StringBuffer()
    AddrBlock = CFGTemplate(AddrBlock, 1)  
    AddrBlock = traverTreeDFS(inputAST, AddrBlock)
    AddrBlock = CFGTemplate(AddrBlock, 0) 
    if(!typeError) {
      CoreCompiler.writeOutputFile(fileName, AddrBlock.toString()) 
      return blockNodes 
    } else {
      return null 
    }        
  }
  
  def traverTreeDFS (asTree: ASTNodes, treeBuffer: StringBuffer): StringBuffer = { 
    var astNodeStack = new Stack[ASTNodes]
    var treeBuild = treeBuffer
    var treeBlock: StringBuffer = new StringBuffer
    var tree: StringBuffer = new StringBuffer
    var block = new AddressCodeBlocks()
    
    block.AddressCodeBlocks("B"+nIndex).setIndex(nIndex) /// create block here
    tree = ILOCInstructionList.ILOCBlockTmpl(block, tree)
    
    astNodeStack.push(asTree)
    var nestCbr: Stack[AddressCodeBlocks] = new Stack[AddressCodeBlocks] 
    val loop = new Breaks
    loop.breakable {
    while (!astNodeStack.isEmpty) {
      if (astNodeStack.head.getVisit()) { 
        if(astNodeStack.head.getIdentifier() == "program") {
          if(astNodeStack.head.getType() == "ERROR") {
            typeError = true
            block = null
            loop.break
          }
        }        
        astNodeStack.head.setVisit(false)
      }
      if (astNodeStack.head.getVisitedChild() != null) {
        if(astNodeStack.head.getIdentifier() == "while") {
          if(block.getCbr1() == null){
            nIndex = nIndex + 1
            var instr: Array[String] = Array("jumpI", "B"+nIndex)
            block.addInstruction(instr)
            block.setJump(nIndex)
            tree = ILOCInstructionList.ILOCJumpTmpl(block, tree, nIndex)
            
            blockNodes += block
            treeBlock.append(ILOCInstructionList.nodeTemplate(block, tree))
            treeBlock = relNodeInCFG(treeBlock, block, nIndex)
            
            tree = new StringBuffer
            block = new AddressCodeBlocks() 
            block.AddressCodeBlocks("B"+nIndex).setIndex(nIndex) /// create block here
            tree = ILOCInstructionList.ILOCBlockTmpl(block, tree)            
          } else {
            blockNodes += block
            var bidx = block.getCbr1()
            treeBlock.append(ILOCInstructionList.nodeTemplate(block, tree))
            treeBlock = relNodeInCFG(treeBlock, block, block.getCbr1().asInstanceOf[Int])
            treeBlock = relNodeInCFG(treeBlock, block, block.getCbr2().asInstanceOf[Int])
            nestCbr.push(block) 
            
            tree = new StringBuffer
            block = new AddressCodeBlocks() 
            block.AddressCodeBlocks("B"+bidx).setIndex(bidx.asInstanceOf[Int]) /// create block here
            tree = ILOCInstructionList.ILOCBlockTmpl(block, tree)  
          }          
        }
        if(astNodeStack.head.getIdentifier() == "if") {
          if(block.getCbr1() != null) {
            blockNodes += block
            var bidx = block.getCbr1()
            treeBlock.append(ILOCInstructionList.nodeTemplate(block, tree))
            treeBlock = relNodeInCFG(treeBlock, block, block.getCbr1().asInstanceOf[Int])
            treeBlock = relNodeInCFG(treeBlock, block, block.getCbr2().asInstanceOf[Int])
            nestCbr.push(block) 
            
            tree = new StringBuffer
            block = new AddressCodeBlocks() 
            block.AddressCodeBlocks("B"+bidx).setIndex(bidx.asInstanceOf[Int]) /// create block here
            tree = ILOCInstructionList.ILOCBlockTmpl(block, tree)  
          }
        }
        if(astNodeStack.head.getIdentifier() == "decl list") {
          NumOfDeclaredVar = astNodeStack.head.getChildren().length
          block.setNumOfDeclVar(NumOfDeclaredVar)
        }
        astNodeStack.push(astNodeStack.head.getVisitedChild())
      } else {
        if(astNodeStack.head.getIdentifier() == "program") {     
          exitIndex = block.getIndex()
          tree = ILOCInstructionList.exitNodeTemplate(block, tree)
        } else if (astNodeStack.head.getIdentifier().split(": ")(0) == "decl"){
          var instr: Array[String] = Array("loadI", "0", "r_"+astNodeStack.head.getIdentifier().split(": ").apply(1))
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCDeclarationTmpl(astNodeStack.head, tree)
        } else if(astNodeStack.head.getIdentifier() == ":=readInt") { 
          var instr: Array[String] = Array("readInt", "r_"+astNodeStack.head.getChildren().head.getIdentifier())
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCReadIntTmpl(astNodeStack.head, tree)
        } else if(astNodeStack.head.getIdentifier() == "writeInt") {
          var op: String = {
            if(astNodeStack.head.getChildren().head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().head.getIdentifier()
            } else {
              "r"+astNodeStack.head.getChildren().head.getRIndex()
            }
          }      
          var instr: Array[String] = Array("writeInt", op)
          block.addInstruction(instr)          
          tree = ILOCInstructionList.ILOCWriteIntTmpl(astNodeStack.head, tree, op)      
        } else if(astNodeStack.head.getIdentifier() == ">=" || astNodeStack.head.getIdentifier() == "<=" 
            || astNodeStack.head.getIdentifier() == "=" || astNodeStack.head.getIdentifier() == ">" 
            || astNodeStack.head.getIdentifier() == "<" || astNodeStack.head.getIdentifier() == "!=" ) {
          var op1: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().head.getIdentifier()
            } else if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().head.getRIndex()
            } else {
              null
            }
          }
          var op2: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().tail.head.getIdentifier()
            } else if((astNodeStack.tail.head.getType() == "BOOL" || astNodeStack.tail.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            } else {             
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            }
          }
          var cmpOp = ""
          astNodeStack.head.getIdentifier() match {
            case ">=" => cmpOp = "cmp_GE"
            case "<=" => cmpOp = "cmp_LE"
            case "=" => cmpOp = "cmp_EQ"
            case ">" => cmpOp = "cmp_GT"
            case "<" => cmpOp = "cmp_LT"
            case "!=" => cmpOp = "cmp_NE"                  
          } 
          var instr: Array[String] = Array(cmpOp, op1, op2, "r"+rIndex)
          block.addInstruction(instr)
          nIndex = nIndex + 1 
          var br1 = nIndex 
          nIndex = nIndex + 1 
          var br2 = nIndex
          instr = Array("cbr", "r"+rIndex, "B"+br1, "B"+br2)
          block.addInstruction(instr).setCbr1(br1).setCbr2(br2)
          
          tree = ILOCInstructionList.ILOCCmpTmpl(astNodeStack.head, tree, rIndex, op1, op2)
          tree = ILOCInstructionList.ILOCCbrTmpl(astNodeStack.head, tree, rIndex, br1, br2)
          astNodeStack.head.addRIndex(rIndex)          
          rIndex = rIndex + 1                   
          
        } else if(astNodeStack.head.getIdentifier() == "+" || astNodeStack.head.getIdentifier() == "-") {
          var op1: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().head.getIdentifier()
            } else if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().head.getRIndex()
            } else {
              null
            }
          }
          var op2: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().tail.head.getIdentifier()
            } else if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            } else {             
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            }
          }
          var addpOp = ""
          astNodeStack.head.getIdentifier() match {
            case "+" => addpOp = "add"
            case "-" => addpOp = "sub"                
          } 
          var instr: Array[String] = Array(addpOp, op1, op2, "r"+rIndex)
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCAddTmpl(astNodeStack.head, tree, rIndex, op1, op2)          
          astNodeStack.head.addRIndex(rIndex)
          rIndex = rIndex + 1
        } else if(astNodeStack.head.getIdentifier() == "*" || astNodeStack.head.getIdentifier() == "div" 
            || astNodeStack.head.getIdentifier() == "mod") {
          var op1: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().head.getIdentifier()
            } else if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().head.getRIndex()
            } else {
              null
            }
          }
          var op2: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().tail.head.getIdentifier()
            } else if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            } else {
              null
            }
          }
          var multpOp = ""
          astNodeStack.head.getIdentifier() match {
            case "*" => multpOp = "mult"
            case "div" => multpOp = "div" 
            case "mod" => multpOp = "mod"
          }
          var instr: Array[String] = Array(multpOp, op1, op2, "r"+rIndex)
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCMultTmpl(astNodeStack.head, tree, rIndex, op1, op2)          
          astNodeStack.head.addRIndex(rIndex)
          rIndex = rIndex + 1
        } else if(astNodeStack.head.getIdentifier() == "while") {
          var instr: Array[String] = Array("jumpI", "B"+nestCbr.head.getIndex())
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCJumpTmpl(block, tree, nestCbr.head.getIndex())  
          block.setJump(nestCbr.head.getIndex())
          blockNodes += block
          treeBlock.append(ILOCInstructionList.nodeTemplate(block, tree))
          treeBlock = relNodeInCFG(treeBlock, block, nestCbr.head.getIndex())
          
          var bidx = nestCbr.head.getCbr2()
          nestCbr.pop()
          tree = new StringBuffer
          block = new AddressCodeBlocks() 
          block.AddressCodeBlocks("B"+bidx).setIndex(bidx.asInstanceOf[Int]) /// create block here
          tree = ILOCInstructionList.ILOCBlockTmpl(block, tree)           
        } else if(astNodeStack.head.getIdentifier() == "if") { 
          var instr: Array[String] = Array("jumpI", "B"+nestCbr.head.getCbr2())
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCJumpTmpl(block, tree, nestCbr.head.getCbr2())  
          block.setJump(nestCbr.head.getCbr2())
          blockNodes += block
          treeBlock.append(ILOCInstructionList.nodeTemplate(block, tree))
          treeBlock = relNodeInCFG(treeBlock, block, nestCbr.head.getCbr2().asInstanceOf[Int])
          
          var bidx = nestCbr.head.getCbr2()
          nestCbr.pop()
          tree = new StringBuffer
          block = new AddressCodeBlocks() 
          block.AddressCodeBlocks("B"+bidx).setIndex(bidx.asInstanceOf[Int]) /// create block here
          tree = ILOCInstructionList.ILOCBlockTmpl(block, tree)   
        } else if(astNodeStack.head.getIdentifier() == ":=") { //here we go
          var op1: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().head.getIdentifier()
            } else {
              null
            }
          }
          var op2: String = {
            if((astNodeStack.head.getType() == "BOOL" || astNodeStack.head.getType() == "INT") 
                && astNodeStack.head.getChildren().tail.head.getIsIdent() == true) {
              "r_"+astNodeStack.head.getChildren().tail.head.getIdentifier()
            } else if((astNodeStack.tail.head.getType() == "BOOL" || astNodeStack.tail.head.getType() == "INT") 
                && astNodeStack.head.getChildren().head.getIsIdent() == false) {
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            } else {
              "r"+astNodeStack.head.getChildren().tail.head.getRIndex()
            }
          }
          var instr: Array[String] = Array("i2i", op2, op1)
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCAsgnTmpl(astNodeStack.head, tree, op1, op2) 
        } else if(astNodeStack.head.getIdentifier().forall(_.isDigit) && !astNodeStack.head.hasChildren()) {
          astNodeStack.head.addRIndex(rIndex)
          var instr: Array[String] = Array("loadI", astNodeStack.head.getIdentifier(), "r"+astNodeStack.head.getRIndex())
          block.addInstruction(instr)
          tree = ILOCInstructionList.ILOCNumTmpl(astNodeStack.head, tree, rIndex)
          rIndex = rIndex + 1
        } else if(!astNodeStack.head.getIdentifier().forall(_.isDigit) && !astNodeStack.head.hasChildren()
            && astNodeStack.head.getParent().getParent().getIdentifier() != "decl list") {   
          astNodeStack.head.setIsIdent(true)
          tree = ILOCInstructionList.ILOCIdentTmpl(astNodeStack.head, tree)
        } else {
          // do nothing
        }            
        astNodeStack.head.setVisit(false)
        astNodeStack.pop()
      }
    }
    }
    if(!typeError){
      treeBlock.append(ILOCInstructionList.nodeTemplate(block, tree))    
      var instr: Array[String] = Array("exit")
      block.addInstruction(instr)
      treeBuild.append(treeBlock)
    }
    blockNodes += block
    return treeBuild
  }
    
  def makeCFG (asTree: ASTNodes, fileName: String): ListBuffer[AddressCodeBlocks] = {
    var addressBlocks: ListBuffer[AddressCodeBlocks] = new ListBuffer()
    addressBlocks = BuildAddressCodeInit(asTree: ASTNodes, fileName: String) 
    if(addressBlocks != null){
      Console.println("ILOC CFG generated successfully !!!")
      
      return addressBlocks
    } else {
      Console.println("There are type errors!!! CFG not generated !!!")
      return null
    }    
  }
    
  def relNodeInCFG(AddrBlock: StringBuffer, block: AddressCodeBlocks, relIndex: Int): StringBuffer = {
    AddrBlock.append(" n"+block.getIndex()+" -> n"+relIndex).append(System.getProperty("line.separator"))
    return AddrBlock
  }
  
  def CFGTemplate(AddrBlock: StringBuffer, action: Int): StringBuffer = {
    action match {
      case 1 => { 
        AddrBlock.append("digraph AST {").append(System.getProperty("line.separator"))
        AddrBlock.append(" node [shape = none];").append(System.getProperty("line.separator"))
        AddrBlock.append(" edge [tailport = s];").append(System.getProperty("line.separator"))
        AddrBlock.append(" entry").append(System.getProperty("line.separator"))
        AddrBlock.append(" subgraph cluster {").append(System.getProperty("line.separator"))
        AddrBlock.append(" color=\"/x11/white\"").append(System.getProperty("line.separator"))
        return AddrBlock
      } 
      case 0 => {
        AddrBlock.append("}").append(System.getProperty("line.separator"))
        AddrBlock.append("entry -> n"+entryIndex).append(System.getProperty("line.separator"))
        AddrBlock.append("n"+exitIndex+" -> exit").append(System.getProperty("line.separator"))
        AddrBlock.append("}").append(System.getProperty("line.separator"))
        return AddrBlock
      }
      case default => {
        throw InternalError("Some Internal Error occured !!!") 
      }      
    }
  }
  
}

package edu.utsa.cs5363
import scala.io.Source
import scala.collection.mutable._
import java.io
import scala.collection.mutable.ArrayBuffer

object ILOCInstructionList { 
  var tr = "<tr>"
  var td = "<td align=\"left\">";
  var tdPr = "</td>"
  var trPr = "</tr>"
  
  def nodeTemplate(block: AddressCodeBlocks, treeBufferNodeIntr: StringBuffer): StringBuffer = {
    var treeBuffer: StringBuffer = new StringBuffer()
    treeBuffer.append("n").append(block.getIndex()+" ")
    treeBuffer.append(" [label=<<table border=\"0\"> ")
    treeBuffer.append(treeBufferNodeIntr)
    treeBuffer.append(" </table>>,fillcolor=\"/x11/white\",shape=box] ").append(System.getProperty("line.separator"))
    return treeBuffer
  }
  
  def exitNodeTemplate (block: AddressCodeBlocks, treeBuffer: StringBuffer): StringBuffer = {
    treeBuffer.append(tr+td+"exit"+tdPr+td+tdPr+td+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCBlockTmpl (block: AddressCodeBlocks, treeBuffer: StringBuffer): StringBuffer = {
    treeBuffer.append("<tr><td border=\"1\" colspan=\"3\">")
    treeBuffer.append("B"+block.getIndex())
    treeBuffer.append("</td></tr>")
    return treeBuffer
  }
  
  def ILOCDeclarationTmpl (astNode: ASTNodes, treeBuffer: StringBuffer): StringBuffer = {
    treeBuffer.append(tr+td+"loadI"+tdPr+td+"0"+tdPr+td+"=&gt; r_"
        +astNode.getIdentifier().split(": ")(1)+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCReadIntTmpl (astNode: ASTNodes, treeBuffer: StringBuffer): StringBuffer = {
    treeBuffer.append(tr+td+"readInt"+tdPr+td+tdPr+td+"=&gt; r_"
        +astNode.getChildren().head.getIdentifier()+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCWriteIntTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, op:String): StringBuffer = {
    treeBuffer.append(tr+td+"writeInt"+tdPr+td+op+tdPr+td+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCNumTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, idx: Int): StringBuffer = {
    treeBuffer.append(tr+td+"loadI"+tdPr+td+astNode.getIdentifier().toInt+tdPr+td+"=&gt; r"
        +idx+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCIdentTmpl (astNode: ASTNodes, treeBuffer: StringBuffer): StringBuffer = {
    return treeBuffer
  }
  
  def ILOCMultTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, idx: Int, op1: String, op2: String): StringBuffer = {
    var multpOp = ""
    astNode.getIdentifier() match {
      case "*" => multpOp = "mult"
      case "div" => multpOp = "div" 
      case "mod" => multpOp = "mod"
    }
    treeBuffer.append(tr+td+multpOp+tdPr+td+op1+", "+op2+tdPr+td+"=&gt; r"+idx+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCAddTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, idx: Int, op1: String, op2: String): StringBuffer = {
    var addpOp = ""
    astNode.getIdentifier() match {
      case "+" => addpOp = "add"
      case "-" => addpOp = "sub"                
    }
    treeBuffer.append(tr+td+addpOp+tdPr+td+op1+", "+op2+tdPr+td+"=&gt; r"+idx+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCCmpTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, idx: Int, op1: String, op2: String): StringBuffer = {
    var cmpOp = ""
    astNode.getIdentifier() match {
      case ">=" => cmpOp = "cmp_GE"
      case "<=" => cmpOp = "cmp_LE"
      case "=" => cmpOp = "cmp_EQ"
      case ">" => cmpOp = "cmp_GT"
      case "<" => cmpOp = "cmp_LT"
      case "!=" => cmpOp = "cmp_NE"                  
    }
    treeBuffer.append(tr+td+cmpOp+tdPr+td+op1+", "+op2+tdPr+td+"=&gt; r"+idx+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCCbrTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, ridx: Int, br1: Int, br2: Int): StringBuffer = {
    treeBuffer.append(tr+td+"cbr"+tdPr+td+"r"+ridx+tdPr+td+"-&gt; B"+br1+", B"+br2+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCAsgnTmpl (astNode: ASTNodes, treeBuffer: StringBuffer, op1: String, op2: String): StringBuffer = {
    treeBuffer.append(tr+td+"i2i"+tdPr+td+op2+tdPr+td+"=&gt; "+op1+tdPr+trPr)
    return treeBuffer
  }
  
  def ILOCJumpTmpl (block: AddressCodeBlocks, treeBuffer: StringBuffer, idx: Any): StringBuffer = {
    treeBuffer.append(tr+td+"jumpI"+tdPr+td+tdPr+td+"=&gt; B"+idx+tdPr+trPr)
    return treeBuffer
  }
}
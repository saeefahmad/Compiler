package edu.utsa.cs5363
import scala.io.Source
import scala.collection.mutable._
import java.io
import scala.collection.mutable.ArrayBuffer
import scala.util.control._
import BuildAddressCode._

object BuildMipsCode {
//  var blockNodes: ListBuffer[AddressCodeBlocks] = new ListBuffer()
  var NumOfDeclaredVar: Int = 0
//  var blockContent: StringBuffer = new StringBuffer()
  var declVarCounter: Int = 0
  var declVar: ArrayBuffer[String] = new ArrayBuffer()
  
  def BuildMIPSInit (inputAddressCode: ListBuffer[AddressCodeBlocks], fileName: String): StringBuffer = {
    NumOfDeclaredVar = 0
    declVarCounter = 0
    declVar = new ArrayBuffer()
    blockNodes = new ListBuffer()
    blockNodes = inputAddressCode
    var blockContent: StringBuffer = new StringBuffer()
    if(blockNodes == null) { 
      return null 
    } else {
      blockContent = MIPSTemplate(blockContent, 1)  
      blockContent = traverAddressCode(blockNodes, blockContent)
      CoreCompiler.writeOutputFile(fileName, blockContent.toString())
      return blockContent
    }     
  }
  
  def traverAddressCode (blocks: ListBuffer[AddressCodeBlocks], strBuffer: StringBuffer): StringBuffer = { 
    var mipsBuild = strBuffer
    var mipsBlock = blocks
    var mipsStr: StringBuffer = new StringBuffer
    var cbrCounter = 0
    var i = 0
    
    while(i < blocks.length) {
      mipsStr.append(System.getProperty("line.separator"))
      mipsStr.append("B").append(mipsBlock.apply(i).getIndex()).append(": ").append(System.getProperty("line.separator"))
      mipsStr.append(System.getProperty("line.separator"))
      for (instr <- mipsBlock.apply(i).getInstruction()) { 
        if(instr(0) == "loadI") {
          mipsStr.append("# ").append(instr(0)).append(" ").append(instr(1)).append(" => ")
          .append(instr(2)).append(System.getProperty("line.separator"))          
          var offset = getRegisterOffset(mipsBlock.head, instr(2))
          var destAddress = offset+"($fp)"
          
          mipsStr.append("li $t0, "+instr(1)).append(System.getProperty("line.separator"))
          mipsStr.append("sw $t0, "+destAddress).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))
        } else if(instr(0) == "i2i") {
          mipsStr.append("# ").append(instr(0)).append(" ").append(instr(1)).append(" => ")
          .append(instr(2)).append(System.getProperty("line.separator"))          
          var offset1 = getRegisterOffset(mipsBlock.head, instr(1))
          var offset2 = getRegisterOffset(mipsBlock.head, instr(2))
          var srcAddress = offset1+"($fp)"
          var destAddress = offset2+"($fp)"
          
          mipsStr.append("lw $t1, "+srcAddress).append(System.getProperty("line.separator"))
          mipsStr.append("add $t0, $t1, $zero").append(System.getProperty("line.separator"))
          mipsStr.append("sw $t0, "+destAddress).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))
        } else if(instr(0) == "readInt") {
          mipsStr.append("# ").append(instr(0)).append(" ").append(" => ")
          .append(instr(1)).append(System.getProperty("line.separator"))          
          var offset = getRegisterOffset(mipsBlock.head, instr(1))
          var destAddress = offset+"($fp)"
          
          mipsStr.append("li $v0, 5").append(System.getProperty("line.separator"))
          mipsStr.append("syscall").append(System.getProperty("line.separator"))
          mipsStr.append("add $t0, $v0, $zero").append(System.getProperty("line.separator"))
          mipsStr.append("sw $t0, "+destAddress).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))
        } else if(instr(0) == "writeInt") { 
          mipsStr.append("# ").append(instr(0)).append(" ").append(" => ")
          .append(instr(1)).append(System.getProperty("line.separator"))          
          var offset = getRegisterOffset(mipsBlock.head, instr(1))
          var address = offset+"($fp)"
          
          mipsStr.append("li $v0, 1").append(System.getProperty("line.separator"))
          mipsStr.append("lw $t1, "+address).append(System.getProperty("line.separator"))
          mipsStr.append("add $a0, $t1, $zero").append(System.getProperty("line.separator"))
          mipsStr.append("syscall").append(System.getProperty("line.separator"))
          mipsStr.append("li $v0, 4").append(System.getProperty("line.separator"))
          mipsStr.append("la $a0, newline").append(System.getProperty("line.separator"))
          mipsStr.append("syscall").append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))
        } else if(instr(0) == "cbr") {
          mipsStr.append("# ").append(instr(0)).append(" ").append(instr(1)).append(" -> ")
          .append(instr(2)).append(" ").append(instr(3)).append(System.getProperty("line.separator"))          
          var offset = getRegisterOffset(mipsBlock.head, instr(1))
          var address = offset+"($fp)"
          
          mipsStr.append("lw $t0, "+address).append(System.getProperty("line.separator"))
          mipsStr.append("bne $t0, $zero, "+instr(2)).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))
          cbrCounter = cbrCounter + 1;
          mipsStr.append("L"+cbrCounter+":").append(System.getProperty("line.separator"))
          mipsStr.append("j "+instr(3)).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))          
        } else if(instr(0) == "jumpI") {
          mipsStr.append("# ").append(instr(0)).append(" ").append(" -> ")
          .append(instr(1)).append(System.getProperty("line.separator"))  
          mipsStr.append("j "+instr(1)).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))
        } else if(instr(0) == "add" || instr(0) == "sub") {
          var operation = instr(0)+"u"
           mipsStr.append("# ").append(instr(0)).append(" ").append(instr(1)+", "+instr(2)).append(" => ")
           .append(instr(3)).append(System.getProperty("line.separator"))          
           var offset1 = getRegisterOffset(mipsBlock.head, instr(1))
           var offset2 = getRegisterOffset(mipsBlock.head, instr(2))
           var offset3 = getRegisterOffset(mipsBlock.head, instr(3))
           var srcAddress1 = offset1+"($fp)"
           var srcAddress2 = offset2+"($fp)"
           var destAddress = offset3+"($fp)"
           
           mipsStr.append("lw $t1, "+srcAddress1).append(System.getProperty("line.separator"))
           mipsStr.append("lw $t2, "+srcAddress2).append(System.getProperty("line.separator"))
           mipsStr.append(operation+" $t0, $t1, $t2").append(System.getProperty("line.separator"))
           mipsStr.append("sw $t0, "+destAddress).append(System.getProperty("line.separator"))
           mipsStr.append(System.getProperty("line.separator"))  
        } else if(instr(0) == "mult" || instr(0) == "div" || instr(0) == "mod") { 
           var operation = ""
           instr(0) match {
             case "mult" => operation = "mul"
             case "div" => operation = "div"
             case "mod" => operation = "rem"
           }
           mipsStr.append("# ").append(instr(0)).append(" ").append(instr(1)+", "+instr(2)).append(" => ")
           .append(instr(3)).append(System.getProperty("line.separator"))          
           var offset1 = getRegisterOffset(mipsBlock.head, instr(1))
           var offset2 = getRegisterOffset(mipsBlock.head, instr(2))
           var offset3 = getRegisterOffset(mipsBlock.head, instr(3))
           var srcAddress1 = offset1+"($fp)"
           var srcAddress2 = offset2+"($fp)"
           var destAddress = offset3+"($fp)"
           
           mipsStr.append("lw $t1, "+srcAddress1).append(System.getProperty("line.separator"))
           mipsStr.append("lw $t2, "+srcAddress2).append(System.getProperty("line.separator"))
           mipsStr.append(operation+" $t0, $t1, $t2").append(System.getProperty("line.separator"))
           mipsStr.append("sw $t0, "+destAddress).append(System.getProperty("line.separator"))
           mipsStr.append(System.getProperty("line.separator"))  
        } else if(instr(0) == "cmp_GE" || instr(0) == "cmp_LE" || instr(0) == "cmp_EQ" ||
            instr(0) == "cmp_GT" || instr(0) == "cmp_LT" || instr(0) == "cmp_NE") {
          var operation = "" 
          instr(0) match {
            case "cmp_GE" => operation = "sge"
            case "cmp_LE" => operation = "sle"
            case "cmp_EQ" => operation = "seq"
            case "cmp_GT" => operation = "sgt"
            case "cmp_LT" => operation = "slt"
            case "cmp_NE" => operation = "sne"
          }
          mipsStr.append("# ").append(instr(0)).append(" ").append(instr(1)+", "+instr(2)).append(" => ")
          .append(instr(3)).append(System.getProperty("line.separator"))          
          var offset1 = getRegisterOffset(mipsBlock.head, instr(1))
          var offset2 = getRegisterOffset(mipsBlock.head, instr(2))
          var offset3 = getRegisterOffset(mipsBlock.head, instr(3))
          var srcAddress1 = offset1+"($fp)"
          var srcAddress2 = offset2+"($fp)"
          var destAddress = offset3+"($fp)"
           
          mipsStr.append("lw $t1, "+srcAddress1).append(System.getProperty("line.separator"))
          mipsStr.append("lw $t2, "+srcAddress2).append(System.getProperty("line.separator"))
          mipsStr.append(operation+" $t0, $t1, $t2").append(System.getProperty("line.separator"))
          mipsStr.append("sw $t0, "+destAddress).append(System.getProperty("line.separator"))
          mipsStr.append(System.getProperty("line.separator"))  
        } else if(instr(0) == "exit") {
          mipsStr.append("# exit").append(System.getProperty("line.separator"))
          mipsStr.append("li $v0, 10").append(System.getProperty("line.separator"))
          mipsStr.append("syscall").append(System.getProperty("line.separator"))
        }  
      }      
      i = i + 1
    }    
    mipsBuild.append(mipsStr)
    return mipsBuild
  }
    
  def makeMIPS (inputAddressCode: ListBuffer[AddressCodeBlocks], fileName: String): Unit = {
    var blockStr = BuildMIPSInit(inputAddressCode: ListBuffer[AddressCodeBlocks], fileName: String)   
    if(blockStr != null){  
      Console.println("MIPS code generated successfully !!!")
    }
  }
   
  def MIPSTemplate(blockStr: StringBuffer, action: Int): StringBuffer = {
    action match {
      case 1 => {         
        blockStr.append(".data").append(System.getProperty("line.separator"))
        blockStr.append("newline:  .asciiz \"\\n\" ").append(System.getProperty("line.separator"))
        blockStr.append(".text").append(System.getProperty("line.separator"))
        blockStr.append(".globl main").append(System.getProperty("line.separator"))
        blockStr.append("main:").append(System.getProperty("line.separator"))
        blockStr.append("li $fp, 0x7ffffffc").append(System.getProperty("line.separator"))        
        return blockStr
      } 
      case 0 => {
        return blockStr
      }
      case default => {
        throw InternalError("Some Internal Error occured !!!") 
      }      
    }
  }
  
  def getRegisterOffset(block: AddressCodeBlocks, regName: String): String = {
    var regVarNum = block.getNumOfDeclVar()
    var regType = regName.indexOf("_")
    
    if(regType == -1) {
      var regNum = regName.substring(1).toInt
      var offset = 0 - 4 * (regVarNum + regNum)
      return offset.toString()
    } else {      
      addToIdntReg(regName)
      var declVarCounter = searchIdntReg(regName) 
      var offset = 0 - 4 * (declVarCounter)
      return offset.toString()
    }    
  }
  
  def searchIdntReg(regName: String): Int = {
    var i = 0
    while(i < declVar.length) {
      if(declVar.apply(i) == regName) {
        return i
      }
      i = i + 1
    }
    return -1
  }
  
  def addToIdntReg(regName: String) = {
    if(searchIdntReg(regName) == -1) {
      declVar += regName
    } 
  }
  
}
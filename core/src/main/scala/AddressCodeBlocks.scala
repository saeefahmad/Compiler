package edu.utsa.cs5363
import scala.io.Source
import scala.util
import scala.collection.mutable.ListBuffer

class AddressCodeBlocks {
  var blockIdentifier: String = new String() 
  var instruction: ListBuffer[Array[String]] = new ListBuffer() 
  var jump: Any = null
  var cbr1: Any = null
  var cbr2: Any = null
  var index: Int = 1
  var numOfDeclaredVar: Int = 0
  
  def AddressCodeBlocks (identifier: String): AddressCodeBlocks = {
      blockIdentifier = identifier
      return this
  }
  
  def getIdentifier(): String = {
    return blockIdentifier
  }
  
  def setJump(jmpIndex: Any): AddressCodeBlocks = {
    jump = jmpIndex
    return this
  }
  
  def getJump(): Any = {
    return jump
  }
  
  def setNumOfDeclVar(value: Int): AddressCodeBlocks = {
    numOfDeclaredVar = value
    return this
  }
  
  def getNumOfDeclVar(): Int = {
    return numOfDeclaredVar
  }
  
  def setCbr1(cbrIndex: Int): AddressCodeBlocks = {
    cbr1 = cbrIndex
    return this
  }
  
  def getCbr1(): Any = {
    return cbr1
  }
  
  def setCbr2(cbrIndex: Int): AddressCodeBlocks = {
    cbr2 = cbrIndex
    return this
  }
  
  def getCbr2(): Any = {
    return cbr2
  }
  
  def setIndex(idx: Int): AddressCodeBlocks = {
    index = idx
    return this
  }
  
  def getIndex(): Int = {
    return index
  }
  
  def addInstruction (instr: Array[String]): AddressCodeBlocks = {
    instruction += instr
    return this
  }
  
  def getInstruction (): ListBuffer[Array[String]] = {
    return instruction
  }
  
}

package edu.utsa.cs5363
import scala.io.Source
import scala.util
import scala.collection.mutable.ListBuffer

class ASTNodes {
  var nodeIdentifier: String = new String() 
  var children: ListBuffer[ASTNodes] = new ListBuffer() 
  var parent: ASTNodes = null 
  var index: Int = 1
  var rIndex: Int = -1
  var visited: Boolean = false
  var typeOk: Any = null
  var typeCheck: String = null
  var isIdent: Boolean = false
  
  def ASTNodes(identifier: String): ASTNodes = {
      nodeIdentifier = identifier
      return this
  }
  
  def addIndex(nIndex: Int): ASTNodes = {
    index = nIndex
    return this
  }
  
  def addRIndex(index: Int): ASTNodes = {
    rIndex = index
    return this
  }
  
  def setVisit(bool: Boolean): ASTNodes = {
    visited = bool
    return this
  }
  
  def addChild(identifier: ASTNodes): ASTNodes = {
    children += identifier
    return this
  }
  
  def addParent(identifier: ASTNodes): ASTNodes = {
    parent = identifier
    return this
  }
  
  def getIndex(): Int = {
    return index
  }
  
  def getRIndex(): Int = {
    return rIndex
  }
  
  def getVisit(): Boolean = {
    return visited
  }
  
  def getIdentifier(): String = {
      return nodeIdentifier
  }

  def getChildren(): ListBuffer[ASTNodes] = {
      return children;
  }
  
  def getUnVisitedChild(): ASTNodes = {
    val children = getChildren()
    for (child <- children) {
      if (!child.getVisit()) {
        return child
      } 
    }
    return null;
  }
  
  def getVisitedChild(): ASTNodes = {
    val children = getChildren()
    for (child <- children) {
      if (child.getVisit()) {
        return child
      } 
    }
    return null;
  }

  def getParent(): ASTNodes = {
      return parent;
  }
  
  def hasChildren(): Boolean = {
      if (children.length == 0) return false
      else return true
  }
  
  def removeChild(): ASTNodes = {
    children = children.tail
    return this
  }
  
  def setTypeOk(bool: Boolean): ASTNodes = {
    typeOk = bool
    return this
  }
  
  def getTypeOk(): Any = {
    return typeOk
  }
  
  def setIsIdent(bool: Boolean): ASTNodes = {
    isIdent = bool
    return this
  }
  
  def getIsIdent(): Any = {
    return isIdent
  }
  
  def setType(typeName: String): ASTNodes = {
    typeCheck = typeName
    return this
  }
  
  def getType(): String = {
    return typeCheck
  }

}

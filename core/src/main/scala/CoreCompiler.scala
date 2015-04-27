package edu.utsa.cs5363

import scala.io.Source
import java.io

case class SyntaxError(msg: String) extends Exception(msg)
case class InternalError(msg: String) extends Exception(msg)

object CoreCompiler {
  
  def main(args: Array[String]) {      
    if (args.length == 0) {
      Console.err.println("No arguments.")
    }

    for (arg <- args 
         if !arg.endsWith(".tl")) {
    }

    for (arg <- args;
         fileNameStem = arg.replaceFirst(".tl$","")) {
      try {
        val sourceName = fileNameStem + ".tl"
        val parseTreeName = fileNameStem + ".pt.dot"
        val astName = fileNameStem + ".ast.dot"
        val ilocCFGName = fileNameStem + ".A3.cfg.dot"
        val mipsAsmName = fileNameStem + ".s"
        val source = Source.fromFile(sourceName)
        
        // Replace this with your with calls to your parser, etc.
        Console.println(s"\n *** $fileNameStem")
        for (line <- source.getLines) {
          Console.println(line)
        }
        
        var inputSource: Array[String] = Array()
        inputSource = readInputFile(sourceName)
        
        Parser.parse(inputSource)
        Parser.makeParseTree(inputSource, parseTreeName) 
        var ast = AST.makeAST(inputSource, astName)
//        var blocks = BuildAddressCode.makeCFG(ast, ilocCFGName)
//        BuildMipsCode.makeMIPS(blocks, mipsAsmName)

      } catch {
        case e: SyntaxError => {
          print(s"Syntax Error [$fileNameStem.tl]: ")
          println(e.getMessage())
        }
        case e: Exception => {
          print(s"Error processing [$fileNameStem.tl]: ")
          println(e.getMessage())
          e.printStackTrace()
        }
      }
    }
  }

  def readInputFile(FileName: String): Array[String] = {
    var inputSourceCode = ""
    val source = Source.fromFile(FileName)
    for (line <- source.getLines) {
          // Console.println(line)
          inputSourceCode = inputSourceCode + line + " "
    }
    val symbol: Array[String] = inputSourceCode.split("\\s+")
    return symbol
  }
  
  def writeOutputFile(fileName: String, str: String): Unit = {
    var writer = new java.io.BufferedWriter(new java.io.FileWriter(fileName, false))
    writer.write(str);
    writer.close();
    writer = null
  }
}


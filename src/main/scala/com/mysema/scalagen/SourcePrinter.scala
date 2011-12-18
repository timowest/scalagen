package com.mysema.scalagen

class SourcePrinter {

  private var level = 0

  private var indented = false

  private val buf = new StringBuilder()

  def indent() { 
    level += 1 
  }

  def unindent() { 
    level -= 1 
  }

  private def makeIndent() {
    for (i <- 0 until level) { buf.append("  ") }
  }

  def print(arg: String) {
    if (!indented) {
      makeIndent()
      indented = true
    }
    buf.append(arg)
  }

  def printLn(arg: String) {
    print(arg)
    printLn()
  }

  def printLn() {
    buf.append("\n")
    indented = false
  }

  def getSource(): String = buf.toString()

  override def toString(): String = getSource
  
}

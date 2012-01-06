/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */
package com.mysema.scalagen

/**
 * @author tiwe
 *
 */
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

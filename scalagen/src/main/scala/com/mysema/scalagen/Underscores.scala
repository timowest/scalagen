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

import java.util.ArrayList
import japa.parser.ast.CompilationUnit
import UnitTransformer._

object Underscores extends Underscores

/**
 * SerialVersionUID turns serialVersionUID fields into annotations
 */
class Underscores extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  // TODO : provide simpler variable name normalizations via UnitTransformerBase template methods
  
  override def visit(n: Variable, cu: CompilationUnit): Node = {         
    val nn = super.visit(n, cu).asInstanceOf[Variable]
    if (nn.getId.getName.startsWith("_")) {
      nn.getId.setName(nn.getId.getName.substring(1))
    }
    nn
  }
  
  override def visit(n: Name, cu: CompilationUnit): Node = {
    if (n.getName.startsWith("_")) {
      new Name(n.getName.substring(1))
    } else {
      n
    }
  }
  
  override def visit(n: FieldAccess, cu: CompilationUnit): Node = {
    val nn = super.visit(n, cu).asInstanceOf[FieldAccess]
    if (nn.getField.startsWith("_")) {
      nn.setField(nn.getField.substring(1))
    }
    nn
  }
}
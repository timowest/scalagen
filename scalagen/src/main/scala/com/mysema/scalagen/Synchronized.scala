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
import defs._
import japa.parser.ast.body.ModifierSet

object Synchronized extends Synchronized

/**
 * 
 */
class Synchronized extends ModifierVisitor[CompilationUnit] with UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(nn: Method, arg: CompilationUnit) = {
    val n = super.visit(nn, arg).asInstanceOf[Method]
    if (n.getModifiers.hasModifier(ModifierSet.SYNCHRONIZED)) {
      n.removeModifier(ModifierSet.SYNCHRONIZED)
      n.setBody(new SynchronizedStmt(null, n.getBody()))
    }
    n
  }
  
}  
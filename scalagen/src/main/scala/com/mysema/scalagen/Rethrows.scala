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

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
import japa.parser.ast.visitor._
import java.util.{ ArrayList, Collections }
import UnitTransformer._

object Rethrows extends Rethrows

class Rethrows extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, new Context()).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: TryStmt, arg: Context) = {
    if (n.getFinallyBlock == null && !isEmpty(n.getCatchs) && n.getCatchs.filter(isPrinted).isEmpty) {
      n.getTryBlock()
    } else {
      super.visit(n, arg)
    }
  }
  
  private def isPrinted(c: CatchClause): Boolean = {
    val block = c.getCatchBlock()
    isEmpty(block.getStmts) || block.getStmts.size > 1 || !block.getStmts.get(0).isInstanceOf[ThrowStmt]
  }
    
}
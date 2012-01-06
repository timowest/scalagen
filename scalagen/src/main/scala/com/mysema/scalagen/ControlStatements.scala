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

object ControlStatements extends ControlStatements

class ControlStatements extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, new Context()).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: ForStmt, arg: Context) = {
    // transform
    //   for (int i = 0; i < x; i++) block 
    // into
    //   for (i <- 0 until x) block
    if (n.getInit != null && n.getInit.size == 1 && n.getInit.get(0).isInstanceOf[VariableDeclaration]
     && n.getCompare.isInstanceOf[Binary] 
     && n.getCompare.asInstanceOf[Binary].getOperator.toString == "less"
     && n.getUpdate != null && n.getUpdate.size == 1 
     && n.getUpdate.get(0).isInstanceOf[Unary]
     && n.getUpdate.get(0).asInstanceOf[Unary].getOperator.toString.endsWith("Increment")) {
      val init = n.getInit.get(0).asInstanceOf[VariableDeclaration]
      val cmp = n.getCompare.asInstanceOf[Binary]
      var args = new ArrayList[Expression]()
      args.add(cmp.getRight)
      var until = new MethodCall(init.getVars.get(0).getInit, "until", args)
      init.getVars.get(0).setInit(null)
      new ForeachStmt(init, until, n.getBody())
    } else {
      n  
    }    
  }
  
  override def visit(n: SwitchEntryStmt, arg: Context) = {    
    // remove break
    val size = if (n.getStmts == null) 0 else n.getStmts.size
    if (size > 1 && n.getStmts.get(size-1).isInstanceOf[BreakStmt]) {
      n.getStmts.remove(size-1)
    }
    n
  }
  
}
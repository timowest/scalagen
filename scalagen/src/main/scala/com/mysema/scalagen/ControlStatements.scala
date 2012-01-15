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

import japa.parser.ast.visitor._
import java.util.ArrayList
import UnitTransformer._

object ControlStatements extends ControlStatements

/**
 * ControlStatements transform ForStmt, SwitchEntryStmt and If statements
 */
class ControlStatements extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
        
  override def visit(nn: For, arg: CompilationUnit): Node = {
    // transform
    //   for (int i = 0; i < x; i++) block 
    // into
    //   for (i <- 0 until x) block
    val n = super.visit(nn, arg).asInstanceOf[For]    
    n match {
      case For((init: VariableDeclaration) :: Nil, l lt r, incr(_) :: Nil, _) => {
        val until = new MethodCall(init.getVars.get(0).getInit, "until", r :: Nil)
        init.getVars.get(0).setInit(null)
        new Foreach(init, until, n.getBody)
      }
      case _ => n
    }
  }
  
  override def visit(nn: If, arg: CompilationUnit): Node = {
    // transform
    //   if (condition) target = x else target = y
    // into
    //   target = if (condition) e else y    
    val n = super.visit(nn, arg).asInstanceOf[If]    
    n match {
      case If(cond, Stmt(t1 set v1), Stmt(t2 set v2)) if t1 == t2 => {
        new ExpressionStmt(new Assign(t1, new Conditional(n.getCondition, v1, v2), Assign.assign))  
      }
      case _ => n
    }    
  }
  
  override def visit(nn: SwitchEntry, arg: CompilationUnit) = {    
    // remove break
    val n = super.visit(nn, arg).asInstanceOf[SwitchEntry]
    val size = if (n.getStmts == null) 0 else n.getStmts.size
    if (size > 1 && n.getStmts.get(size-1).isInstanceOf[Break]) {
      //n.getStmts.remove(size-1)
      n.setStmts(n.getStmts.dropRight(1))
    }
    n
  }
    
}
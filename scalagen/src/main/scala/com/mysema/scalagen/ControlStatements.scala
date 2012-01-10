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

import japa.parser.ast.{CompilationUnit, Node}
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
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
  
  // TODO : convert to use pattern matching
        
  override def visit(nn: ForStmt, arg: CompilationUnit): Node = {
    // transform
    //   for (int i = 0; i < x; i++) block 
    // into
    //   for (i <- 0 until x) block
    val n = super.visit(nn, arg).asInstanceOf[ForStmt]
    if (n.getInit != null && n.getInit.size == 1 && n.getInit.get(0).isInstanceOf[VariableDeclaration]
     && n.getCompare.isInstanceOf[Binary] 
     && n.getCompare.asInstanceOf[Binary].getOperator.toString == "less"
     && n.getUpdate != null && n.getUpdate.size == 1 
     && n.getUpdate.get(0).isInstanceOf[Unary]
     && n.getUpdate.get(0).asInstanceOf[Unary].getOperator.toString.endsWith("Increment")) {
      val init = n.getInit.get(0).asInstanceOf[VariableDeclaration]
      val cmp = n.getCompare.asInstanceOf[Binary]
      var until = new MethodCall(init.getVars.get(0).getInit, "until", cmp.getRight.asList)
      init.getVars.get(0).setInit(null)
      new ForeachStmt(init, until, n.getBody())
    } else {
      n  
    }    
  }
  
  override def visit(nn: IfStmt, arg: CompilationUnit): Node = {
    // transform
    //   if (contition) target = x else target = y
    // into
    //   target = if (condition) e else y    
    val n = super.visit(nn, arg).asInstanceOf[IfStmt]
    if (n.getElseStmt != null) {
      val thenStmt = extractStmt(n.getThenStmt)
      val elseStmt = extractStmt(n.getElseStmt)
      if (isAssignment(thenStmt) && isAssignment(elseStmt)) {
        val thenAssign = getAssignment(thenStmt)
        val elseAssign = getAssignment(elseStmt)
        if (thenAssign.getTarget == elseAssign.getTarget) {
          return new ExpressionStmt(new Assign(thenAssign.getTarget, 
              new Conditional(n.getCondition, thenAssign.getValue, elseAssign.getValue),
              AssignExpr.Operator.assign))
        }
      }
    } 
    super.visit(n, arg)    
  }
  
  override def visit(nn: SwitchEntryStmt, arg: CompilationUnit) = {    
    // remove break
    val n = super.visit(nn, arg).asInstanceOf[SwitchEntryStmt]
    val size = if (n.getStmts == null) 0 else n.getStmts.size
    if (size > 1 && n.getStmts.get(size-1).isInstanceOf[BreakStmt]) {
      n.getStmts.remove(size-1)
    }
    n
  }
    
}
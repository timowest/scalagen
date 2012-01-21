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
  
  private val toUnderscore = new ModifierVisitor[Set[String]] {
    
    override def visitName(n: String, arg: Set[String]): String = {
      if (arg.contains(n)) "_" else n
    }  
  }
  
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
  
  // TODO : maybe move this to own class
  override def visit(nn: Block, arg: CompilationUnit): Node = {
    // simplify
    //   for (format <- values if format.mimetype == contentType) return format
    //   defaultFormat
    // into
    //   values.find(_.mimetype == contenType).getOrElse(defaultFormat)
    val n = super.visit(nn, arg).asInstanceOf[Block]
    n match {
      case Block( 
          Foreach(v, it, If(cond, Return(rv1), null)) ::
          Return(rv2) :: Nil) => createFindCall(it, v, cond, rv1, rv2)
      case _ => n
    }
  }
  
  private def createFindCall(it: Expression, v: VariableDeclaration, 
      cond: Expression, rv1: Expression, rv2: Expression): Statement = {
    val vid = v.getVars.get(0).getId.toString
    val newCond = cond.accept(toUnderscore, Set(vid)).asInstanceOf[Expression]
    val newIt = it match {
      case MethodCall(_, "until", _ :: Nil) => new Enclosed(it)
      case _ => it
    }
    val findCall = new MethodCall(newIt, "find", newCond :: Nil)
    val expr = if (vid == rv1.toString) findCall
               else new MethodCall(findCall, "map", 
                          rv1.accept(toUnderscore, Set(vid)).asInstanceOf[Expression] :: Nil)  
    val getOrElse = new MethodCall(expr, "getOrElse", rv2 :: Nil)
    new Block(new ExpressionStmt(getOrElse) :: Nil)
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
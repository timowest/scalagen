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

import japa.parser.ast.visitor.ModifierVisitorAdapter
import UnitTransformer._
import java.util.ArrayList

object SimpleEquals extends SimpleEquals

/**
 * SimpleEquals simplfies equals method implementations
 */
class SimpleEquals extends UnitTransformerBase {
  
  private val returnFalse: Statement = new Return(new BooleanLiteral(false))
  
  private val replacer = new ModifierVisitor[(Name,Name)]() {    
    
    override def visit(n: Block, arg: (Name,Name)) = { 
      val visited = super.visit(n, arg)
      val matched = n match {
        case Block(Stmt(VariableDeclaration(_, Variable(newName, init) :: Nil)) :: Nil) if init == arg._1 => newName
        case _ => null
      }
      if (matched != null) {
        //n.getStmts.remove(0)
        n.setStmts(n.getStmts.drop(1))
        super.visit(n, (new Name(matched),arg._2))
      } else {
        super.visit(n, arg)
      }
    }
    
    override def visit(n: Enclosed, arg: (Name,Name)) = {
      super.visit(n, arg) match  {
       case Enclosed(n: Name) => n
       case o => o
      }
    }
    
    override def visit(n: Name, arg: (Name,Name)) = {
      if (n == arg._1) arg._2 else n
    }
    
    override def visit(n: Cast, arg: (Name,Name)) = {
      if (n.getExpr == arg._1) arg._2 else super.visit(n,arg) 
    }
  }
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: Method, arg: CompilationUnit) = {
    // transform
    //   if (obj == this) { true }
    //   else if (obj.isInstanceOf[Type]) { obj.asInstanceOf[Type].flag == flag }
    //   else { false }
    // into
    //   obj match {
    //     case obj: JoinFlag => obj.flag == flag
    //     case _ => false
    //   }
    n match {
      case Method("equals", Type.Boolean, Parameter(name) :: Nil, stmt) => {
        val converted = stmt match {
          // skip obj == this check
          case If(_ === This(_) | This(_) === _, Return(Literal(true)), 
               If(InstanceOf(_,t), action, Return(Literal(false)))) => createSwitch(name,t, action)
          case If(InstanceOf(_,t), action, Return(Literal(false)))  => createSwitch(name,t, action)
          case Return(InstanceOf(_,t) and cond) => createSwitch(name, t, new Return(cond))
          case _ => null
        }
        if (converted != null) {
          n.setBody(new Block(converted :: Nil)) 
        }
        n
      }
      case _ => n
    }
  }
  
  private def createSwitch(name: String, t: Type, action: Statement): Statement = {
    //  obj match {
    //    case obj: JoinFlag => obj.flag == flag
    //    case _ => false
    //  }    
    val selector = new Name(name)
    val simplified = action.accept(replacer, (selector,selector)).asInstanceOf[Statement]    
    val matches = new SwitchEntry(VariableDeclaration(-1,name,t), simplified :: Nil)
    val doesnt  = new SwitchEntry(null, returnFalse :: Nil)       
    new Switch(selector, matches :: doesnt :: Nil)
  }
  
    
}  
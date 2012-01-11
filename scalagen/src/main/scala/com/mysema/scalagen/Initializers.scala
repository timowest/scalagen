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
import java.util.ArrayList
import UnitTransformer._

object Initializers extends Initializers

/**
 * Initializers normalizes initializer blocks
 */
class Initializers extends UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    for (t <- cu.getTypes if t.getMembers != null) {
      transform(cu, t)
    }
    cu
  }
  
  private def transform(cu: CompilationUnit, t: Type) {    
    // transform sub types
    t.getMembers.collect { case t: Type => t}.foreach(t => transform(cu, t))    
    
    val initializers = t.getMembers.collect { case i: Initializer => i }
    if (!initializers.isEmpty) {
      val fields = t.getMembers.collect { case f: Field => f }
      val variables = fields.flatMap(_.getVariables).map(v => (v.getId.getName, v)).toMap
      
      for (i <- initializers) {
        val stmts = new java.util.HashSet[Statement]()
        for (stmt <- i.getBlock.getStmts) stmt match {
          case Stmt((t: Name) assign v) if variables.contains(t.getName) => {
            variables(t.getName).setInit(v)
            stmts.add(stmt)
          }
          case _ => 
        }
        i.getBlock.removeAll(stmts)
      }
      
      // remove empty initializers
      for (i <- initializers if i.getBlock.isEmpty) {
        t.getMembers.remove(i)
      }
    }
  }

//      for (i <- initializers) {
//        val stmts = new java.util.HashSet[Statement]()
//        for (stmt <- i.getBlock.getStmts if isAssignment(stmt)) {
//          val assign = getAssignment(stmt)
//          if (assign.getTarget.isInstanceOf[Name]) {
//            val namedTarget = assign.getTarget.asInstanceOf[Name]
//            if (variables.contains(namedTarget.getName)) {
//              variables(namedTarget.getName).setInit(assign.getValue)
//              stmts.add(stmt)
//            }
//          }
//        }  
  
}
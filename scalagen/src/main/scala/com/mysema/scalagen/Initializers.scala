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
import UnitTransformer._

object Initializers extends Initializers

/**
 * Initializers normalizes initializer blocks
 */
class Initializers extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(ci: ClassOrInterfaceDecl, cu: CompilationUnit): Node = {
    val t = super.visit(ci, cu).asInstanceOf[ClassOrInterfaceDecl]
    if (t.getMembers == null) {
      return t
    }
    
    val initializers = t.getMembers.collect { case i: Initializer => i }
    if (!initializers.isEmpty) {
      val fields = t.getMembers.collect { case f: Field => f }
      val variables = fields.flatMap(_.getVariables).map(v => (v.getId.getName, v)).toMap
      
      for (i <- initializers) {
        i.getBlock.setStmts(i.getBlock.getStmts.filter(_ match {
          case Stmt((t: Name) set v) if variables.contains(t.getName) => {
            variables(t.getName).setInit(v)
            false
          }
          case _ => true
        }))
      }
      
      // remove empty initializers
      val emptyInitializerBlocks = initializers.filter(_.getBlock.isEmpty)
      t.setMembers( t.getMembers.filterNot(emptyInitializerBlocks.contains) )      
    }
    t
  }
  
}
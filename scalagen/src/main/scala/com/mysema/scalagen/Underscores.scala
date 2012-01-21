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
 * Underscores strips off underscore prefixes from field names
 */
class Underscores extends UnitTransformerBase {
    
  private val nameReplacer = new ModifierVisitor[Set[String]] {
    
    override def visitName(n: String, arg: Set[String]): String = {
      if (arg.contains(n)) n.substring(1) else n
    }  
  }
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(n: ClassOrInterfaceDecl, cu: CompilationUnit): Node = {
    val variables = n.getMembers.collect { case f: Field => f }
      .flatMap( _.getVariables)
      .filter(_.getId.getName.startsWith("_"))
      .map(_.getId.getName).toSet
    n.accept(nameReplacer, variables)
  }
  
}
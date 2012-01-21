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
 * Underscores strips off underscore prefixes from field names with related Bean properties
 */
class Underscores extends UnitTransformerBase with BeanHelpers {
    
  private val nameReplacer = new ModifierVisitor[Set[String]] {
    
    override def visitName(n: String, arg: Set[String]): String = {
      if (arg.contains(n)) n.substring(1) else n
    }  
  }
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(n: ClassOrInterfaceDecl, cu: CompilationUnit): Node = {
    val getters = n.getMembers.collect { case m: Method => m }
      .filter(m => isBeanGetter(m) || isBooleanBeanGetter(m))
      .map(getProperty)      
    
    val variables = n.getMembers.collect { case f: Field => f }
      .flatMap( _.getVariables)
      .map(_.getId.getName)
      .filter(n => n.startsWith("_") && getters.contains(n.substring(1)))
      .toSet
      
    n.accept(nameReplacer, variables)
  }
  
}
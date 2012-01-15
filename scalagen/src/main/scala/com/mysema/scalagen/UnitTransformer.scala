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

import japa.parser.ast.Node
import japa.parser.ast.body._
import japa.parser.ast.expr._
import japa.parser.ast.stmt._
import japa.parser.ast.`type`._
import japa.parser.ast.visitor.ModifierVisitorAdapter
import japa.parser.ast.CompilationUnit
import japa.parser.ast.ImportDeclaration
import java.util.ArrayList

object UnitTransformer extends Helpers with Types {
  
  @inline
  implicit def toNameExpr(s: String) = new NameExpr(s)
  
  @inline
  implicit def toVariableDeclaratorId(s: String) = new VariableDeclaratorId(s)
  
  @inline 
  implicit def toBlock(s: Statement) = new Block(s :: Nil)
  
  private def safeToString(obj: AnyRef): String = if (obj != null) obj.toString else null
            
  val BEAN_PROPERTY_IMPORT = new Import("scala.reflect.BeanProperty", false, false)

  val BEAN_PROPERTY = new MarkerAnnotation("BeanProperty")
      
  abstract class UnitTransformerBase extends ModifierVisitor[CompilationUnit] with UnitTransformer {
        
    override def visit(n: CompilationUnit, arg: CompilationUnit): Node = {
      val rv = new CompilationUnit()
      rv.setPackage(filter(n.getPackage, arg))
      rv.setImports(filter(n.getImports, arg))
      // arg is replaced with converted instance here
      rv.setTypes(filter(n.getTypes, rv)) 
      rv
    }
  }
  
}

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
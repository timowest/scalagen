/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mysema.scalagen 

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.visitor.ModifierVisitorAdapter
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.ImportDeclaration
import java.util.ArrayList

object UnitTransformer extends Helpers with Types {
  
  @inline
  implicit def toNameExpr(s: String) = new NameExpr(s)
  
  @inline
  implicit def toVariableDeclaratorId(s: String) = new VariableDeclaratorId(s)
  
  @inline 
  implicit def toBlock(s: Statement) = new Block(s :: Nil)
  
  private def safeToString(obj: AnyRef): String = if (obj != null) obj.toString else null
            
  //val BOOLEAN_BEAN_PROPERTY_IMPORT = new Import("scala.reflect.BooleanBeanProperty", false, false)
  
  val BEAN_PROPERTY = new MarkerAnnotation("BeanProperty")
  
  val BOOLEAN_BEAN_PROPERTY = new MarkerAnnotation("BooleanBeanProperty")
      
  abstract class UnitTransformerBase extends ModifierVisitor[CompilationUnit] with UnitTransformer {
        
    override def visit(n: CompilationUnit, arg: CompilationUnit): Node = withCommentsFrom(n, arg) {
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
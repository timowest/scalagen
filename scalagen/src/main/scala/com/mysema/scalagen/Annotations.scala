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
import japa.parser.ast.visitor._
import java.util.{ List => JavaList, ArrayList, Collections }
import UnitTransformer._
import japa.parser.ast.`type`.ClassOrInterfaceType

object Annotations extends Annotations

class Annotations extends UnitTransformerBase {
  
  private val staticAnnotationType = new ClassOrInterfaceType("StaticAnnotation")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, new Context()).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: AnnotationDeclaration, arg: Context) = {
    val clazz = new ClassOrInterface()
    clazz.setName(n.getName)    
    clazz.setExtends(createExtends())
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createExtends(): JavaList[ClassOrInterfaceType] = {
    val list = new ArrayList[ClassOrInterfaceType]()
    list.add(staticAnnotationType)
    list
  }
  
  private def createMembers(n: AnnotationDeclaration): JavaList[BodyDeclaration] = {
    // TODO : default values
    val params = n.getMembers.collect { case m: AnnotationMember => m }
      .map(m => new Parameter(0, m.getType, new VariableDeclaratorId(m.getName)))
      
    val members = new ArrayList[BodyDeclaration]()
    if (!params.isEmpty) {
      val constructor = new Constructor()
      val mutableParams = new ArrayList[Parameter]
      mutableParams.addAll(params)
      constructor.setParameters(mutableParams)
      constructor.setBlock(new BlockStmt())
      members.add(constructor)
    }
    members
  }
    
}  
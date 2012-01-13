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
import japa.parser.ast.visitor.ModifierVisitorAdapter
import UnitTransformer._

object Annotations extends Annotations

/**
 * Annotations turns Annotation type declarations into normal classes which extend
 * StaticAnnotation
 */
class Annotations extends UnitTransformerBase {
  
  private val staticAnnotationType = new ClassOrInterface("StaticAnnotation")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: AnnotationDecl, arg: CompilationUnit) = {
    // turns annotations into StaticAnnotation subclasses
    val clazz = new ClassOrInterfaceDecl()
    clazz.setName(n.getName)    
    clazz.setExtends(staticAnnotationType.asList)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: AnnotationDecl): JavaList[BodyDecl] = {
    // TODO : default values
    val params = n.getMembers.collect { case m: AnnotationMember => m }
      .map(m => new Parameter(PROPERTY, m.getType, new VariableDeclaratorId(m.getName)))
      
    val members = new ArrayList[BodyDecl]()
    if (!params.isEmpty) {
      val constructor = new Constructor()
      val mutableParams = new ArrayList[Parameter]
      mutableParams.addAll(params)
      constructor.setParameters(mutableParams)
      constructor.setBlock(new Block())
      members.add(constructor)
    }
    members
  }
    
}  
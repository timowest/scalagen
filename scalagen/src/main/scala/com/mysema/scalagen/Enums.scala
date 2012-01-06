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

object Enums extends Enums

class Enums extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, new Context()).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: EnumDeclaration, arg: Context) = {
    val clazz = new ClassOrInterface()
    clazz.setName(n.getName)
    clazz.setModifiers(OBJECT)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: EnumDeclaration): JavaList[BodyDeclaration] = {
    val typeDecl = new ClassOrInterface(0, false, n.getName)
    typeDecl.setImplements(n.getImplements)
    typeDecl.setMembers(n.getMembers)
    
    val ty = new ClassOrInterfaceType(n.getName)
    val entries = n.getEntries.map(e => {
      val init = new ObjectCreationExpr(null, ty, e.getArgs)
      new FieldDeclaration(ModifierSet.FINAL, ty, new VariableDeclarator(e.getName, init)) })
        
    val members = new ArrayList[BodyDeclaration]()
    members.add(typeDecl)
    members.addAll(entries)    
    members
  }
    
}  
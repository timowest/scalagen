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

/**
 * Enums converts Java enum type declarations into Scala enumerations
 */
class Enums extends UnitTransformerBase {
  
  private val enumerationType = new ClassOrInterfaceType("Enumeration")
  
  private val valType = new ClassOrInterfaceType("Val")
  
  private val valueType = new ClassOrInterfaceType("Value")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, new Context()).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: EnumDeclaration, arg: Context) = {
    val clazz = new ClassOrInterface()
    clazz.setExtends(enumerationType.asList)
    clazz.setName(n.getName)
    clazz.setModifiers(OBJECT)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: EnumDeclaration): JavaList[BodyDeclaration] = {
    val typeDecl = new ClassOrInterface(0, false, n.getName)
    typeDecl.setExtends(valType.asList)
    typeDecl.setImplements(n.getImplements)
    typeDecl.setMembers(new ArrayList[BodyDeclaration])
    typeDecl.getMembers.addAll(n.getMembers.filterNot(isStatic))
    
    // entries
    val ty = new ClassOrInterfaceType(n.getName)
    val entries = n.getEntries.map(e => {
      val init = new ObjectCreationExpr(null, ty, e.getArgs)
      new FieldDeclaration(ModifierSet.FINAL, ty, new VariableDeclarator(e.getName, init)) })
        
    // conversion function
    val conversion = new MethodDeclaration(IMPLICIT, ty, "convertValue")
    conversion.setBody(new Return(new CastExpr(ty, "v")))
    conversion.setParameters(new Parameter(valueType, "v").asList)
          
    val members = new ArrayList[BodyDeclaration]()
    members.addAll(entries)
    members.add(typeDecl)       
    members.addAll(n.getMembers.filter(isStatic))
    members.add(conversion)
    members
  }
    
}  
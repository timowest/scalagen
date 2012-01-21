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

import japa.parser.ast.body.ModifierSet
import japa.parser.ast.visitor._
import java.util.ArrayList
import UnitTransformer._

object Enums extends Enums

/**
 * Enums converts Java enum type declarations into Scala enumerations
 */
class Enums extends UnitTransformerBase {
  
  private val enumerationType = new ClassOrInterface("Enumeration")
  
  private val valType = new ClassOrInterface("Val")
  
  private val valueType = new ClassOrInterface("Value")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }   
    
  override def visit(n: EnumDecl, arg: CompilationUnit) = {
    // transform enums into Scala Enumerations
    val clazz = new ClassOrInterfaceDecl()
    clazz.setExtends(enumerationType :: Nil)
    clazz.setName(n.getName)
    clazz.setModifiers(OBJECT)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: EnumDecl): JavaList[BodyDecl] = {
    val typeDecl = new ClassOrInterfaceDecl(0, false, n.getName)
    typeDecl.setExtends(valType :: Nil)
    typeDecl.setImplements(n.getImplements)
    typeDecl.setMembers(n.getMembers.filterNot(isStatic))
    
    // entries
    val ty = new ClassOrInterface(n.getName)
    val entries = n.getEntries.map(e => {
      val init = new ObjectCreation(null, ty, e.getArgs)
      new Field(ModifierSet.FINAL, ty, new Variable(e.getName, init)) })
        
    // conversion function
    val conversion = new Method(IMPLICIT, ty, "convertValue")
    conversion.setBody(new Return(new Cast(ty, "v")))
    conversion.setParameters(new Parameter(valueType, "v") :: Nil)
          
//    val members = new ArrayList[BodyDecl]()
//    members.addAll(entries)
//    members.add(typeDecl)       
//    members.addAll(n.getMembers.filter(isStatic))
//    members.add(conversion)
//    members
    
    entries ::: typeDecl :: n.getMembers.filter(isStatic) ::: conversion :: Nil
  }
    
}  
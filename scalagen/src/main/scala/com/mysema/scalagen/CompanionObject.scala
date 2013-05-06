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

object CompanionObject extends CompanionObject

/**
 * CompanionObject moves static members into companion objects
 */
// TODO : use ModifierVisitorAdapter
// TODO : get totally rid of mutable lists
class CompanionObject extends UnitTransformer {

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      val types = cu.getTypes.filter(!_.getModifiers.isObject)
      val cuTypes = new ArrayList[TypeDecl](cu.getTypes)
      handleTypes(cu, types, cuTypes)
      cu.setTypes(cuTypes)
    }
    cu
  }
  
  private def handleTypes(cu: CompilationUnit, types: Seq[TypeDecl], members: JavaList[_ >: TypeDecl]) {    
    types.foreach { t => handleType(cu,t) }
    
    // get companion objects
    val typeToCompanion = types.map(t => (t, getCompanionObject(t)))
      .filter(_._2 != null).toMap
       
    for ( (cl, companion) <- typeToCompanion) {
      handleClassAndCompanion(cu, members, cl, companion)
    }   
  }
    
  private def handleType(cu: CompilationUnit, clazz: TypeDecl) {
    if (clazz.getMembers != null) {
      val types = clazz.getMembers.collect { case t: TypeDecl => t }
        .filter(!_.getModifiers.isObject)    
      val members = new ArrayList[BodyDecl](clazz.getMembers)
      handleTypes(cu, types, members)
      clazz.setMembers(members)
    }   
  }  
  
  private def handleClassAndCompanion(cu: CompilationUnit, members: JavaList[_ >: TypeDecl], 
      clazz: TypeDecl, companion: TypeDecl) {
    // add companion
    members.add(members.indexOf(clazz), companion)
    if (clazz.getMembers.isEmpty) {
      members.remove(clazz)
    } else if (clazz.getMembers.size == 1) {
      clazz.getMembers.get(0) match {
        case c: Constructor => {
          // remove private empty constructor
          if (c.getModifiers.isPrivate && isEmpty(c.getParameters)) {
            members.remove(clazz)
          } 
        }
        case _ => 
      }
    }

    // add import for companion object members, if class has not been removed
    if (members.contains(clazz)) {
      cu.setImports(cu.getImports :+ new Import(clazz.getName, false, true))
    }
  }

  private def getCompanionObject(t: TypeDecl): TypeDecl = {
    if (t.getMembers == null) {
      return null
    }
    
    val staticMembers = t.getMembers.filter(isStatic)
    if (!staticMembers.isEmpty) {
      t.setMembers(t.getMembers.filterNot(staticMembers.contains))
      var companion = new ClassOrInterfaceDecl(OBJECT, false, t.getName)
      companion.setMembers(staticMembers)
      companion
    } else {
      null
    }
  }
  

}

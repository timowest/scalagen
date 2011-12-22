package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body.ModifierSet
import java.util.ArrayList
import java.util.HashMap
import java.util.Map
import UnitTransformer._

/**
 * @author tiwe
 *
 */
object CompanionObject extends UnitTransformer {

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes == null) {
      return cu
    }
        
    val typeToCompanion = cu.getTypes.map(t => (t, getCompanionObject(cu,t)))
      .filter(_._2 != null)
      .toMap
      
    if (!typeToCompanion.isEmpty && cu.getImports == null) {
      cu.setImports(new ArrayList[Import]())
    }
      
    for ( (clazz, companion) <- typeToCompanion) {
      handleClassAndCompanion(cu, clazz, companion)
    }    
    cu
  }
  
  private def handleClassAndCompanion(cu: CompilationUnit, clazz: Type, companion: Type) {
    cu.getTypes.add(cu.getTypes.indexOf(clazz), companion)
    if (clazz.getMembers.isEmpty) {
      cu.getTypes.remove(clazz)
    } else if (clazz.getMembers.size == 1) {
      clazz.getMembers.get(0) match {
        case c: Constructor => {
          if (c.getModifiers.isPrivate && isEmpty(c.getParameters)) {
            cu.getTypes.remove(clazz)
          } 
        }
        case _ => 
      }
    }

    // add import for companion object members, if class has not been removed
    if (cu.getTypes.contains(clazz)) {
      var importDecl = new Import(clazz.getName, false, true)
      cu.getImports.add(importDecl)
    }
  }

  private def getCompanionObject(cu: CompilationUnit, t: Type): Type = {
    if (t.getMembers == null) {
      return null
    }
    
    var companion = new ClassOrInterface(t.getModifiers, false, " " + t.getName)
    companion.setMembers(new ArrayList[Body]())
   
    // add static members to class
    for (member <- t.getMembers) {
      val add = member match {
        case f: Field => f.getModifiers.isStatic
        case m: Method => m.getModifiers.isStatic
        case i: Initializer => i.isStatic
        case _ => false
      }
      if (add) {
        companion.getMembers.add(member)
      }
    }
    
    // return companion object, if not empty
    if (!companion.getMembers.isEmpty) {
      // remove companion members from class
      for (member <- companion.getMembers) {
        t.getMembers.remove(member)
      }
      companion
    } else {
      null
    }
  }
}

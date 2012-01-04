package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body.ModifierSet
import java.util.ArrayList
import java.util.HashMap
import java.util.Map
import UnitTransformer._

object CompanionObject extends CompanionObject

class CompanionObject extends UnitTransformer {

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes == null) {
      return cu
    }
    
    // track lower level companion objects
    cu.getTypes.foreach { t => handleType(cu,t) }
        
    // track top level companions
    val typeToCompanion = cu.getTypes.map(t => (t, getCompanionObject(t)))
      .filter(_._2 != null).toMap
      
    if (!typeToCompanion.isEmpty && cu.getImports == null) {
      cu.setImports(new ArrayList[Import]())
    }
      
    for ( (clazz, companion) <- typeToCompanion) {
      handleClassAndCompanion(cu, cu.getTypes, clazz, companion)
    }    
    cu
  }
  
  private def handleType(cu: CompilationUnit, clazz: Type) {
    if (clazz.getMembers == null) {
      return
    }
    
    val types = clazz.getMembers.collect { case t: Type => t }
    
    types.foreach { t => handleType(cu,t) }
    
    val typeToCompanion = types.map(t => (t, getCompanionObject(t)))
      .filter(_._2 != null).toMap
      
    if (!typeToCompanion.isEmpty && cu.getImports == null) {
      cu.setImports(new ArrayList[Import]())
    }  
    
    for ( (cl, companion) <- typeToCompanion) {
      handleClassAndCompanion(cu, clazz.getMembers, cl, companion)
    }   
  }
  
  private def handleClassAndCompanion(cu: CompilationUnit, members: java.util.List[_ >: Type], clazz: Type, companion: Type) {
    members.add(members.indexOf(clazz), companion)
    if (clazz.getMembers.isEmpty) {
      members.remove(clazz)
    } else if (clazz.getMembers.size == 1) {
      clazz.getMembers.get(0) match {
        case c: Constructor => {
          if (c.getModifiers.isPrivate && isEmpty(c.getParameters)) {
            members.remove(clazz)
          } 
        }
        case _ => 
      }
    }

    // add import for companion object members, if class has not been removed
    if (members.contains(clazz)) {
      var importDecl = new Import(clazz.getName, false, true)
      cu.getImports.add(importDecl)
    }
  }

  private def getCompanionObject(t: Type): Type = {
    if (t.getMembers == null) {
      return null
    }
    
    var companion = new ClassOrInterface(t.getModifiers, false, " " + t.getName)
    companion.setMembers(new ArrayList[Body]())
   
    // add static members to class
    for (member <- t.getMembers) {
      val add = member match {
        case t: Type => t.getModifiers.isStatic
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

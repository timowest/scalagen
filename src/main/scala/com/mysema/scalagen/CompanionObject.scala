package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.ImportDeclaration
import japa.parser.ast.body.BodyDeclaration
import japa.parser.ast.body.ClassOrInterfaceDeclaration
import japa.parser.ast.body.ConstructorDeclaration
import japa.parser.ast.body.FieldDeclaration
import japa.parser.ast.body.InitializerDeclaration
import japa.parser.ast.body.MethodDeclaration
import japa.parser.ast.body.ModifierSet
import japa.parser.ast.body.TypeDeclaration
import japa.parser.ast.expr.NameExpr
import java.util.ArrayList
import java.util.HashMap
import java.util.Map

/**
 * @author tiwe
 *
 */
object CompanionObject extends UnitTransformer {

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes == null) {
      return cu
    }
        
    val typeToCompanion = cu.getTypes.map(t => (t, withCompanionObject(cu,t)))
      .filter(_._2 != null)
      .toMap
      
    if (!typeToCompanion.isEmpty && cu.getImports == null) {
      cu.setImports(new ArrayList[ImportDeclaration]())
    }
      
    for ( (clazz,companion) <- typeToCompanion) {
      handleClassAndCompanion(cu, clazz, companion)
    }    
    cu
  }
  
  private def handleClassAndCompanion(cu: CompilationUnit, clazz: TypeDeclaration, companion: TypeDeclaration) {
    cu.getTypes.add(cu.getTypes.indexOf(clazz), companion)
    if (clazz.getMembers.isEmpty()) {
      cu.getTypes.remove(clazz)
    } else if (clazz.getMembers.size == 1) {
      clazz.getMembers.get(0) match {
        case c: ConstructorDeclaration => {
          if (c.getModifiers.isPrivate && isEmpty(c.getParameters)) {
            cu.getTypes.remove(clazz)
          } 
        }
        case _ => 
      }
    }

    // add import for companion object members, if class has not been removed
    if (cu.getTypes.contains(clazz)) {
      var importDecl = new ImportDeclaration(new NameExpr(clazz.getName), false, true)
      cu.getImports.add(importDecl)
    }
  }

  private def withCompanionObject(cu: CompilationUnit, t: TypeDeclaration): TypeDeclaration = {
    if (t.getMembers == null) {
      return null
    }
    
    var companion = new ClassOrInterfaceDeclaration(t.getModifiers, false, " " + t.getName)
    companion.setMembers(new ArrayList[BodyDeclaration]())
   
    // add static members to class
    for (member <- t.getMembers) {
      val add = member match {
        case f: FieldDeclaration => f.getModifiers.isStatic
        case m: MethodDeclaration => m.getModifiers.isStatic
        case i: InitializerDeclaration => i.isStatic
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

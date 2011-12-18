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
    var typeToCompanion = new HashMap[TypeDeclaration, TypeDeclaration]()
    
    if (cu.getTypes != null) {
      for (t <- cu.getTypes) {
        var companion = withCompanionObject(cu, t)
        if (companion != null) {
          typeToCompanion.put(t, companion)
        }
      }
      
      if (!typeToCompanion.isEmpty() && cu.getImports == null) {
        cu.setImports(new ArrayList[ImportDeclaration]())
      }
      
      // remove classes
      for (entry <- typeToCompanion.entrySet()) {
        cu.getTypes.add(cu.getTypes.indexOf(entry.getKey), entry.getValue)
        if (entry.getKey.getMembers.isEmpty()) {
          cu.getTypes.remove(entry.getKey)
        } else if (entry.getKey.getMembers.size == 1) {
          entry.getKey.getMembers.get(0) match {
            case c: ConstructorDeclaration => {
              if (c.getModifiers.isPrivate && isEmpty(c.getParameters)) {
                cu.getTypes.remove(entry.getKey)
              } 
            }
            case _ => 
          }
        }

        // add import for companion object members, if class has not been removed
        if (cu.getTypes.contains(entry.getKey)) {
          var importDecl = new ImportDeclaration(new NameExpr(entry.getKey.getName), false, true)
          cu.getImports.add(importDecl)
        }
      }
    }
    cu
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

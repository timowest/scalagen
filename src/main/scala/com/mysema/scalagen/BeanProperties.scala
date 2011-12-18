package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.ImportDeclaration
import japa.parser.ast.body.BodyDeclaration
import japa.parser.ast.body.FieldDeclaration
import japa.parser.ast.body.MethodDeclaration
import japa.parser.ast.body.ModifierSet
import japa.parser.ast.body.TypeDeclaration
import japa.parser.ast.body.VariableDeclarator
import japa.parser.ast.expr.AnnotationExpr
import japa.parser.ast.expr.MarkerAnnotationExpr
import japa.parser.ast.expr.NameExpr
import japa.parser.ast.`type`.PrimitiveType
import japa.parser.ast.`type`.PrimitiveType.Primitive
import japa.parser.ast.`type`.VoidType
import java.util.ArrayList
import java.util.HashMap
import java.util.Map
import com.mysema.scala.BeanUtils
import scala.collection.JavaConversions._

object BeanProperties extends UnitTransformer {

  private val BEAN_PROPERTY_IMPORT = new ImportDeclaration(new NameExpr("scala.reflect.BeanProperty"), false, false)

  private val BEAN_PROPERTY = new MarkerAnnotationExpr(new NameExpr("BeanProperty"))

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      cu.getTypes.foreach { transform(cu,_) }
    }
    cu
  }

  private def transform(cu: CompilationUnit, t: TypeDeclaration) {
    if (t.getMembers == null) {
      return 
    }
    var fields = new HashMap[String, FieldDeclaration]()
    var getters = new HashMap[String, MethodDeclaration]()
    var setters = new HashMap[String, MethodDeclaration]()
    
    for (member <- t.getMembers) {
      if (member.isInstanceOf[ FieldDeclaration]) {
        var field = member.asInstanceOf[FieldDeclaration]
        field.getVariables.foreach { v => fields.put(v.getId.getName, field) }        
      } else if (member.isInstanceOf[ MethodDeclaration]) {
        var method = member.asInstanceOf[MethodDeclaration]
        if (method.getName.startsWith("get") 
            && !method.getModifiers.isPrivate 
            && (method.getParameters == null || method.getParameters.isEmpty) 
            && !(method.getType.isInstanceOf[VoidType])) {
          getters.put(BeanUtils.uncapitalize(method.getName.substring(3)), method)
        } else if (method.getName.startsWith("is") 
            && !method.getModifiers.isPrivate 
            && (method.getParameters == null || method.getParameters.isEmpty) 
            && method.getType.isInstanceOf[PrimitiveType] 
            && (method.getType.asInstanceOf[PrimitiveType]).getType == Primitive.Boolean) {
          getters.put(BeanUtils.uncapitalize(method.getName.substring(2)), method)
        } else if (method.getName.startsWith("set") 
            && (method.getParameters != null && method.getParameters.size == 1) 
            && method.getType.isInstanceOf[ VoidType]) {
          setters.put(BeanUtils.uncapitalize(method.getName.substring(3)), method)
        }
      }
    }
    
    var foundProperties = false    
    for (entry <- fields.entrySet()) {
      var field = entry.getValue
      var getter = getters.get(entry.getKey)
      if (getter != null) {
        foundProperties = true
        t.getMembers.remove(getter)
        var setter = setters.get(entry.getKey)
        if (setter != null) {
          t.getMembers.remove(setter)
        }
        field.setModifiers(field.getModifiers.removeModifier(ModifierSet.PRIVATE))
        if (field.getAnnotations == null) {
          field.setAnnotations(new ArrayList[AnnotationExpr]())
        }
        if (!field.getAnnotations.contains(BEAN_PROPERTY)) {
          field.getAnnotations.add(BEAN_PROPERTY)
        }
      }
    }
    
    // add BeanProperty imports, if properties have been found
    if (foundProperties) {
      if (cu.getImports == null) {
        cu.setImports(new ArrayList[ImportDeclaration]())
      }
      if (!cu.getImports.contains(BEAN_PROPERTY_IMPORT)) {
        cu.getImports.add(BEAN_PROPERTY_IMPORT)
      }
    }
  }
}

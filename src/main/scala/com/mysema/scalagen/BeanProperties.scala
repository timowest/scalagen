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
import com.mysema.scala.BeanUtils

/**
 * @author tiwe
 *
 */
object BeanProperties extends UnitTransformer {
  
  val BEAN_PROPERTY_IMPORT = new ImportDeclaration(new NameExpr("scala.reflect.BeanProperty"), false, false)

  val BEAN_PROPERTY = new MarkerAnnotationExpr(new NameExpr("BeanProperty"))

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      cu.getTypes.foreach { transform(cu,_) }
    }
    cu
  }
  
  private def isGetter(method: MethodDeclaration): Boolean = {
    method.getName.startsWith("get") && !method.getModifiers.isPrivate && 
    isEmpty(method.getParameters) && 
    !(method.getType.isInstanceOf[VoidType])
  }
  
  private def isBooleanGetter(method: MethodDeclaration): Boolean = {
    method.getName.startsWith("is") && !method.getModifiers.isPrivate && 
    isEmpty(method.getParameters) && 
    method.getType.isInstanceOf[PrimitiveType] && 
    (method.getType.asInstanceOf[PrimitiveType]).getType == Primitive.Boolean
  }
  
  private def isSetter(method: MethodDeclaration): Boolean = {
    method.getName.startsWith("set") && 
    (method.getParameters != null && method.getParameters.size == 1) && 
    method.getType.isInstanceOf[VoidType]
  }

  private def transform(cu: CompilationUnit, t: TypeDeclaration) {
    if (t.getMembers == null) {
      return 
    }
        
    // accessors
    val methods = t.getMembers.collect { case m: MethodDeclaration => m }
    val getters = methods.filter(m => isGetter(m) || isBooleanGetter(m))
      .map(m => (BeanUtils.uncapitalize(m.getName.substring(if (isGetter(m)) 3 else 2)),m)).toMap      
    val setters = methods.filter(m => isSetter(m))
      .map(m => (BeanUtils.uncapitalize(m.getName.substring(3)), m)).toMap
   
    // fields with accessors
    val fields = t.getMembers.collect { case f: FieldDeclaration => f }
      .filter(_.getModifiers.isPrivate)
      .flatMap( f => f.getVariables.map( v => (v.getId.getName,f) ))
      .filter { case (field,f) =>  getters.contains(field) }
      .toMap
          
    // remove accessors 
    for ( (name,field) <- fields) {
      var getter = getters(name)
      t.getMembers.remove(getter)
      setters.get(name).foreach { t.getMembers.remove(_) }

      // make field public
      field.setModifiers(field.getModifiers.removeModifier(ModifierSet.PRIVATE))
      if (field.getAnnotations == null) {
        field.setAnnotations(new ArrayList[AnnotationExpr]())
      }
      if (!field.getAnnotations.contains(BEAN_PROPERTY)) {
        field.getAnnotations.add(BEAN_PROPERTY)
      }
    }
    
    // add BeanProperty import, if properties have been found
    if (!fields.isEmpty) {
      if (cu.getImports == null) {
        cu.setImports(new ArrayList[ImportDeclaration]())
      }
      if (!cu.getImports.contains(BEAN_PROPERTY_IMPORT)) {
        cu.getImports.add(BEAN_PROPERTY_IMPORT)
      }
    }
  }
}

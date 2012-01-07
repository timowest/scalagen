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
import japa.parser.ast.body.ModifierSet
import japa.parser.ast.body.VariableDeclarator
import japa.parser.ast.`type`.PrimitiveType
import japa.parser.ast.`type`.PrimitiveType.Primitive
import japa.parser.ast.`type`.VoidType
import japa.parser.ast.stmt.{ ExpressionStmt, Statement }
import java.util.ArrayList
import com.mysema.scala.BeanUtils
import UnitTransformer._

object BeanProperties extends BeanProperties

/**
 * BeanProperties turns field + accessor combinations into @BeanPropert annotated 
 * Scala properties
 */
class BeanProperties extends UnitTransformer {
    
  def transform(cu: CompilationUnit): CompilationUnit = {
    for (t <- cu.getTypes if t.getMembers != null) {
      transform(cu, t)
    }
    cu
  }
  
  private def isGetter(method: Method): Boolean = { 
    method.getName.startsWith("get") && !method.getModifiers.isPrivate && 
    isEmpty(method.getParameters) && 
    !(method.getType.isInstanceOf[VoidType]) &&
    method.getBody != null && method.getBody.size == 1 &&
    isReturnFieldStmt(method.getBody()(0))
  }
  
  private def isBooleanGetter(method: Method): Boolean = {
    method.getName.startsWith("is") && !method.getModifiers.isPrivate && 
    isEmpty(method.getParameters) && 
    method.getType.isInstanceOf[PrimitiveType] && 
    (method.getType.asInstanceOf[PrimitiveType]).getType == Primitive.Boolean &&
    method.getBody != null && method.getBody.size == 1 && 
    isReturnFieldStmt(method.getBody()(0))
  }
    
  private def isSetter(method: Method): Boolean = {
    method.getName.startsWith("set") && 
    (method.getParameters != null && method.getParameters.size == 1) && 
    method.getType.isInstanceOf[VoidType] &&
    method.getBody != null && method.getBody.size == 1 && 
    isSetFieldStmt(method.getBody()(0))
  }
    
  private def isReturnFieldStmt(stmt: Statement): Boolean = stmt match {
    case r: Return => r.getExpr.isInstanceOf[Name] || r.getExpr.isInstanceOf[FieldAccess]
    case _ => false
  }
  
  private def isSetFieldStmt(stmt: Statement): Boolean = stmt match {
    case e: ExpressionStmt => e.getExpression.isInstanceOf[Assign] && 
      e.getExpression.asInstanceOf[Assign].getOperator.toString == "assign"
    case _ => false
  }
  
  private def transform(cu: CompilationUnit, t: Type) {
    // transform sub types
    t.getMembers.collect { case t: Type => t}.foreach(t => transform(cu, t))
    
    // accessors
    val methods = t.getMembers.collect { case m: Method => m }
    val getters = methods.filter(m => isGetter(m) || isBooleanGetter(m))
      .map(m => (BeanUtils.uncapitalize(m.getName.substring(if (isGetter(m)) 3 else 2)),m)).toMap      
    val setters = methods.filter(m => isSetter(m))
      .map(m => (BeanUtils.uncapitalize(m.getName.substring(3)), m)).toMap
   
    // fields with accessors
    val fields = t.getMembers.collect { case f: Field => f }
      .filter(_.getModifiers.isPrivate)
      .flatMap( f => f.getVariables.map( v => (v.getId.getName,f) ))
      .filter { case (field,_) =>  getters.contains(field) }
      .toMap
          
    // remove accessors 
    for ( (name,field) <- fields) {
      var getter = getters(name)
      t.getMembers.remove(getter)
      setters.get(name).foreach { t.getMembers.remove(_) }

      // make field public
      field.setModifiers(field.getModifiers.removeModifier(ModifierSet.PRIVATE))
      if (field.getAnnotations == null) {
        field.setAnnotations(new ArrayList[Annotation]())
      }
      if (!field.getAnnotations.contains(BEAN_PROPERTY)) {
        field.getAnnotations.add(BEAN_PROPERTY)
      }
    }
    
    // add BeanProperty import, if properties have been found
    if (!fields.isEmpty && !cu.getImports.contains(BEAN_PROPERTY_IMPORT)) {
      cu.getImports.add(BEAN_PROPERTY_IMPORT)
    }
  }
}

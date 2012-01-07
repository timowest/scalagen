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
import java.util.ArrayList
import UnitTransformer._

object Constructors extends Constructors

/**
 * Constructors reorders and normalizes constructors
 */
class Constructors extends UnitTransformer {
   
  def transform(cu: CompilationUnit): CompilationUnit = {
    for (t <- cu.getTypes if t.getMembers != null) {
      transform(cu, t)
    }
    cu
  }

  private def isThisConstructor(s: Statement): Boolean = {
    s.isInstanceOf[ConstructorInvocation] && 
    s.asInstanceOf[ConstructorInvocation].isThis
  }
  
  private def transform(cu: CompilationUnit, t: Type) {
    // transform sub types
    t.getMembers.collect { case t: Type => t}.foreach(t => transform(cu, t))    
    
    // get all constructors
    val constr = t.getMembers.collect { case c: Constructor => c }
      
    if (constr.isEmpty) {
      return
    }
    
    // get first without delegating
    val first = constr.find( c =>
      c.getBlock.isEmpty || !isThisConstructor(c.getBlock()(0)))
    
    // move in front of others
    first.filter(_ != constr(0)).foreach { c =>
      t.getMembers.remove(c)
      t.getMembers.add(t.getMembers.indexOf(constr(0)), c)
    }   
        
    // copy initializer, if constructor block has non-constructor statements
    val c = first.getOrElse(constr(0))  
        
    if (!c.getBlock.isEmpty &&  
        !c.getBlock.getStmts.filter(!_.isInstanceOf[ConstructorInvocation]).isEmpty) {
      
      processStatements(cu, t, c)
      
      if (!c.getBlock.isEmpty && 
          !(c.getBlock.size == 1 && c.getBlock()(0).isInstanceOf[ConstructorInvocation] &&
          !c.getBlock()(0).asInstanceOf[ConstructorInvocation].isThis())) {
        val initializer = new Initializer(false, c.getBlock)
        t.getMembers.add(t.getMembers.indexOf(c), initializer)  
      }      
    }    
    
    // add missing delegations
    t.getMembers.collect { case c: Constructor => c }.filter(_ != c)
      .foreach { c =>
        if (!c.getBlock.isEmpty && !c.getBlock()(0).isInstanceOf[ConstructorInvocation]) {
          c.getBlock.getStmts.add(0, new ConstructorInvocation(true, null, null))
        }
      }
    
  }
  
  private def processStatements(cu: CompilationUnit, t: Type, c: Constructor) {
    val fields = t.getMembers.collect { case f: Field => f }
    val variables = fields.flatMap(_.getVariables).map(v => (v.getId.getName, v)).toMap
    val variableToField = fields.flatMap(f => f.getVariables.map(v => (v.getId.getName,f)) ).toMap
      
    // go through statements and map assignments to variable initializers
    c.getBlock.getStmts.collect { case s: ExpressionStmt => s }
      .filter(isAssignment(_))
      .foreach { s =>
      val assign = s.getExpression.asInstanceOf[Assign]
      if (assign.getTarget.isInstanceOf[FieldAccess]) {
        val fieldAccess = assign.getTarget.asInstanceOf[FieldAccess]
        processFieldAssign(s, assign, fieldAccess, c, variables, variableToField)
      } else if (assign.getTarget.isInstanceOf[Name]) {
        val namedTarget = assign.getTarget.asInstanceOf[Name]
        if (variables.contains(namedTarget.getName)) {
          if (assign.getValue.isInstanceOf[Name]) { // field = parameter
            val namedValue = assign.getValue.asInstanceOf[Name]
            val param = c.getParameters.find(_.getId.getName == namedValue.getName).get
            val field = variableToField(namedTarget.getName)
            // rename parameter to field name
            param.setId(namedTarget.getName)
            copyAnnotationsAndModifiers(field, param)
            // remove field
            field.getVariables.remove(variables(namedTarget.getName))
          } else { // field = ?!?
            variables(namedTarget.getName).setInit(assign.getValue)              
          }          
          c.getBlock.remove(s)
        }
      }
    }
    
    // remove empty field declarations
    fields.filter(_.getVariables.isEmpty).foreach { t.getMembers.remove(_) }
    
  }

  private def processFieldAssign(s: ExpressionStmt, assign: Assign, fieldAccess: FieldAccess, 
      c: Constructor, variables: Map[String, Variable], variableToField: Map[String, Field] ) {
    if (fieldAccess.getScope.isInstanceOf[This] &&
        variables.contains(fieldAccess.getField)) {
      if (fieldAccess.getField == assign.getValue.toString) {
        val field = variableToField(fieldAccess.getField)
        c.getParameters.find(_.getId.getName == fieldAccess.getField)
          .foreach(copyAnnotationsAndModifiers(field,_))
        // remove field, as constructor parameter can be used
        field.getVariables.remove(variables(fieldAccess.getField))  
         
      } else {
        // remove statement, put init to field
        variables(fieldAccess.getField).setInit(assign.getValue)            
      }            
      c.getBlock.remove(s)  
    }
  }
  
  private def copyAnnotationsAndModifiers(f: Field, p: Parameter) {
    if (f.getAnnotations != null) {
      if (p.getAnnotations == null) {
        p.setAnnotations(new ArrayList[Annotation]())
      }
      for (a <- f.getAnnotations if !p.getAnnotations.contains(a)) {
        p.getAnnotations.add(a)
      }  
    }
    
    val modifiers = f.getModifiers.removeModifier(ModifierSet.PRIVATE).addModifier(PROPERTY)
    p.setModifiers(modifiers)
  }  
  
}

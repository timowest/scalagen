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
import java.util.ArrayList
import com.mysema.scala.BeanUtils
import UnitTransformer._

object BeanProperties extends BeanProperties

/**
 * BeanProperties turns field + accessor combinations into @BeanPropert annotated 
 * Scala properties
 */
class BeanProperties extends UnitTransformerBase with BeanHelpers {
     
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(n: ClassOrInterfaceDecl, cu: CompilationUnit): ClassOrInterfaceDecl = {      
    // merges getters and setters into properties
    val t = super.visit(n, cu).asInstanceOf[ClassOrInterfaceDecl]
    
    // accessors
    val methods = t.getMembers.collect { case m: Method => m }
    val getters = methods.filter(m => isBeanGetter(m) || isBooleanBeanGetter(m))
      .map(m => (getProperty(m) ,m)).toMap      
    val setters = methods.filter(m => isBeanSetter(m))
      .map(m => (getProperty(m), m)).toMap
   
    // fields with accessors
    val fields = t.getMembers.collect { case f: Field => f }
      .filter(_.getModifiers.isPrivate)
      .flatMap( f => f.getVariables.map( v => (v.getId.getName,v,f) ))
      .filter { case (name,_,_) =>  getters.contains(name) }
          
    // remove accessors 
    for ( (name, variable, field) <- fields) {
      var getter = getters(name)
      //t.getMembers.remove(getter)
      t.setMembers(t.getMembers.filterNot(_ == getter))
      setters.get(name).foreach { s => t.setMembers(t.getMembers.filterNot(_ == s)) }
      
      // make field public
      val isFinal = field.getModifiers.isFinal
       field.setModifiers(getter.getModifiers
          .addModifier(if (isFinal) ModifierSet.FINAL else 0))
      val annotation = if (getter.getName.startsWith("is")) BOOLEAN_BEAN_PROPERTY else BEAN_PROPERTY 
      if (field.getAnnotations == null || !field.getAnnotations.contains(annotation)) {
        field.setAnnotations(field.getAnnotations :+ annotation)
      }      
      
      // handle lazy init
      if (isLazyCreation(getter.getBody, name)) {
        variable.setInit(getLazyInit(getter.getBody))
        field.addModifier(LAZY)
        if (!setters.contains(name)) {
          field.addModifier(ModifierSet.FINAL)
        }
      }
    }
    
    // add BeanProperty import, if properties have been found
    if (!fields.isEmpty && !cu.getImports.contains(BEAN_PROPERTY_IMPORT)) {
      cu.setImports(cu.getImports :+ BEAN_PROPERTY_IMPORT)
    }
    t
  }
    
}

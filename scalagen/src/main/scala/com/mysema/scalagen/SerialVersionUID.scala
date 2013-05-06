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
import japa.parser.ast.CompilationUnit
import UnitTransformer._

object SerialVersionUID extends SerialVersionUID

/**
 * SerialVersionUID turns serialVersionUID fields into annotations
 */
class SerialVersionUID extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(nn: ClassOrInterfaceDecl, cu: CompilationUnit): ClassOrInterfaceDecl = {      
    val n = super.visit(nn, cu).asInstanceOf[ClassOrInterfaceDecl]
    if (n.getMembers == null) {
      return n
    }
    
    val varAndField = n.getMembers.collect { case f: Field => f }    
       .flatMap { f => f.getVariables.map( v => (v.getId.getName,v,f)) }
       .find(_._1 == "serialVersionUID").map(t => (t._2,t._3))
       .getOrElse(null)
       
    if (varAndField != null) {
      //varAndField._2.getVariables.remove(varAndField._1)
      varAndField._2.setVariables(varAndField._2.getVariables.filterNot(_ == varAndField._1))
      if (varAndField._2.getVariables.isEmpty) {
        //n.getMembers.remove(varAndField._2)
        n.setMembers( n.getMembers.filterNot(_ == varAndField._2) )
      }
      val value = varAndField._1.getInit
      n.setAnnotations(new SingleMemberAnnotation("SerialVersionUID", value) :: n.getAnnotations)
    }
    n
  }
  
}
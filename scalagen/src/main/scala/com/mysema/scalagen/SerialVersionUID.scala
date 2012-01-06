package com.mysema.scalagen

import java.util.ArrayList
import japa.parser.ast.CompilationUnit
import UnitTransformer._

object SerialVersionUID extends SerialVersionUID

class SerialVersionUID extends UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    for (t <- cu.getTypes if t.getMembers != null) {
      transform(cu, t)
    }
    cu
  }  
 
  private def transform(cu: CompilationUnit, t: Type) {
    if (t.getMembers == null) {
      return
    }
    
    val varAndField = t.getMembers.collect { case f: Field => f }    
       .flatMap { f => f.getVariables.map( v => (v.getId.getName,v,f)) }
       .find(_._1 == "serialVersionUID").map(t => (t._2,t._3))
       .getOrElse(null)
       
    if (varAndField != null) {
      varAndField._2.getVariables.remove(varAndField._1)
      if (varAndField._2.getVariables.isEmpty) {
        t.getMembers.remove(varAndField._2)
      }
      if (t.getAnnotations == null) {
        t.setAnnotations(new ArrayList[Annotation]())
      }
      val value = varAndField._1.getInit
      t.getAnnotations.add(new SingleMemberAnnotation("SerialVersionUID", value))
    }
  }
  
}
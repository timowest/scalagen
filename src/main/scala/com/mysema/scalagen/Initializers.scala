package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
import java.util.ArrayList
import UnitTransformer._

object Initializers extends Initializers

class Initializers extends UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    for (t <- cu.getTypes if t.getMembers != null) {
      transform(cu, t)
    }
    cu
  }
  
  private def transform(cu: CompilationUnit, t: Type) {    
    val initializers = t.getMembers.collect { case i: Initializer => i }
    if (!initializers.isEmpty) {
      val fields = t.getMembers.collect { case f: Field => f }
      val variables = fields.flatMap(_.getVariables).map(v => (v.getId.getName, v)).toMap
      
      for (i <- initializers) {
        val stmts = new java.util.HashSet[Statement]()
        for (stmt <- i.getBlock.getStmts if isAssignment(stmt)) {
          val assign = getAssignment(stmt)
          if (assign.getTarget.isInstanceOf[Name]) {
            val namedTarget = assign.getTarget.asInstanceOf[Name]
            if (variables.contains(namedTarget.getName)) {
              variables(namedTarget.getName).setInit(assign.getValue)
              stmts.add(stmt)
            }
          }
        }
        
        i.getBlock.getStmts.removeAll(stmts)      
      }      
      
      // remove empty initializers
      for (i <- initializers if i.getBlock.getStmts.isEmpty) {
        t.getMembers.remove(i)
      }
    }
  }

  
}
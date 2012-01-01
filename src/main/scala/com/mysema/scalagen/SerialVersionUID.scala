package com.mysema.scalagen

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
    // TODO    
  }
  
}
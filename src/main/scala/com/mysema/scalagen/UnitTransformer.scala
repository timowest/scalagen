package com.mysema.scalagen 

import japa.parser.ast.CompilationUnit

trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
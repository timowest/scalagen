package com.mysema.scalagen 

import japa.parser.ast.CompilationUnit

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
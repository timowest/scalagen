package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
import japa.parser.ast.visitor._
import java.util.{ ArrayList, Collections }
import UnitTransformer._

object RemoveAsserts extends RemoveAsserts

class RemoveAsserts extends UnitTransformerBase {
  
  private val methods = Set("hasLength","hasText","notEmpty","notNull") 
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, new Context()).asInstanceOf[CompilationUnit] 
  }  
    
  // TODO : don't remove method calls when used as statements
  
  override def visit(n: MethodCall, arg: Context) = {
    if (methods.contains(n.getName) && n.getScope != null && n.getScope.toString == "Assert") {
      n.getArgs.get(0)
    } else {
      super.visit(n, arg)
    } 
  }
  
}
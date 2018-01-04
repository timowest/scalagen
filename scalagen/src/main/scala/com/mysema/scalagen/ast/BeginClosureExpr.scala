package com.mysema.scalagen.ast

import com.github.javaparser.ast.expr.NameExpr

class BeginClosureExpr(params: String) extends NameExpr(params) {
  def params = getName
}
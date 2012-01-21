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
import UnitTransformer._

object RemoveAsserts extends RemoveAsserts

/**
 * RemoveAsserts unwraps assertion method call
 */
class RemoveAsserts extends UnitTransformerBase {
  
  private val methods = Set("hasLength","hasText","notEmpty","notNull") 
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  // TODO : don't remove method calls when used as statements
  
  override def visit(n: MethodCall, arg: CompilationUnit) = n match {
    case MethodCall("Assert", _, a :: rest) => a.accept(this, arg)
    case _ => super.visit(n, arg)
  }
    
}
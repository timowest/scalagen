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

object Rethrows extends Rethrows

/**
 * Rethrows unwraps try/catch blocks with simple rethrows
 */
class Rethrows extends UnitTransformerBase {
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: Try, arg: CompilationUnit): Node = {
    if (n.getFinallyBlock == null && !isEmpty(n.getCatchs) && n.getCatchs.filter(isPrinted).isEmpty) {
      extract(super.visit(n.getTryBlock, arg).asInstanceOf[Statement])
    } else {
      super.visit(n, arg)
    }
  }
  
  private def isPrinted(c: Catch): Boolean = {
    val block = c.getCatchBlock()
    block.isEmpty || block.size > 1 || (block.size == 1 && !block(0).isInstanceOf[Throw])
  }
    
}
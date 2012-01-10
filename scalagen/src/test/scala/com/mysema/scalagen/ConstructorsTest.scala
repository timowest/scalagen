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

import org.junit.Assert.assertEquals
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import java.io.FileNotFoundException
import org.junit.Test
import com.mysema.examples._

class ConstructorsTest extends AbstractParserTest {

  @Test
  def Empty_Constructor_Are_Ignored() {
    var unit = getCompilationUnit(classOf[WithStatic])
    assertEquals(2, unit.getTypes.get(0).getMembers.size)
    unit = Constructors.transform(unit)
    assertEquals(2, unit.getTypes.get(0).getMembers.size)
  }

  @Test
  def Body_Is_Extracted() {
    var unit = getCompilationUnit(classOf[Immutable])
    assertEquals(6, unit.getTypes.get(0).getMembers.size)
    unit = Constructors.transform(unit)
    assertEquals(4, unit.getTypes.get(0).getMembers.size)
  }
  
  @Test
  def Immutable2 {
    var unit = getCompilationUnit(classOf[Immutable2])
    unit = Constructors.transform(unit)
    // TODO
  }
  
}

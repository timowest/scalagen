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

import org.junit.Assert._
import java.io.FileNotFoundException
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import org.junit.Test
import com.mysema.examples._
import UnitTransformer._

class CompanionObjectTest extends AbstractParserTest {

  @Test
  def Replace_Class() {
    var unit = getCompilationUnit(classOf[WithStatic])
    assertEquals(1, unit.getTypes.size)
    unit = CompanionObject.transform(unit)
    assertEquals(1, unit.getTypes.size)
    assertEquals(OBJECT, unit.getTypes.get(0).getModifiers)
    assertEquals("WithStatic", unit.getTypes.get(0).getName)
  }

  @Test
  def Replace_Class_Imports() {
    var unit = getCompilationUnit(classOf[WithStatic])
    assertEquals(0, if (unit.getImports == null) 0 else unit.getImports.size)
    unit = CompanionObject.transform(unit)
    assertEquals(0, if (unit.getImports == null) 0 else unit.getImports.size)
  }

  @Test
  def Split_Class() {
    var unit = getCompilationUnit(classOf[WithStaticAndInstance])
    assertEquals(1, unit.getTypes.size)
    unit = CompanionObject.transform(unit)
    assertEquals(2, unit.getTypes.size)
  }

  @Test
  def Split_Class_Imports() {
    var unit = getCompilationUnit(classOf[WithStaticAndInstance])
    assertEquals(0, if (unit.getImports == null) 0 else unit.getImports.size)
    unit = CompanionObject.transform(unit)
    assertEquals(1, if (unit.getImports == null) 0 else unit.getImports.size)
  }
}

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
import japa.parser.ast.body.BodyDeclaration
import japa.parser.ast.body.FieldDeclaration
import java.io.FileNotFoundException
import org.junit.Test
import com.mysema.examples._

class BeanPropertiesTest extends AbstractParserTest {

  @Test
  def Methods_Are_Removed1() {
    var unit = getCompilationUnit(classOf[Bean])
    assertEquals(14, unit.getTypes.get(0).getMembers.size)    
    unit = BeanProperties.transform(unit)
    assertEquals(4, unit.getTypes.get(0).getMembers.size)
//    for (member <- unit.getTypes.get(1).getMembers) {
//      assertEquals(classOf[FieldDeclaration], member.getClass)
//    }
  }

  @Test
  def Methods_Are_Removed2() {
    var unit = getCompilationUnit(classOf[Bean2])
    assertEquals(13, unit.getTypes.get(0).getMembers.size)    
    unit = BeanProperties.transform(unit)
    assertEquals(5, unit.getTypes.get(0).getMembers.size)
//    for (member <- unit.getTypes.get(0).getMembers) {
//      assertEquals(classOf[FieldDeclaration], member.getClass)
//    }
  }
}

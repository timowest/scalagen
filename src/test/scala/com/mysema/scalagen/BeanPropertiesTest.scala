package com.mysema.scalagen

import org.junit.Assert.assertEquals
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import japa.parser.ast.body.BodyDeclaration
import japa.parser.ast.body.FieldDeclaration
import java.io.FileNotFoundException
import org.junit.Test

class BeanPropertiesTest extends AbstractParserTest {

  @Test
  def Methods_Are_Removed1() {
    var unit = getCompilationUnit(classOf[ExampleBean])
    assertEquals(10, unit.getTypes.get(0).getMembers.size)
    unit = BeanProperties.transform(unit)
    assertEquals(2, unit.getTypes.get(0).getMembers.size)
    for (member <- unit.getTypes.get(0).getMembers) {
      assertEquals(classOf[FieldDeclaration], member.getClass)
    }
  }

  @Test
  def Methods_Are_Removed2() {
    var unit = getCompilationUnit(classOf[ExampleBean2])
    assertEquals(12, unit.getTypes.get(0).getMembers.size)
    unit = BeanProperties.transform(unit)
    assertEquals(4, unit.getTypes.get(0).getMembers.size)
    for (member <- unit.getTypes.get(0).getMembers) {
      assertEquals(classOf[FieldDeclaration], member.getClass)
    }
  }
}

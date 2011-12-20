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
    assertEquals(11, unit.getTypes.get(0).getMembers.size)    
    unit = BeanProperties.transform(unit)
    assertEquals(3, unit.getTypes.get(0).getMembers.size)
//    for (member <- unit.getTypes.get(1).getMembers) {
//      assertEquals(classOf[FieldDeclaration], member.getClass)
//    }
  }

  @Test
  def Methods_Are_Removed2() {
    var unit = getCompilationUnit(classOf[ExampleBean2])
    assertEquals(13, unit.getTypes.get(0).getMembers.size)    
    unit = BeanProperties.transform(unit)
    assertEquals(5, unit.getTypes.get(0).getMembers.size)
//    for (member <- unit.getTypes.get(0).getMembers) {
//      assertEquals(classOf[FieldDeclaration], member.getClass)
//    }
  }
}

package com.mysema.scalagen

import org.junit.Assert.assertEquals
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import java.io.FileNotFoundException
import org.junit.Test

class ConstructorsTest extends AbstractParserTest {

  @Test
  def Empty_Constructor_Are_Ignored() {
    var unit = getCompilationUnit(classOf[ExampleWithStatic])
    assertEquals(2, unit.getTypes.get(0).getMembers.size)
    unit = Constructors.transform(unit)
    assertEquals(2, unit.getTypes.get(0).getMembers.size)
  }

  @Test
  def Body_Is_Extracted() {
    var unit = getCompilationUnit(classOf[ExampleImmutable])
    assertEquals(6, unit.getTypes.get(0).getMembers.size)
    unit = Constructors.transform(unit)
    assertEquals(4, unit.getTypes.get(0).getMembers.size)
  }
  
  @Test
  def Immutable2 {
    var unit = getCompilationUnit(classOf[ExampleImmutable2])
    unit = Constructors.transform(unit)
    // TODO
  }
  
}

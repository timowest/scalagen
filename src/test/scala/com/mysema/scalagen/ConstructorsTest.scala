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

package com.mysema.scalagen

import org.junit.Assert._
import java.io.FileNotFoundException
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import org.junit.Test

class CompanionObjectTest extends AbstractParserTest {

  @Test
  def Replace_Class() {
    var unit = getCompilationUnit(classOf[ExampleWithStatic])
    assertEquals(1, unit.getTypes.size)
    unit = CompanionObject.transform(unit)
    assertEquals(1, unit.getTypes.size)
    assertEquals(" ExampleWithStatic", unit.getTypes.get(0).getName)
  }

  @Test
  def Replace_Class_Imports() {
    var unit = getCompilationUnit(classOf[ExampleWithStatic])
    assertEquals(0, if (unit.getImports == null) 0 else unit.getImports.size)
    unit = CompanionObject.transform(unit)
    assertEquals(0, if (unit.getImports == null) 0 else unit.getImports.size)
  }

  @Test
  def Split_Class() {
    var unit = getCompilationUnit(classOf[ExampleWithStaticAndInstance])
    assertEquals(1, unit.getTypes.size)
    unit = CompanionObject.transform(unit)
    assertEquals(2, unit.getTypes.size)
  }

  @Test
  def Split_Class_Imports() {
    var unit = getCompilationUnit(classOf[ExampleWithStaticAndInstance])
    assertEquals(0, if (unit.getImports == null) 0 else unit.getImports.size)
    unit = CompanionObject.transform(unit)
    assertEquals(1, if (unit.getImports == null) 0 else unit.getImports.size)
  }
}

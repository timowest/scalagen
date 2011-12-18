package com.mysema.scalagen 

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import org.junit.{ Test, Ignore }
import org.junit.Assert._
import japa.parser.JavaParser

class SerializationTest extends AbstractParserTest {
  
  private def assertContains(str: String, strings: String*) {
    strings.foreach { s => assertTrue(s + " was not found", str.contains(s)) }
  }
  
  @Test
  def ExampleBean {
    val sources = toScala[ExampleBean]
    assertContains(sources, "@BeanProperty")
  }
  
  @Test
  def ExampleBean2 {
    val sources = toScala[ExampleBean2]
    assertContains(sources, "@BeanProperty")
  }
  
  @Test
  def ExampleControl {
    val sources = toScala[ExampleControl]
  }
  
  @Test
  def ExampleConstructors {
    val sources = toScala[ExampleConstructors]
    assertContains(sources, "class ExampleConstructors(first: String, last: String) {")
  }
  
  @Test
  def ExampleImmutable {
    val sources = toScala[ExampleImmutable]
    assertContains(sources, "(firstName: String, lastName: String)")
  }
  
  def ExampleInnerClasses {
    val sources = toScala[ExampleInnerClasses]
    assertContains(sources, "private class LoopContext private () {")
  }
  
  @Test
  def ExampleReserved {
    val sources = toScala[ExampleReserved]
    assertContains(sources, "`object`","`type`","`var`","`val`")
  }
  
  @Test
  def ExampleSwitchCase {
    val sources = toScala[ExampleSwitchCase]
    assertContains(sources, 
      "case 0 => System.out.println(0)", 
      "case 1 => System.out.println(1)",
      "case 0 | 1 => System.out.println(1)")
  }
  
  @Test
  def ExampleTryCatch {
    val sources = toScala[ExampleTryCatch]
    assertContains(sources, 
      "case e: IllegalArgumentException => throw new RuntimeException(e)",
      "case e: NullPointerException => System.err.println(e.getMessage)")
    
  }
  
  @Test @Ignore // FIXME
  def ExampleWithComments {
    val sources = toScala[ExampleWithComments]
    assertContains(sources, "javadocs", "// comments inside")
  }
  
  @Test
  def ExampleWithStatic {
    val sources = toScala[ExampleWithStatic]
    assertContains(sources, "object ExampleWithStatic {")
  }
  
  @Test
  def ExampleWithStaticAndInstance {
    val sources = toScala[ExampleWithStaticAndInstance]
    assertContains(sources,
      "object ExampleWithStaticAndInstance {",
      "class ExampleWithStaticAndInstance {")
  }
  
}
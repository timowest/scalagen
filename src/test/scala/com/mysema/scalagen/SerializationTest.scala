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
  def ExampleAbstractCodeWriter {
    
  }
  
  @Test
  def ExampleAbstractDao {
    
  }
  
  @Test
  def ExampleArrayConstructorExpression {
    
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
  def ExampleConstantImpl {
    val sources = toScala[ExampleConstantImpl[_]]
    assertContains(sources, "private val BYTES = new Array[Constant[Byte]](CACHE_SIZE)")
  }
    
  @Test
  def ExampleConstructors {
    val sources = toScala[ExampleConstructors]
    assertContains(sources, "class ExampleConstructors(first: String, last: String) {")
  }    
  
  @Test
  def ExampleControl {
    val sources = toScala[ExampleControl]
    assertContains(sources, 
        "for (i <- 0 until integers.size) {",
        "for (i <- integers) {",
        "for (i <- integers if i > 0) {")
  }
  
  @Test
  def ExampleDao {
    
  }
  
  @Test
  def ExampleFileSystemRegistry {
    
  }
  
  @Test
  def ExampleImmutable {
    val sources = toScala[ExampleImmutable]
    assertContains(sources, 
        "class ExampleImmutable(@BeanProperty val firstName: String, @BeanProperty val lastName: String)")
  }
  
  @Test
  def ExampleImmutable2 {
    val sources = toScala[ExampleImmutable2]
    assertContains(sources, 
        "class ExampleImmutable2(@BeanProperty val firstName: String, @BeanProperty val lastName: String)")
  }
  
  @Test
  def ExampleInitializers {
    
  }
  
  @Test
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
  def ExampleSimpleCompiler {
    
  }
  
  @Test
  def ExampleSourceFileObject {
    
  }
  
  @Test
  def ExampleSuperConstructors {
    val sources = toScala[ExampleSuperConstructors]
    assertContains(sources, 
      "class ExampleSuperConstructors(first: String, last: String) extends SuperClass(first) {")
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
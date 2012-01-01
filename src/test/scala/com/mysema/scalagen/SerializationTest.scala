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
    val sources = toScala[ExampleAbstractCodeWriter[_]]
    assertContains(sources,
      "abstract class ExampleAbstractCodeWriter[T <: ExampleAbstractCodeWriter[T]]" +
      "(appendable: Appendable, spaces: Int) extends Appendable {")
  }
  
  @Test
  def ExampleAbstractDao {
    val sources = toScala[ExampleAbstractDao[_]]
    assertContains(sources, "protected def query(): JPQLQuery = new HibernateQuery(getSession)")
  }
  
  @Test
  def ExampleArrayConstructorExpression {
    val sources = toScala[ExampleArrayConstructorExpression[_]]
    assertContains(sources, 
      "val elementType = Assert.notNull(`type`.getComponentType, \"componentType\").asInstanceOf[Class[T]]",
      "override def equals(obj: Any): Boolean = {")
    
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
    val sources = toScala[ExampleDao[_,_]]
    assertContains(sources, "trait ExampleDao[Entity, Id <: Serializable] {")
  }
  
  @Test
  def ExampleFileSystemRegistry {
    val sources = toScala[ExampleFileSystemRegistry]
    assertContains(sources, "class ExampleFileSystemRegistry private () {")  
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
  def ExampleReturns {
    val sources = toScala[ExampleReturns]
    assertContains(sources,
      "for (i <- start until n if i / 5 > 1) return i",
      "    -1")    
  }
  
  @Test
  def ExampleSimpleCompiler {
    val sources = toScala[ExampleSimpleCompiler]
    assertContains(sources, 
      "for (url <- (classLoader.asInstanceOf[URLClassLoader]).getURLs) {",
      "case e: UnsupportedEncodingException => throw new RuntimeException(e)",
      "this(ToolProvider.getSystemJavaCompiler, Thread.currentThread().getContextClassLoader)")
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
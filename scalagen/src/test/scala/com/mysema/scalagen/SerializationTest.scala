package com.mysema.scalagen 

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import org.junit.{ Test, Ignore }
import org.junit.Assert._
import japa.parser.JavaParser
import com.mysema.examples._

class SerializationTest extends AbstractParserTest {
  
  private def assertContains(str: String, strings: String*) {
    strings.foreach { s => assertTrue(s + " was not found", str.contains(s)) }
  }
  
  @Test
  def AbstractCodeWriter {
    val sources = toScala[AbstractCodeWriter[_]]
    assertContains(sources,
      "abstract class AbstractCodeWriter[T <: AbstractCodeWriter[T]]" +
      "(val appendable: Appendable, val spaces: Int) extends Appendable {")
  }
  
  @Test
  def AbstractDao {
    val sources = toScala[AbstractDao[_]]
    assertContains(sources, "protected def query(): JPQLQuery = new HibernateQuery(getSession)")
  }
  
  @Test
  def AnnotatedElementAdapter {
    val sources = toScala[AnnotatedElementAdapter]
    assertContains(sources, "for (element <- elements; annotation <- element.getAnnotations) {")
  }
  
  @Test
  def ArrayConstructorExpression {
    val sources = toScala[ArrayConstructorExpression[_]]
    assertContains(sources, 
      "@SerialVersionUID(8667880104290226505L)",
      "val elementType = `type`.getComponentType.asInstanceOf[Class[T]]",
      "override def equals(obj: Any): Boolean = {")
    
  }
  
  @Test
  def ArrayTests {
    val sources = toScala[ArrayTests]
    assertContains(sources, "def foo(): Array[Int] = new Array[Int](2)")        
  }
  
  @Test
  def Bean {
    val sources = toScala[Bean]
    assertContains(sources, "@BeanProperty")
  }
  
  @Test
  def Bean2 {
    val sources = toScala[Bean2]
    assertContains(sources, "@BeanProperty")
  }
  
  @Test
  def Casts {
    val sources = toScala[Casts]
    assertContains(sources, "args.length.toDouble")
  }
  
  @Test
  def ConstantImpl {
    val sources = toScala[ConstantImpl[_]]
    assertContains(sources, "private val BYTES = new Array[Constant[Byte]](CACHE_SIZE)")
  }
    
  @Test
  def Constructors {
    val sources = toScala[com.mysema.examples.Constructors]
    assertContains(sources, "class Constructors(first: String, last: String) {")
  }    
  
  @Test
  def Control {
    val sources = toScala[Control]
    assertContains(sources, 
        "for (i <- 0 until integers.size) {",
        "for (i <- integers) {",
        "for (i <- integers if i > 0) {")
  }
  
  @Test
  def Dao {
    val sources = toScala[IDao[_,_]]
    assertContains(sources, "trait IDao[Entity, Id <: Serializable] {")
  }
  
  @Test
  def FileSystemRegistry {
    val sources = toScala[FileSystemRegistry]
    assertContains(sources, "class FileSystemRegistry private () {")  
  }
  
  @Test
  def Immutable {
    val sources = toScala[Immutable]
    assertContains(sources, 
        "class Immutable(@BeanProperty val firstName: String, @BeanProperty val lastName: String)")
  }
  
  @Test
  def Immutable2 {
    val sources = toScala[Immutable2]
    assertContains(sources, 
        "class Immutable2(@BeanProperty val firstName: String, @BeanProperty val lastName: String)")
  }
  
  @Test
  def Initializers {
    
  }
  
  @Test
  def InnerClasses {
    val sources = toScala[InnerClasses]
    assertContains(sources, "private class LoopContext private () {")
  }
  
  @Test
  def Modifiers {
    val sources = toScala[Modifiers]
    assertContains(sources, 
        "@transient private var foo: String = \"foo\"",
        "@volatile private var bar: String = \"bar\"")
  }
  
  @Test
  def Ops {
    val sources = toScala[Ops]
    assertContains(sources, "object Ops {", "object AggOps {")
  }
  
  @Test
  def Reserved {
    val sources = toScala[Reserved]
    assertContains(sources, "`object`","`type`","`var`","`val`")
  }
  
  @Test
  def Returns {
    val sources = toScala[Returns]
    assertContains(sources,
      "for (i <- start until n if i / 5 > 1) return i",
      "    -1")    
  }
  
  @Test
  def SimpleCompiler {
    val sources = toScala[SimpleCompiler]
    assertContains(sources, 
      "for (url <- (classLoader.asInstanceOf[URLClassLoader]).getURLs) {",
      //"case e: UnsupportedEncodingException => throw new RuntimeException(e)",
      "this(ToolProvider.getSystemJavaCompiler, Thread.currentThread().getContextClassLoader)")
  }
  
  @Test
  def SourceFileObject {
    
  }
  
  @Test
  def SuperConstructors {
    val sources = toScala[SuperConstructors]
    assertContains(sources, 
      "class SuperConstructors(first: String, last: String) extends SuperClass(first) {")
  }
  
  @Test
  def SwitchCase {
    val sources = toScala[SwitchCase]
    assertContains(sources, 
      "case 0 => System.out.println(0)", 
      "case 1 => System.out.println(1)",
      "case 0 | 1 => System.out.println(1)")
  }
  
  @Test
  def TryCatch {
    val sources = toScala[TryCatch]
    assertContains(sources, 
      "case e: IllegalArgumentException => throw new RuntimeException(e)",
      "case e: NullPointerException => System.err.println(e.getMessage)")
    
  }
  
  @Test @Ignore // FIXME
  def WithComments {
    val sources = toScala[WithComments]
    assertContains(sources, "javadocs", "// comments inside")
  }
  
  @Test
  def WithStatic {
    val sources = toScala[WithStatic]
    assertContains(sources, "object WithStatic {")
  }
  
  @Test
  def WithStaticAndInstance {
    val sources = toScala[WithStaticAndInstance]
    assertContains(sources,
      "object WithStaticAndInstance {",
      "class WithStaticAndInstance {")
  }
  
  @Test
  def Wildcard {
    val sources = toScala[Wildcard]
    assertContains(sources, "def bar(list: List[_ <: CharSequence]): Int = list.size")
  }
  
}
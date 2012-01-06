package com.mysema.scalagen 

import java.io.File
import java.io.FileInputStream
import java.io.IOException
import org.junit.Test
import japa.parser.JavaParser
import com.mysema.scala.CompileTestUtils
import org.junit.Assert._

class ScalaCompilationTest extends AbstractParserTest with CompileTestUtils {

  @Test
  def Compile {
    val resources = List[File](new File("src/test/scala/com/mysema/examples").listFiles():_*)
    
    // parallel compilation
    val failures = resources.filter(_.getName.endsWith(".java")).par.map { f =>
      var unit = JavaParser.parse(new FileInputStream(f))
      val source = toScala(unit)      
      try {
        assertCompileSuccess(source)
        null
      } catch {
        case e: AssertionError => (f.getName, e.getMessage)  
      }
    }.toList.filter(_ != null).toMap
    
    failures.foreach { case (n,m) => System.err.println(n + " => " + m)}
    
    assertTrue(
      failures.size + " of " + resources.size + " failures : " + failures.keys.mkString(", "), 
      failures.isEmpty)
  }
}
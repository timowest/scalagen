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
    val resources = List[File](new File("src/test/scala/com/mysema/scalagen").listFiles():_*)
    val failures = Map.newBuilder[String,String]
    
    resources.filter(_.getName.endsWith(".java")).foreach { f =>
      var in = new FileInputStream(f)
      var unit = JavaParser.parse(in)
      val source = toScala(unit)      
      try {
        assertCompileSuccess(source)
      } catch {
        case e: AssertionError => failures.+=((f.getName, e.getMessage)) 
      }
    }
    
    val result = failures.result
    result.foreach { case(n,m) =>
      System.err.println(n + " => " + m)      
    }
    assertTrue(
      result.size + " of " + resources.size + " failures : " + result.keys.mkString(", "), 
      result.isEmpty)
  }
}
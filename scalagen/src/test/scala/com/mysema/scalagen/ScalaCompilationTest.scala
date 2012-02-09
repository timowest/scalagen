/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */
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
        //case e: Exception => (f.getName, e.getMessage)  
      }
    }.toList.filter(_ != null).toMap
    
    failures.foreach { case (n,m) => System.err.println(n + " => " + m)}
    
    assertTrue(
      failures.size + " of " + resources.size + " failures : " + failures.keys.mkString(", "), 
      failures.isEmpty)
  }
}
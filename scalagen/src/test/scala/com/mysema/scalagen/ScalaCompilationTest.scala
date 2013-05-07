/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
    val failures = resources.filter(_.getName.endsWith(".java")).map { f =>
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
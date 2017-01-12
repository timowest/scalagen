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
import com.github.javaparser.JavaParser
import com.mysema.scala.CompileTestUtils
import org.junit.Assert._
import TestDirectoryStructure._
import scala.tools.nsc.Settings

class ScalaCompilationTest extends AbstractParserTest with CompileTestUtils {

  @Test
  def Compile {
    val resources = List[File](EXAMPLE_FILE_DIR.listFiles():_*)
    val filterString = sys.props.get("test-compile-filter")
    // parallel compilation
    val failures = resources.filter(
      f => f.getName.endsWith(".java") &&
        filterString.map(f.getName.contains).getOrElse(true)
    ).par.map(tryCompiling)
      .toList
      .filter(_ != null)

    failures.foreach { case (n,m,s) => System.err.println(s"$n => $m in \n$s")}

    assertTrue(
      failures.size + " of " + resources.size + " failures : " + failures.map(_._1).mkString(", "),
      failures.isEmpty)
  }

  private def tryCompiling(f: java.io.File): (String, String, String) = {
    var unit = JavaParser.parse(new FileInputStream(f))
    val source = toScala(unit)
    try {
      assertCompileSuccess(source)
      null
    } catch {
      case e: AssertionError => (f.getName, e.getMessage, source)
      //case e: Exception => (f.getName, e.getMessage)
    }
  }
}

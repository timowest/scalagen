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
import java.io.{IOException, File}
import org.junit._
import com.github.javaparser.JavaParser
import com.mysema.scala.CompileTestUtils
import org.junit.Assert._
import org.junit.runner.RunWith
import org.junit.runners.Parameterized
import org.junit.runners.Parameterized.Parameters
import TestDirectoryStructure._
import scala.tools.nsc.Settings

import scala.collection.JavaConverters._
import ScalaCompilationTest._
object ScalaCompilationTest {

  @Parameters
  def data(): java.util.Collection[Array[File]] = {
    val resources = List[File](EXAMPLE_FILE_DIR.listFiles():_*)
    val filterString = sys.props.get("test-compile-filter")
    filterString.foreach(f => println(s"Filtering compilation test to files containing '$f'"))
    resources.filter(
      f => f.getName.endsWith(".java") &&
        filterString.map(f.getName.contains).getOrElse(true)
    ).map(Array(_)).asJava
  }
}

@RunWith(classOf[Parameterized])
class ScalaCompilationTest(private var file: File) extends AbstractParserTest with CompileTestUtils {

  @Test
  def Compile {
    var unit = JavaParser.parse(new FileInputStream(file))
    val source = toScala(unit)
    val compilerSettingRegex = """compile: (.*)""".r
    val compilerSettingsModifier =  compilerSettingRegex.findFirstIn(unit.toString) match {
      case Some(compilerSettingRegex(compilerSettings)) =>
        { settings: Settings =>
          settings.processArgumentString(compilerSettings)
          settings
        }
      case None =>
        identity[Settings] _
    }
    assertCompileSuccess(source, compilerSettingsModifier)
  }
}

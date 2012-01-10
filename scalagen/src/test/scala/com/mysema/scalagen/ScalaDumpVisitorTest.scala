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

import japa.parser.JavaParser
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.util.ArrayList
import java.util.Arrays
import java.util.List
import org.apache.commons.io.FileUtils
import org.junit.Test

class ScalaDumpVisitorTest extends AbstractParserTest {

  @Test
  def Dump {
    val resources = new ArrayList[File]()
    resources.addAll(Arrays.asList(new File("src/test/scala/com/mysema/examples").listFiles():_*))
    for (res <- resources if res.getName.endsWith(".java")) {
      val out = new File("target/" + res.getName.substring(0, res.getName.length() - 5) + ".scala")
      Converter.instance.convertFile(res, out)
    }
  }
}

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
import japa.parser.ast.{CompilationUnit, ImportDeclaration}
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import java.util.ArrayList
import scala.collection.JavaConversions._

abstract class AbstractParserTest {
  
  def getCompilationUnit(cl: Class[_]): CompilationUnit = {
    var file = new File("src/test/scala/" + cl.getName.replace('.', '/') + ".java")
    var in = new FileInputStream(file)
    val unit = JavaParser.parse(in)
    if (unit.getImports == null) {
      unit.setImports(new ArrayList[ImportDeclaration])
    }
    unit
  }
  
  def toScala(obj: AnyRef): String = toScala(getCompilationUnit(obj.getClass))
  
  def toScala[T](implicit mf: Manifest[T]): String = toScala(getCompilationUnit(mf.erasure))
  
  def toScala(unit: CompilationUnit): String = Converter.instance.toScala(unit)
  
}

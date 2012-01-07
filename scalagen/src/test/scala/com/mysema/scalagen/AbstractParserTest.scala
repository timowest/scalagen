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
  
  def toScala[T](implicit mf: Manifest[T]): String = toScala(getCompilationUnit(mf.erasure))
  
  def toScala(unit: CompilationUnit): String = Converter.instance.toScala(unit)
  
}

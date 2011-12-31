package com.mysema.scalagen

import japa.parser.JavaParser
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import scala.collection.JavaConversions._
import AbstractParserTest._

object AbstractParserTest {
  
  val converter = new Converter("UTF-8",List[UnitTransformer](
    Primitives,
    ControlStatements, 
    CompanionObject, 
    BeanProperties, 
    Constructors, 
    Initializers))  
}

abstract class AbstractParserTest {
  
  def getCompilationUnit(cl: Class[_]): CompilationUnit = {
    var file = new File("src/test/scala/" + cl.getName.replace('.', '/') + ".java")
    var in = new FileInputStream(file)
    JavaParser.parse(in)
  }
  
  def toScala[T](implicit mf: Manifest[T]): String = toScala(getCompilationUnit(mf.erasure))
  
  def toScala(unit: CompilationUnit): String = converter.toScala(unit)
  
}

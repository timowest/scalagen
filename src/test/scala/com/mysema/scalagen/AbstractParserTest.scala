package com.mysema.scalagen

import japa.parser.JavaParser
import japa.parser.ParseException
import japa.parser.ast.CompilationUnit
import java.io.File
import java.io.FileInputStream
import java.io.FileNotFoundException
import scala.collection.JavaConversions._

abstract class AbstractParserTest {

  private val transformers = List[UnitTransformer](ControlStatements, CompanionObject, BeanProperties, Constructors)
  
  def getCompilationUnit(cl: Class[_]): CompilationUnit = {
    var file = new File("src/test/scala/" + cl.getName.replace('.', '/') + ".java")
    var in = new FileInputStream(file)
    JavaParser.parse(in)
  }
  
  def toScala[T](implicit mf: Manifest[T]): String = {
    toScala(getCompilationUnit(mf.erasure))
  }
  
  def toScala(unit: CompilationUnit) = {
    val transformed = transformers.foldLeft(unit) { case (u,t) => t.transform(u) }    
    var visitor = new ScalaDumpVisitor()
    unit.accept(visitor, new Context())
    visitor.getSource
  }
  
}

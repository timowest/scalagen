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

object ConvertQuerydsl extends AbstractParserTest {
  
  def main(args: Array[String]) {
    Converter.instance.convert(
        new File("../../querydsl/querydsl-core/src/main/java"),
        new File("target/querydsl-core"))
    Converter.instance.convert(
        new File("../../querydsl/querydsl-sql/src/main/java"),
        new File("target/querydsl-sql"))    
  }
  
}
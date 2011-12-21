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
    var resources = new ArrayList[File]()
    resources.addAll(Arrays.asList(new File("../../querydsl/querydsl-core/src/main/java/com/mysema/query").listFiles():_*))
    resources.addAll(Arrays.asList(new File("../../querydsl/querydsl-core/src/main/java/com/mysema/query/types").listFiles():_*))
    for (res <- resources) {
      if (res.getName.endsWith(".java")) {
        var unit = JavaParser.parse(new FileInputStream(res))
        val sources = toScala(unit)
        var out = new File("target/querydsl/" + res.getName.substring(0, res.getName.length() - 5) + ".scala")
        FileUtils.writeStringToFile(out, sources, "UTF-8")
      }
    }
  }
  
}
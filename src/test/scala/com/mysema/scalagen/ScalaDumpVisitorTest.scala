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
    var resources = new ArrayList[File]()
    resources.addAll(Arrays.asList(new File("src/test/scala/com/mysema/scalagen").listFiles():_*))
    resources.addAll(Arrays.asList(new File("../codegen/src/main/java/com/mysema/codegen").listFiles():_*))
//    resources.addAll(Arrays.asList(new File("../edith/src/main/java/fi/finlit/edith/sql/domain").listFiles():_*))
//    resources.addAll(Arrays.asList(new File("../edith/src/main/java/fi/finlit/edith/ui/services").listFiles():_*))
//    resources.addAll(Arrays.asList(new File("../edith/src/main/java/fi/finlit/edith/ui/services/hibernate").listFiles():_*))
    for (res <- resources) {
      if (res.getName.endsWith(".java")) {
        var unit = JavaParser.parse(new FileInputStream(res))
        val sources = toScala(unit)
        var out = new File("target/" + res.getName.substring(0, res.getName.length() - 5) + ".scala")
        FileUtils.writeStringToFile(out, sources, "UTF-8")
      }
    }
  }
}

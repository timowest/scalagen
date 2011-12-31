package com.mysema.scalagen

import java.io.File
import org.junit.Test
import org.junit.Assert._
import AbstractParserTest._

class ConverterTest extends AbstractParserTest {
  
  @Test
  def Convert_Creates_Files {       
    converter.convert(new File("src/test/scala"), new File("target/test/scala"))    
    assertTrue(new File("target/test/scala/com/mysema/scalagen/ExampleBean.scala").exists)
    assertTrue(new File("target/test/scala/com/mysema/scalagen/ExampleBean2.scala").exists)
  }
  
  @Test
  def Convert_Creates_File_with_Content {
    converter.convert(new File("src/test/scala"), new File("target/test2/scala"))    
    assertTrue(new File("target/test2/scala/com/mysema/scalagen/ExampleBean.scala").length > 0)
  }

}
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

import java.io.File
import org.junit.Test
import org.junit.Assert._

class ConverterTest extends AbstractParserTest {
  
  @Test
  def Convert_Creates_Files {       
    Converter.instance.convert(new File("src/test/scala"), new File("target/test/scala"))    
    assertTrue(new File("target/test/scala/com/mysema/examples/Bean.scala").exists)
    assertTrue(new File("target/test/scala/com/mysema/examples/Bean2.scala").exists)
  }
  
  @Test
  def Convert_Creates_File_with_Content {
    Converter.instance.convert(new File("src/test/scala"), new File("target/test2/scala"))    
    assertTrue(new File("target/test2/scala/com/mysema/examples/Bean.scala").length > 0)
  }

  @Test
  def Convert_String_Has_Content {
    assertTrue(Converter.instance.convert("class A {}").length > 0)
  }
  
}
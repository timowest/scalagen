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

import org.junit.Test
import org.junit.Assert._

class ReturnsTest {
  
  @Test
  def test {
    assertEquals(-1, printReturns())  
  }
  
  def printReturns(): Int = {
    for (i <- 0 until 10 if i > 2) i
    -1
  }
  
}
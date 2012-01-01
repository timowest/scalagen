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
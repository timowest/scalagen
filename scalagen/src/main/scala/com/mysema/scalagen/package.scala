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
package com.mysema

import japa.parser.ast.body._
import japa.parser.ast.expr._
import japa.parser.ast.stmt._
import _root_.scala.collection.JavaConversions
import _root_.scala.collection.Set

/**
 * scalagen provides common functionality for this package
 */
package object scalagen {
  
  type JavaCollection[T] = java.util.Collection[T]
  
  type JavaList[T] = java.util.List[T]
  
  type JavaSet[T] = java.util.Set[T]
  
  implicit def toJavaList[T](col: Seq[T]): JavaList[T] = JavaConversions.seqAsJavaList(col) 
  
  implicit def toJavaSet[T](col: Set[T]): JavaSet[T] = JavaConversions.setAsJavaSet(col)
      
  implicit def toScalaSeq[T](col: JavaList[T]): Seq[T] = {
    if (col != null) JavaConversions.asScalaBuffer(col) else Nil 
  }
  
  def toScalaList[T](col: JavaList[T]): List[T] = {
    if (col != null) JavaConversions.asScalaBuffer(col).toList else Nil 
  }
  
  implicit def toScalaSet[T](col: JavaSet[T]): Set[T] = {
    if (col != null) JavaConversions.asScalaSet(col) else Set[T]()
  }
    
}

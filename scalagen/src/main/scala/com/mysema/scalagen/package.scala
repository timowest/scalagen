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
  
  val PROPERTY = 0x00001000
  val LAZY     = 0x00002000
  val OBJECT   = 0x00004000
  val IMPLICIT = 0x00008000
    
  @inline
  def isEmpty(col: java.util.Collection[_]): Boolean = col == null || col.isEmpty

  implicit def toJavaList[T](col: Seq[T]): java.util.List[T] = JavaConversions.asJavaList(col) 
  
  implicit def toJavaSet[T](col: Set[T]): java.util.Set[T] = JavaConversions.asJavaSet(col)
      
  implicit def toScalaSeq[T](col: java.util.List[T]): Seq[T] = {
    if (col != null) JavaConversions.asBuffer(col) else List[T]() 
  }
  
  implicit def toScalaSet[T](col: java.util.Set[T]): Set[T] = {
    if (col != null) JavaConversions.asScalaSet(col) else Set[T]()
  }
  
  implicit def toRichModifiers(i: Int) = new RichModifiers(i)
  
  class RichModifiers(i: Int) {
    def isAbstract = ModifierSet.isAbstract(i)
    def isFinal = ModifierSet.isFinal(i)
    def isImplicit = ModifierSet.hasModifier(i, IMPLICIT)
    def isLazy = ModifierSet.hasModifier(i, LAZY)
    def isNative = ModifierSet.isNative(i)
    def isObject = ModifierSet.hasModifier(i, OBJECT)
    def isPrivate = ModifierSet.isPrivate(i)
    def isProtected = ModifierSet.isProtected(i)
    def isProperty = ModifierSet.hasModifier(i, PROPERTY)
    def isPublic = ModifierSet.isPublic(i)
    def isStatic = ModifierSet.isStatic(i)
    def isStrictfp = ModifierSet.isStrictfp(i)
    def isSynchronized = ModifierSet.isSynchronized(i)
    def isTransient = ModifierSet.isTransient(i)
    def isVolatile = ModifierSet.isVolatile(i)
    def hasModifier(mod: Int) = ModifierSet.hasModifier(i,mod)
    def addModifier(mod: Int) = ModifierSet.addModifier(i,mod)
    def removeModifier(mod: Int) = ModifierSet.removeModifier(i,mod)    
  }
  
}

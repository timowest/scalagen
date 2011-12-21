package com.mysema

import japa.parser.ast.body._
import japa.parser.ast.expr._
import japa.parser.ast.stmt._
import _root_.scala.collection.JavaConversions
import _root_.scala.collection.Set

package object scalagen {
    
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
    def isNative = ModifierSet.isNative(i)
    def isPrivate = ModifierSet.isPrivate(i)
    def isProtected = ModifierSet.isProtected(i)
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

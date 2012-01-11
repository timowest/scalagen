
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

import japa.parser.ast.Node
import japa.parser.ast.body._
import japa.parser.ast.expr._
import japa.parser.ast.stmt._
import _root_.scala.collection.JavaConversions
import _root_.scala.collection.Set

/**
 * Common helper methods for transformers and ScalaDumpVisitor
 */
trait Helpers {
  import UnitTransformer._
  
  val PROPERTY = 0x00001000
  val LAZY     = 0x00002000
  val OBJECT   = 0x00004000
  val IMPLICIT = 0x00008000  
  
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
  
  implicit def toRichBlock(b: BlockStmt) = new RichBlockStmt(b)
  
  class RichBlockStmt(b: BlockStmt) {
    def add(s: Statement) = b.getStmts.add(s)
    def apply(i: Int) = b.getStmts.get(i)
    def isEmpty = b.getStmts == null || b.getStmts.isEmpty
    def remove(s: Statement) = b.getStmts.remove(s)
    def removeAll(s: java.util.Collection[Statement]) = b.getStmts.removeAll(s)
    def size = if (b.getStmts != null) b.getStmts.size else 0
  }  
  
  implicit def toListBuilder[T <: Node](v: T) = new ListBuilder[T](v)
  
  class ListBuilder[T <: Node](v: T) {
    def asList: JavaList[T] = {
      val list = new java.util.ArrayList[T]()
      list.add(v)
      list
    }
  }  
   
  @inline
  def isEmpty(col: JavaCollection[_]): Boolean = col == null || col.isEmpty
    
  def extract(stmt: Statement): Statement = stmt match {
    case b: BlockStmt => if (b.size == 1) b(0) else b
    case _ => stmt
  } 
  
  def getAssignment(s: Statement): Assign = s match {
    case Stmt(a: Assign) => a
    case _ => null
  }
        
  def isAssignment(s: Statement): Boolean = s match {
    //case Stmt(Assign(Assign.assign,_,_)) => true
    case Stmt(_ assign _) => true
    case _ => false
  }
    
  def isThisConstructor(s: Statement): Boolean = s match {
    case ci: ConstructorInvocation => ci.isThis
    case _ => false
  }
  
  def isStatic(member: BodyDeclaration): Boolean = member match {
    case t: ClassOrInterface => t.getModifiers.isStatic || t.getModifiers.isObject || t.isInterface
    case t: Type => t.getModifiers.isStatic || t.getModifiers.isObject
    case f: Field => f.getModifiers.isStatic
    case m: Method => m.getModifiers.isStatic
    case i: Initializer => i.isStatic
    case _ => false
  }  
  
  def isHashCode(n: Method): Boolean = n match { 
    case Method("hashCode", Type.Int, Nil, _) => true
    case _ => false
  }
    
  def isEquals(n: Method): Boolean = n match {
    case Method("equals", Type.Boolean,_ :: Nil, _) => true
    case _ => false
  }
    
  def isToString(n: Method): Boolean = n match {
    case Method("toString", Type.String, Nil, _) => true
    case _ => false
  }
    
  def isReturnFieldStmt(stmt: Statement): Boolean = stmt match {
    case Return(field(_)) => true
    case _ => false
  }
  
  def isSetFieldStmt(stmt: Statement): Boolean = stmt match {
    //case Stmt(Assign(Assign.assign,_,_)) => true
    case Stmt(_ assign _) => true
    case _ => false
  }
      
}
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
    
  def extractStmt(stmt: Statement): Statement = stmt match {
    case b: BlockStmt => if (b.getStmts != null && b.getStmts.size == 1) b.getStmts.get(0) else b
    case _ => stmt
  } 
      
  def getAssignment(s: Statement): AssignExpr = {
    s.asInstanceOf[ExpressionStmt].getExpression.asInstanceOf[AssignExpr]
  }
  
  def isAssignment(s: Statement): Boolean = s match {
    case s: ExpressionStmt => s.getExpression.isInstanceOf[AssignExpr] &&
       s.getExpression().asInstanceOf[AssignExpr].getOperator.toString == "assign"
    case _ => false
  }

  def isStatic(member: BodyDeclaration): Boolean = member match {
    case t: TypeDeclaration => t.getModifiers.isStatic || t.getModifiers.isObject
    case f: FieldDeclaration => f.getModifiers.isStatic
    case m: MethodDeclaration => m.getModifiers.isStatic
    case i: InitializerDeclaration => i.isStatic
    case _ => false
  }  
  
  def isHashCode(n: MethodDeclaration): Boolean = {
    n.getName == "hashCode" && isEmpty(n.getParameters) 
  }
  
  def isEquals(n: MethodDeclaration): Boolean = {
    n.getName == "equals" && n.getParameters != null && n.getParameters.size == 1
  }
  
  def isToString(n: MethodDeclaration): Boolean = {
    n.getName == "toString" && isEmpty(n.getParameters)
  }
  

  
}
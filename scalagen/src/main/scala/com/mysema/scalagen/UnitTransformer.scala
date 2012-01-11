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
import japa.parser.ast.`type`._
import japa.parser.ast.visitor.ModifierVisitorAdapter
import japa.parser.ast.CompilationUnit
import japa.parser.ast.ImportDeclaration
import java.util.ArrayList

object UnitTransformer extends Helpers {
  
  @inline
  implicit def toNameExpr(s: String) = new NameExpr(s)
  
  @inline
  implicit def toVariableDeclaratorId(s: String) = new VariableDeclaratorId(s)
  
  @inline 
  implicit def toBlock(s: Statement) = {
    val block = new BlockStmt(new ArrayList[Statement]())
    block.getStmts.add(s)
    block
  }
  
  private def safeToString(obj: AnyRef): String = if (obj != null) obj.toString else null
            
  val BEAN_PROPERTY_IMPORT = new Import("scala.reflect.BeanProperty", false, false)

  val BEAN_PROPERTY = new MarkerAnnotation("BeanProperty")
    
  object assign {
    def unapply(a: Assign) = if (a.getOperator == Assign.assign) Some(a.getTarget, a.getValue) else None
  }
    
  object equals {
    def unapply(b: Binary) = if (b.getOperator == Binary.equals) Some(b.getLeft, b.getRight) else None 
  }
  
  object increment {
    def unapply(u: Unary) = if (u.getOperator.toString.endsWith("Increment")) Some(u.getExpr) else None
  }
  
  object less {
    def unapply(b: Binary) = if (b.getOperator == Binary.less) Some(b.getLeft, b.getRight) else None
  }
    
  object field {
    def unapply(f: Expression) = f match {
      case Field(scope, field) if scope == "this" => Some(field)
      case Name(field) => Some(field)
      case _ => None
    }
  }
  
  object isnull {
    def unapply(b: Binary) = {
      if (b.getOperator == Binary.equals && b.getRight.isInstanceOf[Null]) Some(b.getLeft)
      else None        
    }
  }
  
  type Annotation = AnnotationExpr 
  
  type AnnotationMember = AnnotationMemberDeclaration
    
  object Assign {
    val assign = AssignExpr.Operator.assign
    def unapply(a: Assign) = Some(a.getOperator, a.getTarget, a.getValue)
  }
  
  type Assign = AssignExpr
    
  object Binary {
    val or = BinaryExpr.Operator.or
    val and = BinaryExpr.Operator.and
    val equals = BinaryExpr.Operator.equals
    val notEquals = BinaryExpr.Operator.notEquals
    val less = BinaryExpr.Operator.less
    val greater = BinaryExpr.Operator.greater
    def unapply(b: Binary) = Some(b.getOperator, b.getLeft, b.getRight)
  }
  
  type Binary = BinaryExpr
  
  object Block {
    //def unapply(b: Block) = Some(if (b != null) toScalaList(b.getStmts) else Nil)
    def unapply(s: Statement) = s match {
      case b: Block => Some(if (b != null) toScalaList(b.getStmts) else Nil)
      case _ => Some(List(s))
    } 
  }
  
  type Block = BlockStmt
  
  type Body = BodyDeclaration
  
  object BooleanLiteral {
    def unapply(b: BooleanLiteral) = Some(b.getValue)
  }
  
  type BooleanLiteral = BooleanLiteralExpr
  
  object Catch {
    def unapply(c: Catch) = Some(c.getExcept, extract(c.getCatchBlock))
  }
  
  type Catch = CatchClause
  
  object ClassOrInterface {
    def unapply(c: ClassOrInterface) = Some(c.getName, toScalaList(c.getMembers))
  }
  
  type ClassOrInterface = ClassOrInterfaceDeclaration
  
  object Conditional {
    def unapply(c: Conditional) = Some(c.getCondition, c.getThenExpr, c.getElseExpr)
  }
  
  type Conditional = ConditionalExpr
  
  object Constructor {
    def unapply(c: Constructor) = Some(toScalaList(c.getParameters), extract(c.getBlock))
    def unapply(c: ConstructorInvocation) = Some(c.isThis, toScalaList(c.getArgs))
  }
  
  type Constructor = ConstructorDeclaration
  
  type ConstructorInvocation = ExplicitConstructorInvocationStmt
  
  object Field {
    def unapply(f: FieldAccess) = Some(safeToString(f.getScope), f.getField)
  }
    
  type Field = FieldDeclaration
    
  type FieldAccess = FieldAccessExpr
  
  object For {
    def unapply(f: For) = Some(toScalaList(f.getInit), f.getCompare, toScalaList(f.getUpdate), extract(f.getBody))
  }
  
  type For = ForStmt
  
  object If {
    def unapply(i: If) = Some(i.getCondition, extract(i.getThenStmt), extract(i.getElseStmt))
  }
  
  type If = IfStmt
  
  type Import = ImportDeclaration
  
  object Initializer {
    def unapply(i: Initializer) = Block.unapply(i.getBlock)
  }
  
  type Initializer = InitializerDeclaration
  
  type MarkerAnnotation = MarkerAnnotationExpr
  
  object Method {
    def unapply(m: Method) = Some(m.getName, m.getType, toScalaList(m.getParameters), extract(m.getBody))
    def unapply(m: MethodCall) = Some(safeToString(m.getScope), m.getName, toScalaList(m.getArgs))
  }
  
  type Method = MethodDeclaration
  
  type MethodCall = MethodCallExpr
  
  object Name {
    def unapply(n: Name) = Some(n.getName)
  }
  
  type Name = NameExpr
    
  type Null = NullLiteralExpr
    
  object Return {
    def unapply(r: Return) = Some(r.getExpr)
  }
  
  type Return = ReturnStmt
  
  type SingleMemberAnnotation = SingleMemberAnnotationExpr
  
  object Stmt {
    def unapply(s: ExpressionStmt) = Some(s.getExpression)
  }
  
  type This = ThisExpr
  
  object Type {
    val Boolean = new PrimitiveType(PrimitiveType.Primitive.Boolean)
    val Int = new PrimitiveType(PrimitiveType.Primitive.Int)
    val Object = new ReferenceType(new ClassOrInterfaceType("Object"))
    val String = new ReferenceType(new ClassOrInterfaceType("String"))
    val Void = new VoidType()
  }
  
  type Type = TypeDeclaration
  
  object Unary {
    def unapply(u: Unary) = Some(u.getOperator, u.getExpr)
  }
  
  type Unary = UnaryExpr
  
  object Variable {
    def unapply(v: VariableDeclarator) = Some(v.getId.getName, v.getInit)    
  }
  
  type Variable = VariableDeclarator
  
  object VariableDeclaration {
    def unapply(v: VariableDeclaration) = Some(v.getType, toScalaList(v.getVars))
  }
  
  type VariableDeclaration = VariableDeclarationExpr

  abstract class UnitTransformerBase extends ModifierVisitorAdapter[CompilationUnit] with UnitTransformer
  
}

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
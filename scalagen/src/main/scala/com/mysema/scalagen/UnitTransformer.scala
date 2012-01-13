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
    
  object set {
    def unapply(a: Assign) = if (a.getOperator == Assign.assign) Some(a.getTarget, a.getValue) else None
  }
    
  object === {
    def unapply(b: Binary) = if (b.getOperator == Binary.equals) Some(b.getLeft, b.getRight) else None 
  }
  
  object incr {
    def unapply(u: Unary) = if (u.getOperator.toString.endsWith("Increment")) Some(u.getExpr) else None
  }
  
  object lt {
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
    
  object Assign {
    val assign = AssignExpr.Operator.assign
    def unapply(a: Assign) = Some(a.getOperator, a.getTarget, a.getValue)
  }
    
  object Binary {
    val or = BinaryExpr.Operator.or
    val and = BinaryExpr.Operator.and
    val equals = BinaryExpr.Operator.equals
    val notEquals = BinaryExpr.Operator.notEquals
    val less = BinaryExpr.Operator.less
    val greater = BinaryExpr.Operator.greater
    def unapply(b: Binary) = Some(b.getOperator, b.getLeft, b.getRight)
  }
    
  object Block {
    //def unapply(b: Block) = Some(if (b != null) toScalaList(b.getStmts) else Nil)
    def unapply(s: Statement) = s match {
      case b: Block => Some(if (b != null) toScalaList(b.getStmts) else Nil)
      case _ => Some(List(s))
    } 
  }
  
  object Cast {
    def unapply(c: Cast) = Some(c.getExpr, c.getType)
  }
  
  object Catch {
    def unapply(c: Catch) = Some(c.getExcept, extract(c.getCatchBlock))
  }
  
  object ClassOrInterface {
    def unapply(c: ClassOrInterfaceDecl) = Some(c.getName, toScalaList(c.getMembers))
  }
  
  object Conditional {
    def unapply(c: Conditional) = Some(c.getCondition, c.getThenExpr, c.getElseExpr)
  }
  
  object Constructor {
    def unapply(c: Constructor) = Some(toScalaList(c.getParameters), extract(c.getBlock))
    def unapply(c: ConstructorInvocation) = Some(c.isThis, toScalaList(c.getArgs))
  }
  
  object Enclosed {
    def unapply(e: Enclosed) = Some(e.getInner)
  }
  
  object Field {
    def unapply(f: FieldAccess) = Some(safeToString(f.getScope), f.getField)
  }
  
  object For {
    def unapply(f: For) = Some(toScalaList(f.getInit), f.getCompare, toScalaList(f.getUpdate), extract(f.getBody))
  }
  
  object If {
    def unapply(i: If) = Some(i.getCondition, extract(i.getThenStmt), extract(i.getElseStmt))
  }
  
  object InstanceOf {
    def unapply(i: InstanceOf) = Some(i.getExpr, i.getType)
  }
  
  object Initializer {
    def unapply(i: Initializer) = Block.unapply(i.getBlock)
  }
    
  object Literal {
    def unapply(l: Literal) = l match {
      case b: BooleanLiteral => Some(b.getValue)
    }
  }
  
  object Method {
    def unapply(m: Method) = Some(m.getName, m.getType, toScalaList(m.getParameters), extract(m.getBody))
    def unapply(m: MethodCall) = Some(safeToString(m.getScope), m.getName, toScalaList(m.getArgs))
  }
  
  object Name {
    def unapply(n: Name) = Some(n.getName)
  }
  
  object Parameter {
    def unapply(p: Parameter) = Some(p.getId.getName)
  }
    
  object Return {
    def unapply(r: Return) = Some(r.getExpr)
  }
  
  object Stmt {
    def unapply(s: ExpressionStmt) = Some(s.getExpression)
  }
  
  object This {
    def unapply(t: This) = Some(t.getClassExpr)
  }
  
  object Type {
    val Boolean = new PrimitiveType(PrimitiveType.Primitive.Boolean)
    val Int = new PrimitiveType(PrimitiveType.Primitive.Int)
    val Object = new ReferenceType(new ClassOrInterfaceType("Object"))
    val String = new ReferenceType(new ClassOrInterfaceType("String"))
    val Void = new VoidType()
  }
    
  object Unary {
    def unapply(u: Unary) = Some(u.getOperator, u.getExpr)
  }
  
  object Variable {
    def unapply(v: VariableDeclarator) = Some(v.getId.getName, v.getInit)    
  }
  
  object VariableDeclaration {
    def apply(mod: Int, name: String, t: Type): VariableDeclaration = {
      val variable = new VariableDeclarator(name)
      new VariableDeclaration(mod, t, variable.asList)
    }
    def unapply(v: VariableDeclaration) = Some(v.getType, toScalaList(v.getVars))
  }
  
  type Annotation = AnnotationExpr 
  
  type AnnotationDecl = AnnotationDeclaration
  
  type AnnotationMember = AnnotationMemberDeclaration
  
  type Assign = AssignExpr
  
  type Binary = BinaryExpr
    
  type Block = BlockStmt
  
  type BodyDecl = BodyDeclaration
  
  type BooleanLiteral = BooleanLiteralExpr
  
  type Break = BreakStmt
    
  type Cast = CastExpr
  
  type Catch = CatchClause
  
  type ClassOrInterfaceDecl = ClassOrInterfaceDeclaration
  
  type ClassOrInterface = ClassOrInterfaceType
  
  type CompilationUnit = japa.parser.ast.CompilationUnit
  
  type Conditional = ConditionalExpr
    
  type Constructor = ConstructorDeclaration
  
  type ConstructorInvocation = ExplicitConstructorInvocationStmt
  
  type Enclosed = EnclosedExpr
  
  type EnumDecl = EnumDeclaration
  
  type Expression = japa.parser.ast.expr.Expression
  
  type ExpressionStmt = japa.parser.ast.stmt.ExpressionStmt
  
  type Field = FieldDeclaration
    
  type FieldAccess = FieldAccessExpr
    
  type For = ForStmt
  
  type Foreach = ForeachStmt
    
  type If = IfStmt
  
  type Import = ImportDeclaration
  
  type InstanceOf = InstanceOfExpr
  
  type Initializer = InitializerDeclaration
    
  type Literal = LiteralExpr
  
  type MarkerAnnotation = MarkerAnnotationExpr
    
  type Method = MethodDeclaration
  
  type MethodCall = MethodCallExpr
    
  type Name = NameExpr
    
  type Node = japa.parser.ast.Node
  
  type Null = NullLiteralExpr
  
  type ObjectCreation = ObjectCreationExpr
    
  type Parameter = japa.parser.ast.body.Parameter
  
  type Return = ReturnStmt
  
  type SingleMemberAnnotation = SingleMemberAnnotationExpr

  type Statement = japa.parser.ast.stmt.Statement
  
  type Switch = SwitchStmt
  
  type SwitchEntry = SwitchEntryStmt  
  
  type This = ThisExpr
  
  type Throw = ThrowStmt
  
  type Try = TryStmt
  
  type Type = japa.parser.ast.`type`.Type

  type TypeDecl = japa.parser.ast.body.TypeDeclaration
  
  type Unary = UnaryExpr
  
  type Variable = VariableDeclarator
  
  type VariableDeclaration = VariableDeclarationExpr
  
  type VariableDeclarator = japa.parser.ast.body.VariableDeclarator
  
  type VariableDeclaratorId = japa.parser.ast.body.VariableDeclaratorId

  type VoidType = japa.parser.ast.`type`.VoidType
  
  abstract class UnitTransformerBase extends ModifierVisitorAdapter[CompilationUnit] with UnitTransformer
  
}

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
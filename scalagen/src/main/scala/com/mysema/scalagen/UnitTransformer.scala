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
            
  val BEAN_PROPERTY_IMPORT = new Import("scala.reflect.BeanProperty", false, false)

  val BEAN_PROPERTY = new MarkerAnnotation("BeanProperty")
  
  type Annotation = AnnotationExpr 
  
  type AnnotationMember = AnnotationMemberDeclaration
  
  type Assign = AssignExpr
  
  type Binary = BinaryExpr
  
  type Body = BodyDeclaration
  
  type BooleanLiteral = BooleanLiteralExpr
  
  type ClassOrInterface = ClassOrInterfaceDeclaration
  
  type Conditional = ConditionalExpr
  
  type Constructor = ConstructorDeclaration
  
  type ConstructorInvocation = ExplicitConstructorInvocationStmt
  
  type Field = FieldDeclaration
  
  type FieldAccess = FieldAccessExpr
  
  type Import = ImportDeclaration
  
  type Initializer = InitializerDeclaration
  
  type MarkerAnnotation = MarkerAnnotationExpr
  
  type Method = MethodDeclaration
  
  type MethodCall = MethodCallExpr
  
  type Name = NameExpr
  
  type Return = ReturnStmt
  
  type SingleMemberAnnotation = SingleMemberAnnotationExpr
  
  type This = ThisExpr
  
  type Type = TypeDeclaration
  
  type Unary = UnaryExpr
  
  type Variable = VariableDeclarator
  
  type VariableDeclaration = VariableDeclarationExpr
      
}

abstract class UnitTransformerBase extends ModifierVisitorAdapter[Context] with UnitTransformer {
  
}

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
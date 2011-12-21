package com.mysema.scalagen 

import japa.parser.ast.body.BodyDeclaration
import japa.parser.ast.body.ClassOrInterfaceDeclaration
import japa.parser.ast.body.ConstructorDeclaration
import japa.parser.ast.body.FieldDeclaration
import japa.parser.ast.body.InitializerDeclaration
import japa.parser.ast.body.MethodDeclaration
import japa.parser.ast.body.TypeDeclaration
import japa.parser.ast.body.VariableDeclarator
import japa.parser.ast.body.VariableDeclaratorId
import japa.parser.ast.expr.AnnotationExpr
import japa.parser.ast.expr.AssignExpr
import japa.parser.ast.expr.BinaryExpr
import japa.parser.ast.expr.FieldAccessExpr
import japa.parser.ast.expr.MarkerAnnotationExpr
import japa.parser.ast.expr.MethodCallExpr
import japa.parser.ast.expr.NameExpr
import japa.parser.ast.expr.ThisExpr
import japa.parser.ast.expr.UnaryExpr
import japa.parser.ast.expr.VariableDeclarationExpr
import japa.parser.ast.stmt.ExplicitConstructorInvocationStmt
import japa.parser.ast.CompilationUnit
import japa.parser.ast.ImportDeclaration

object UnitTransformer {
  
  @inline
  implicit def toNameExpr(s: String) = new NameExpr(s)
  
  @inline
  implicit def toVariableDeclaratorId(s: String) = new VariableDeclaratorId(s)
  
  val BEAN_PROPERTY_IMPORT = new Import("scala.reflect.BeanProperty", false, false)

  val BEAN_PROPERTY = new MarkerAnnotation("BeanProperty")
  
  type Annotation = AnnotationExpr 
  
  type Assign = AssignExpr
  
  type Binary = BinaryExpr
  
  type Body = BodyDeclaration
  
  type ClassOrInterface = ClassOrInterfaceDeclaration
  
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
  
  type This = ThisExpr
  
  type Type = TypeDeclaration
  
  type Unary = UnaryExpr
  
  type Variable = VariableDeclarator
  
  type VariableDeclaration = VariableDeclarationExpr
  
}

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}
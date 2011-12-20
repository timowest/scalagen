package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
import java.util.ArrayList

/**
 * @author tiwe
 *
 */
object Constructors extends UnitTransformer {
  
 type ConstructorInvocation = ExplicitConstructorInvocationStmt
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      cu.getTypes.foreach { transform(cu,_) }      
    }
    cu
  }

  private def isThisConstructorInvocation(s: Statement): Boolean = {
    s.isInstanceOf[ConstructorInvocation] && 
    s.asInstanceOf[ConstructorInvocation].isThis
  }
  
  private def transform(cu: CompilationUnit, t: TypeDeclaration) {
    if (t.getMembers == null) {
      return
    }
    
    // get all constructors
    val constr = t.getMembers.collect { case c: ConstructorDeclaration => c }
      
    if (constr.isEmpty) {
      return
    }
    
    // get first without delegating
    val first = constr.find( c => 
      isEmpty(c.getBlock.getStmts) || !isThisConstructorInvocation(c.getBlock.getStmts.get(0)))
    
    // move in front of others
    first.filter(_ != constr(0)).foreach { c =>
      t.getMembers.remove(c)
      t.getMembers.add(t.getMembers.indexOf(constr(0)), c)
    }   
        
    // copy initializer, if constructor block has non-constructor statements
    val c = first.getOrElse(constr(0))  
        
    if (!isEmpty(c.getBlock.getStmts) && !c.getBlock.getStmts.filter(!_.isInstanceOf[ConstructorInvocation]).isEmpty) {
      
      processStatements(cu, t, c)
      
      val initializer = new InitializerDeclaration(false, c.getBlock)
      t.getMembers.add(t.getMembers.indexOf(c), initializer)
    }    
    
  }
  
  private def processStatements(cu: CompilationUnit, t: TypeDeclaration, c: ConstructorDeclaration) {
    val fields = t.getMembers.collect { case f: FieldDeclaration => f }
    val variables = fields.flatMap(_.getVariables).map(v => (v.getId.getName, v)).toMap
    val variableToField = fields.flatMap(f => f.getVariables.map(v => (v.getId.getName,f)) ).toMap
    
    // TODO : simplify
      
    // go through statements and map assignments to variable initializers
    c.getBlock.getStmts.collect { case s: ExpressionStmt => s }
      .filter(s => s.getExpression.isInstanceOf[AssignExpr] && 
        s.getExpression.asInstanceOf[AssignExpr].getOperator.toString == "assign")
      .foreach { s =>
      val assign = s.getExpression.asInstanceOf[AssignExpr]
      if (assign.getTarget.isInstanceOf[FieldAccessExpr]) {
        val fieldAccess = assign.getTarget.asInstanceOf[FieldAccessExpr]
        if (fieldAccess.getScope.isInstanceOf[ThisExpr] &&
            variables.contains(fieldAccess.getField)) {
          if (fieldAccess.getField == assign.getValue.toString) {
            val field = variableToField(fieldAccess.getField)
            if (field.getAnnotations != null && field.getAnnotations.contains(BeanProperties.BEAN_PROPERTY)) {
              c.getParameters.find(_.getId.getName == fieldAccess.getField)
                .foreach {p =>
                  if (p.getAnnotations == null) {
                    p.setAnnotations(new ArrayList[AnnotationExpr]())
                  }
                  p.getAnnotations.add(BeanProperties.BEAN_PROPERTY)
                  p.setModifiers(field.getModifiers)
                }
            }
            // remove field, as constructor parameter can be used
            field.getVariables.remove(variables(fieldAccess.getField))  
            
          } else {
            // remove statement, put init to field
            variables(fieldAccess.getField).setInit(assign.getValue)            
          }            
          c.getBlock.getStmts.remove(s)  
        }
      }
    }
    
    // remove empty field declarations
    fields.filter(_.getVariables.isEmpty).foreach { t.getMembers.remove(_) }
    
  }
  
}

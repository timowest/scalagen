package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import japa.parser.ast.expr._
import java.util.ArrayList
import UnitTransformer._

/**
 * @author tiwe
 *
 */
object Constructors extends UnitTransformer {
   
  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      cu.getTypes.foreach { transform(cu,_) }      
    }
    cu
  }

  private def isThisConstructor(s: Statement): Boolean = {
    s.isInstanceOf[ConstructorInvocation] && 
    s.asInstanceOf[ConstructorInvocation].isThis
  }
  
  private def transform(cu: CompilationUnit, t: Type) {
    if (t.getMembers == null) {
      return
    }
    
    // get all constructors
    val constr = t.getMembers.collect { case c: Constructor => c }
      
    if (constr.isEmpty) {
      return
    }
    
    // get first without delegating
    val first = constr.find( c => 
      isEmpty(c.getBlock.getStmts) || !isThisConstructor(c.getBlock.getStmts.get(0)))
    
    // move in front of others
    first.filter(_ != constr(0)).foreach { c =>
      t.getMembers.remove(c)
      t.getMembers.add(t.getMembers.indexOf(constr(0)), c)
    }   
        
    // copy initializer, if constructor block has non-constructor statements
    val c = first.getOrElse(constr(0))  
        
    if (!isEmpty(c.getBlock.getStmts) && !c.getBlock.getStmts.filter(!_.isInstanceOf[ConstructorInvocation]).isEmpty) {
      
      processStatements(cu, t, c)
      
      if (!isEmpty(c.getBlock.getStmts)) {
        val initializer = new Initializer(false, c.getBlock)
        t.getMembers.add(t.getMembers.indexOf(c), initializer)  
      }      
    }    
    
  }
  
  private def processStatements(cu: CompilationUnit, t: Type, c: Constructor) {
    val fields = t.getMembers.collect { case f: Field => f }
    val variables = fields.flatMap(_.getVariables).map(v => (v.getId.getName, v)).toMap
    val variableToField = fields.flatMap(f => f.getVariables.map(v => (v.getId.getName,f)) ).toMap
      
    // go through statements and map assignments to variable initializers
    c.getBlock.getStmts.collect { case s: ExpressionStmt => s }
      .filter(s => s.getExpression.isInstanceOf[Assign] && 
        s.getExpression.asInstanceOf[Assign].getOperator.toString == "assign")
      .foreach { s =>
      val assign = s.getExpression.asInstanceOf[Assign]
      if (assign.getTarget.isInstanceOf[FieldAccess]) {
        val fieldAccess = assign.getTarget.asInstanceOf[FieldAccess]
        processFieldAssign(s, assign, fieldAccess, c, variables, variableToField)
      } else if (assign.getTarget.isInstanceOf[Name]) {
        val namedTarget = assign.getTarget.asInstanceOf[Name]
        if (variables.contains(namedTarget.getName)) {
          if (assign.getValue.isInstanceOf[Name]) { // field = parameter
            val namedValue = assign.getValue.asInstanceOf[Name]
            val param = c.getParameters.find(_.getId.getName == namedValue.getName).get
            val field = variableToField(namedTarget.getName)
            // rename parameter to field name
            param.setId(namedTarget.getName)
            if (field.getAnnotations != null) {
              copyAnnotationsAndModifiers(field, param)
            }
            // remove field
            field.getVariables.remove(variables(namedTarget.getName))
          } else { // field = ?!?
            variables(namedTarget.getName).setInit(assign.getValue)              
          }          
          c.getBlock.getStmts.remove(s)
        }
      }
    }
    
    // remove empty field declarations
    fields.filter(_.getVariables.isEmpty).foreach { t.getMembers.remove(_) }
    
  }

  private def processFieldAssign(s: ExpressionStmt, assign: Assign, fieldAccess: FieldAccess, 
      c: Constructor, variables: Map[String, Variable], variableToField: Map[String, Field] ) {
    if (fieldAccess.getScope.isInstanceOf[This] &&
        variables.contains(fieldAccess.getField)) {
      if (fieldAccess.getField == assign.getValue.toString) {
        val field = variableToField(fieldAccess.getField)
        if (field.getAnnotations != null) {
          c.getParameters.find(_.getId.getName == fieldAccess.getField)
            .foreach(copyAnnotationsAndModifiers(field,_))
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
  
  private def copyAnnotationsAndModifiers(f: Field, p: Parameter) {
    if (p.getAnnotations == null) {
      p.setAnnotations(new ArrayList[Annotation]())
    }
    p.getAnnotations.addAll(f.getAnnotations)
    p.setModifiers(f.getModifiers)
  }  
  
}

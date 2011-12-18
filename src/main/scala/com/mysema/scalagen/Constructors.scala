package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._

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

  private def isThisConstructorInvocation(s: Statement): Boolean = {
    s.isInstanceOf[ExplicitConstructorInvocationStmt] && s.asInstanceOf[ExplicitConstructorInvocationStmt].isThis
  }
  
  private def transform(cu: CompilationUnit, t: TypeDeclaration) {
    if (t.getMembers == null) {
      return
    }
    
    // get all constructors
    val constr = t.getMembers.collect { case c: ConstructorDeclaration => c }
      
    if (!constr.isEmpty) {
      
      // get first without delegating
      val first = constr.find( c => 
        isEmpty(c.getBlock.getStmts) || !isThisConstructorInvocation(c.getBlock.getStmts.get(0)))
    
      // move in front of others
      first.filter(_ != constr(0)).foreach { c =>
        t.getMembers.remove(c)
        t.getMembers.add(t.getMembers.indexOf(constr(0)), c)
      }   
        
      // copy initializer
      val c = first.getOrElse(constr(0))  
      if (!isEmpty(c.getBlock.getStmts)) {
        var initializer = new InitializerDeclaration(false, c.getBlock)
        t.getMembers.add(t.getMembers.indexOf(c), initializer)
      }    
    }   
      

  }
}

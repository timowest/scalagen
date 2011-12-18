package com.mysema.scalagen

import japa.parser.ast.CompilationUnit
import japa.parser.ast.body._
import japa.parser.ast.stmt._
import scala.collection.JavaConversions._

object Constructors extends UnitTransformer {

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      cu.getTypes.foreach { transform(cu,_) }      
    }
    cu
  }

  private def transform(cu: CompilationUnit, t: TypeDeclaration) {
    if (t.getMembers == null) {
      return
    }
    
    // get all constructors
    val constr = t.getMembers()
      .filter(_.isInstanceOf[ConstructorDeclaration])
      .map(_.asInstanceOf[ConstructorDeclaration])
      
    if (!constr.isEmpty) {
      
      // get first without delegating
      val first = constr.find( c =>
        c.getBlock.getStmts == null || (!c.getBlock.getStmts.isEmpty &&
        !c.getBlock.getStmts.get(0).isInstanceOf[ExplicitConstructorInvocationStmt]))        
    
      // move to first
      first.filter(_ != constr(0)).foreach { c =>
        t.getMembers.remove(c)
        t.getMembers.add(t.getMembers.indexOf(constr(0)), c)
      }   
        
      // copy initializer
      val c = first.getOrElse(constr(0))  
      if (c.getBlock.getStmts != null && !c.getBlock.getStmts.isEmpty) {
        var initializer = new InitializerDeclaration(false, c.getBlock)
        t.getMembers.add(t.getMembers.indexOf(c), initializer)
      }    
    }   
      

  }
}

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

import japa.parser.ast._
import japa.parser.ast.body._
import japa.parser.ast.expr._
import japa.parser.ast.stmt._
import japa.parser.ast.`type`._
import japa.parser.ast.visitor.VoidVisitor
import java.util.ArrayList
import java.util.HashMap
import java.util.Iterator
import java.util.List
import org.apache.commons.lang3.StringUtils

object ScalaDumpVisitor {
    
  private val PARAMETRIZED = Set("Class","Comparable","Enum","Iterable")
  
  private val UTIL_PARAMETRIZED = Set("Collection","List","Set","Map")
  
  private val METHOD_REPLACEMENTS = Map("equals"->"==") 
  
  private val SKIPPED_ANNOTATIONS = Set("Override","SuppressWarnings","Nullable")
  
  private val PRIMITIVES = Set("Boolean","Byte","Character","Double","Float","Integer","Long","Short")
  
  private val SHORT_FORM = Set("query","eq","ne","lt","until","gt","size","hasNext","toString","hashCode","equals","!=","keys","values","length")
  
  private val RESERVED = Set("def","match","object","type","val","var")
  
  private val JAVA_TYPES = Set("Iterable")

  class Context {  
    var arrayAccess = false
    var classOf = false
    var label: String = _   
    var skip = false
    var assignType: Type = _
    var inObjectEquals = false
    var returnOn = false
    var typeArg = false
    var imports = Map[String,String]()
    //var split = false
  }
  
}


/**
 * ScalaDumpVisitor is a serializing visitor for CompilationUnit instances
 *
 */
class ScalaDumpVisitor extends VoidVisitor[ScalaDumpVisitor.Context] with Helpers {
  import ScalaDumpVisitor._
    
  private val printer = new SourcePrinter()

  def getSource: String = printer.source
    
  private def print(node: Node, arg: Context): String = {
    val v = new ScalaDumpVisitor()
    node.accept(v, arg)
    v.getSource
  }
  
  private def printMethodModifiers(m: Int) {
    printModifiers(ModifierSet.removeModifier(m, ModifierSet.ABSTRACT))
  }
  
  private def printModifiers(m: Int) {
    val modifiers: RichModifiers = new RichModifiers(m)
    if (modifiers.isTransient) {
      printer.print("@transient ")
    }    
    if (modifiers.isVolatile) {
      printer.print("@volatile ")
    }
    
    if (modifiers.isPrivate) {
      printer.print("private ")
    } else if (modifiers.isProtected) {
      printer.print("protected ")
    } else if (modifiers.isPublic) {
    }
    
    if (modifiers.isLazy) {
      printer.print("lazy ")
    }
    
    if (modifiers.isImplicit) {
      printer.print("implicit ")
    }
    
    if (modifiers.isAbstract) {
      printer.print("abstract ")
    }
    if (modifiers.isStatic) {
      // skip
    }
    if (modifiers.isFinal) {
      // skip
    }
    if (modifiers.isNative) {
      printer.print("/* native */ ")
    }
    if (modifiers.isStrictfp) {
      printer.print("/* strictfp */ ")
    }
    if (modifiers.isSynchronized) {
      printer.print("/* synchronized */ ")
    }
  }

  private def printMembers(members: List[BodyDeclaration], arg: Context) {
    for (member <- members) {
      printer.printLn()
      member.accept(this, arg)
      printer.printLn()
    }
  }

  private def printMemberAnnotations(annotations: List[AnnotationExpr], arg: Context): Boolean = {
    var hasOverride = false
    if (annotations != null) {
      for (a <- annotations) {
        if (!SKIPPED_ANNOTATIONS.contains(a.getName.getName)) {
          a.accept(this, arg)
          printer.printLn()
        } else {
          hasOverride |= a.getName.getName == "Override"
        }
      }
    }
    hasOverride
  }

  private def printAnnotations(annotations: List[AnnotationExpr], arg: Context) {
    if (annotations != null) {
      for (a <- annotations if !SKIPPED_ANNOTATIONS.contains(a.getName.getName)) {
        a.accept(this, arg)
        printer.print(" ")
      }
    }
  }

  private def printTypeArgs(args: List[Type], arg: Context) {
    if (args != null) {
      val typeArg = arg.typeArg
      arg.typeArg = true
      printer.print("[")
      var i = args.iterator()
      while (i.hasNext) {
        var t = i.next()
        t.accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
      printer.print("]")
      arg.typeArg = typeArg
    }
  }

  private def printTypeParameters(args: List[TypeParameter], arg: Context) {
    if (args != null) {      
      printer.print("[")
      var i = args.iterator()
      while (i.hasNext) {
        var t = i.next()
        t.accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
      printer.print("]")      
    }
  }

  private def printArguments(args: List[Expression], arg: Context) {
    printer.print("(")
    if (args != null) {
      var i = args.iterator()
      while (i.hasNext) {
        var e = i.next()
        e.accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    printer.print(")")
  }

  private def printJavadoc(javadoc: JavadocComment, arg: Context) {
    if (javadoc != null) {
      javadoc.accept(this, arg)
    }
  }

  def visit(n: CompilationUnit, arg: Context) {
    if (n.getPackage != null) {
      n.getPackage.accept(this, arg)
    }
    if (n.getImports != null) {
      for (i <- n.getImports) {
        i.accept(this, arg)
      }      
    }
    
    arg.imports = n.getImports
      .filter(i => !i.isAsterisk && !i.isStatic)
      .map(i => split(i.getName).swap).toMap
    
    printer.print("//remove if not needed\n")
    printer.print("import scala.collection.JavaConversions._\n")
    printer.printLn()
    
    if (n.getPackage != null && !isEmpty(n.getPackage.getAnnotations)) {
      printMemberAnnotations(n.getPackage.getAnnotations, arg)
      printer.printLn("package object " + split(n.getPackage.getName)._2 + " {")
      printer.printLn("}")
      printer.printLn()
    }
    
    if (n.getTypes != null) {
      var i = n.getTypes.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        printer.printLn()
        if (i.hasNext) {
          printer.printLn()
        }
      }
    }
    
    arg.imports = Map[String,String]()
  }
  
  private def split(name: NameExpr): (String, String) = {
    val str = name.toString
    val separator = str.lastIndexOf('.')
    (str.substring(0,separator), str.substring(separator+1)) 
  }
   
  def visit(n: PackageDeclaration, arg: Context) {
    printer.print("package ")
    if (!isEmpty(n.getAnnotations)) {
      printer.print(split(n.getName)._1) 
    } else {
      n.getName.accept(this, arg)  
    }    
    printer.printLn()
    printer.printLn()
  }

  def visit(n: NameExpr, arg: Context) {
    visitName(n.getName)   
  }
  
  def visitName(name: String) {
    if (RESERVED.contains(name)) {
      printer.print("`"+name+"`") 
    } else {
      printer.print(name)  
    }
  }

  def visit(n: QualifiedNameExpr, arg: Context) {
    n.getQualifier.accept(this, arg)
    printer.print(".")
    printer.print(n.getName)
  }
  
  def visit(n: ImportDeclaration, arg: Context) {
    printer.print("import ")
    if (n.getName.getName.endsWith(".Array") && !n.isAsterisk) {
      val className = n.getName.getName
      val pkg = className.substring(0, className.lastIndexOf('.'))
      printer.print(pkg + ".{Array => _Array}")
    } else {
      n.getName.accept(this, arg)
      if (n.isAsterisk) {
        printer.print("._")
      }
    }
    
    printer.printLn()
  }

  def visit(n: ClassOrInterfaceDeclaration, arg: Context) {
    // TODO : simplify
    printJavadoc(n.getJavaDoc, arg)
    printMemberAnnotations(n.getAnnotations, arg)
    printModifiers(n.getModifiers)
    if (n.getModifiers.isObject) {
      printer.print("object ")
    } else if (n.isInterface) {
      printer.print("trait ")
    } else {
      printer.print("class ")
    }
    printer.print(n.getName)
    printTypeParameters(n.getTypeParameters, arg)
    var constr = getFirstConstructor(n.getMembers)
    var superInvocation: Option[ExplicitConstructorInvocationStmt] = None
    if (constr != null) {
      if (!isEmpty(constr.getParameters) || !constr.getModifiers.isPublic) {
        printConstructor(constr, arg, true)
        superInvocation = constr.getBlock.getStmts
          .collect({ case x: ExplicitConstructorInvocationStmt => x })
          .filter(!_.isThis).headOption
      }
    }
    var superTypes = new ArrayList[ClassOrInterfaceType]()
    if (n.getExtends != null) {
      superTypes.addAll(n.getExtends)
    }
    if (n.getImplements != null) {
      superTypes.addAll(n.getImplements)
    }
    if (printer.lineLength > 80) {
      printer.printLn()
      printer.print("   ")
    }    
    if (!superTypes.isEmpty) {
      printer.print(" extends ")
      var i = superTypes.iterator()
      i.next().accept(this, arg)
      superInvocation.foreach { s => 
        constr.getBlock.getStmts.remove(s)
        printArguments(s.getArgs, arg)
      }
      while (i.hasNext) {
        printer.print(" with ")
        i.next().accept(this, arg)        
      }
    }
    
    if (!isEmpty(n.getMembers)) {
      printer.printLn(" {")
      printer.indent()
      printMembers(n.getMembers, arg)
      printer.unindent()
      printer.print("}")
    }    
  }

  private def getFirstConstructor(members: List[BodyDeclaration]): ConstructorDeclaration = {
    if (members == null) {
      return null
    }
    var i = members.iterator()
    while (i.hasNext) {
      var member = i.next()
      if (member.isInstanceOf[ConstructorDeclaration]) {
        i.remove()
        return member.asInstanceOf[ConstructorDeclaration]
      }
    }
    null
  }

  def visit(n: EmptyTypeDeclaration, arg: Context) {
    printJavadoc(n.getJavaDoc, arg)
  }

  def visit(n: JavadocComment, arg: Context) {
    printer.printLn("/**")
    for (line <- StringUtils.split(n.getContent.trim, '\n')) {
      printer.printLn(" " + line.trim)
    }
    printer.printLn(" */")
  }

  def visit(n: ClassOrInterfaceType, arg: Context) {
    if (n.getScope != null) {
      n.getScope.accept(this, arg)
      printer.print(".")    
    } else if (!arg.classOf && !arg.typeArg && PRIMITIVES.contains(n.getName)) {    
      // primitive types are favored for class literals and type arguments
      printer.print("java.lang.")
    } else if (JAVA_TYPES.contains(n.getName)) {  
      printer.print("java.lang.")
    }
    if (n.getName == "Object") {
      if (arg.inObjectEquals || arg.typeArg) {
        printer.print("Any") 
      } else {
        printer.print("AnyRef")
      }
    } else if (n.getScope == null && n.getName == "Array") { 
      // TODO : only if Array import is present
      printer.print("_Array")
    } else {
      printer.print(n.getName)  
    }
    if (isEmpty(n.getTypeArgs)) {
      if (PARAMETRIZED.contains(n.getName)) {
        printer.print("[_]")  
      } else if (UTIL_PARAMETRIZED.contains(n.getName) && 
          arg.imports.getOrElse(n.getName,"") == "java.util") {
        printer.print(if (n.getName == "Map") "[_,_]" else "[_]")
      }       
    }    
    printTypeArgs(n.getTypeArgs, arg)
  }

  def visit(n: TypeParameter, arg: Context) {
    printer.print(n.getName)
    if (n.getTypeBound != null) {
      printer.print(" <: ")
      var i = n.getTypeBound.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(" with ")
        }
      }
    }
  }

  def visit(n: PrimitiveType, arg: Context) {
    printer.print(n.getType.name)
  }

  def visit(n: ReferenceType, arg: Context) {
    val typeArg = arg.typeArg
    for (i <- 0 until n.getArrayCount) {    
      printer.print("Array[")
      arg.typeArg = true
    }
    n.getType.accept(this, arg)
    arg.typeArg = typeArg
    for (i <- 0 until n.getArrayCount) {
      printer.print("]")
    }
  }

  def visit(n: WildcardType, arg: Context) {
    printer.print("_")
    if (n.getExtends != null) {
      printer.print(" <: ")
      n.getExtends.accept(this, arg)
    }
    if (n.getSuper != null) {
      printer.print(" >: ")
      n.getSuper.accept(this, arg)
    }
  }

  def visit(n: FieldDeclaration, arg: Context) {
    val oldType = arg.assignType
    arg.assignType = n.getType
    printJavadoc(n.getJavaDoc, arg)
    val modifier = if (ModifierSet.isFinal(n.getModifiers)) "val " else "var "
    val i = n.getVariables.iterator()
    while (i.hasNext) {
      var v = i.next()
      printMemberAnnotations(n.getAnnotations, arg)
      printModifiers(n.getModifiers)
      printer.print(modifier)
      
      v.getId.accept(this, arg)
      if (v.getInit == null || modifier != "val ") {
        printer.print(": ")
        n.getType.accept(this, arg)
      }
      if (v.getInit == null) {
        printer.print(" = _")
      } else {
        printer.print(" = ")
        v.getInit.accept(this, arg)
      }

      if (i.hasNext) {
        printer.printLn()
        printer.printLn()
      }
    }
    arg.assignType = oldType
  }

  def visit(n: VariableDeclarator, arg: Context) {
    n.getId.accept(this, arg)
    if (n.getInit != null) {
      printer.print(" = ")
      n.getInit.accept(this, arg)
    }
  }

  def visit(n: VariableDeclaratorId, arg: Context) {
    visitName(n.getName)
//    for (i <- 0 until n.getArrayCount) { 
//      printer.print("[]") 
//    }
  }

  def visit(n: ArrayInitializerExpr, arg: Context) {
    printer.print("Array(")
    if (n.getValues != null) {
      var i = n.getValues.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    printer.print(")")
  }

  def visit(n: VoidType, arg: Context) {
    printer.print("Unit")
  }

  def visit(n: ArrayAccessExpr, arg: Context) {
    val arrayAccess = arg.arrayAccess
    arg.arrayAccess = true
    n.getName.accept(this, arg)
    arg.arrayAccess = arrayAccess
    printer.print("(")
    n.getIndex.accept(this, arg)
    printer.print(")")
  }

  def visit(n: ArrayCreationExpr, arg: Context) {    
    if (n.getDimensions != null) {
      printer.print("new ")
      
      if (arg.assignType != null) {
        arg.assignType.accept(this, arg) 
      } else {
        val max = if (n.getDimensions != null) n.getArrayCount + 1 else n.getArrayCount        
        for (i <- 0 until max) {
          printer.print("Array[")
        }
        val typeArg = arg.typeArg
        arg.typeArg = true
        n.getType.accept(this, arg)
        arg.typeArg = typeArg
        for (i <- 0 until max) {
          printer.print("]")
        }
      }       
      
      printer.print(n.getDimensions.map(print(_,arg)).mkString("(",", ",")"))
    } else {
      n.getInitializer.accept(this, arg)
    }
  }
  
  def visit(n: AssignExpr, arg: Context) {    
    n.getTarget.accept(this, arg)
    printer.print(" ")
    import AssignExpr.{Operator => Op}
    val symbol = n.getOperator match {
      case Op.assign => "="
      case Op.and => "&="
      case Op.or => "|="
      case Op.xor => "^="
      case Op.plus => "+="
      case Op.minus => "-="
      case Op.rem => "%="
      case Op.slash => "/="
      case Op.star => "*="
      case Op.lShift => "<<="
      case Op.rSignedShift => ">>="
      case Op.rUnsignedShift => ">>>="
    }
    printer.print(symbol)
    printer.print(" ")
    n.getValue.accept(this, arg)
  }

  def visit(n: BinaryExpr, arg: Context) {
    n.getLeft.accept(this, arg)
    printer.print(" ")
    import BinaryExpr.{Operator => Op}
    val symbol = n.getOperator match {
      case Op.or => "||"
      case Op.and => "&&"
      case Op.binOr => "|"
      case Op.binAnd => "&"
      case Op.xor => "^"
      case Op.equals => "=="
      case Op.notEquals => "!="
      case Op.less => "<"
      case Op.greater => ">"
      case Op.lessEquals => "<="
      case Op.greaterEquals => ">="
      case Op.lShift => "<<"
      case Op.rSignedShift => ">>"
      case Op.rUnsignedShift => ">>>"
      case Op.plus => "+"
      case Op.minus => "-"
      case Op.times => "*"
      case Op.divide => "/"
      case Op.remainder => "%"
    }
    printer.print(symbol)
    printer.print(" ")
    if (print(n.getLeft, arg).length > 50 || print(n.getRight, arg).length > 50) {
      printer.printLn()
      printer.print("  ")
    }    
    n.getRight.accept(this, arg)
  }

  def visit(n: CastExpr, arg: Context) {
    n.getExpr.accept(this, arg)
    if (n.getType.isInstanceOf[PrimitiveType]) {
      printer.print(".to")
      n.getType.accept(this, arg)
    } else {
      printer.print(".asInstanceOf[")
      n.getType.accept(this, arg)
      printer.print("]")  
    }    
  }

  def visit(n: ClassExpr, arg: Context) {
    printer.print("classOf[")
    arg.classOf = true
    n.getType.accept(this, arg)
    arg.classOf = false
    printer.print("]")
  }

  def visit(n: ConditionalExpr, arg: Context) {
    printer.print("if (")
    n.getCondition.accept(this, arg)
    printer.print(") ")
    n.getThenExpr.accept(this, arg)
    printer.print(" else ")
    n.getElseExpr.accept(this, arg)
  }

  def visit(n: EnclosedExpr, arg: Context) {
    if (n.getInner.isInstanceOf[CastExpr]) {
      n.getInner.accept(this, arg)
    } else {
      printer.print("(")
      n.getInner.accept(this, arg)
      printer.print(")")  
    }    
  }

  def visit(n: FieldAccessExpr, arg: Context) {
    n.getScope.accept(this, arg)
    printer.print(".")
    visitName(n.getField)
  }

  def visit(n: InstanceOfExpr, arg: Context) {
    n.getExpr.accept(this, arg)
    printer.print(".isInstanceOf[")
    n.getType.accept(this, arg)
    printer.print("]")
  }

  def visit(n: CharLiteralExpr, arg: Context) {
    printer.print("'")
    printer.print(n.getValue)
    printer.print("'")
  }

  def visit(n: DoubleLiteralExpr, arg: Context) {
    printer.print(n.getValue)
  }

  def visit(n: IntegerLiteralExpr, arg: Context) {
    printer.print(n.getValue)
  }

  def visit(n: LongLiteralExpr, arg: Context) {
    printer.print(n.getValue)
  }

  def visit(n: IntegerLiteralMinValueExpr, arg: Context) {
    printer.print(n.getValue)
  }

  def visit(n: LongLiteralMinValueExpr, arg: Context) {
    printer.print(n.getValue)
  }

  def visit(n: StringLiteralExpr, arg: Context) {
    printer.print("\"")
    printer.print(n.getValue)
    printer.print("\"")
  }

  def visit(n: BooleanLiteralExpr, arg: Context) {
    printer.print(String.valueOf(n.getValue))
  }

  def visit(n: NullLiteralExpr, arg: Context) {
    printer.print("null")
  }

  def visit(n: ThisExpr, arg: Context) {
    if (n.getClassExpr != null) {
      n.getClassExpr.accept(this, arg)
      printer.print(".")
    }
    printer.print("this")
  }

  def visit(n: SuperExpr, arg: Context) {
    if (n.getClassExpr != null) {
      n.getClassExpr.accept(this, arg)
      printer.print(".")
    }
    printer.print("super")
  }

  def visit(n: MethodCallExpr, arg: Context) {
    //val split = arg.split
    var args = if (n.getArgs == null) 0 else n.getArgs.size
    val shortForm = SHORT_FORM.contains(n.getName) && args < 2
    if (n.getScope != null) {
      val split = print(n.getScope, arg).length > 50
      n.getScope.accept(this, arg)
      if (split) {
        printer.printLn()
        printer.print("  ")
      }
      printer.print(if ((shortForm && args == 1)) " " else ".")
    }
    if (METHOD_REPLACEMENTS.contains(n.getName)) {
      printer.print(METHOD_REPLACEMENTS(n.getName))
    } else {
      visitName(n.getName)
    }   
    printTypeArgs(n.getTypeArgs, arg)
    if (n.getName == "asList" && n.getScope != null && n.getScope.toString == "Arrays" && n.getArgs.size == 1) {
      // assume Arrays.asList is called with an array argument
      printer.print("(")
      n.getArgs().get(0).accept(this, arg)
      printer.print(":_*)")
    } else if (arg.arrayAccess) {  
      printArguments(n.getArgs, arg)    
    } else if (shortForm) {
      if (args == 1) {
        printer.print(" ")
        n.getArgs.get(0).accept(this, arg)
      }
    } else if (!(n.getName.startsWith("get") || n.getName.startsWith("is")) || args > 0) {
      printArguments(n.getArgs, arg)
    }
    //arg.split = split
  }
  
  def visit(n: ObjectCreationExpr, arg: Context) {
    if (n.getScope != null) {
      n.getScope.accept(this, arg)
      printer.print(".")
    }
    printer.print("new ")
    printTypeArgs(n.getTypeArgs, arg)
    n.getType.accept(this, arg)
    printArguments(n.getArgs, arg)
    if (n.getAnonymousClassBody != null) {
      printer.printLn(" {")
      printer.indent()
      printMembers(n.getAnonymousClassBody, arg)
      printer.unindent()
      printer.print("}")
    }
  }

  def visit(n: UnaryExpr, arg: Context) {
    import UnaryExpr.{Operator => Op}
    
    // !x.equals(y) into x != y
    if (n.getOperator == Op.not && n.getExpr.isInstanceOf[MethodCallExpr] &&
        n.getExpr.asInstanceOf[MethodCallExpr].getName == "equals") {
      val method = n.getExpr.asInstanceOf[MethodCallExpr]
      new MethodCallExpr(method.getScope, "!=", method.getArgs).accept(this, arg)
      return
    }
    
    printer.print(n.getOperator match {
      case Op.positive => "+"
      case Op.negative => "-"
      case Op.inverse => "~"
      case Op.not => "!"
//      case Op.preIncrement => "+= 1"
//      case Op.preDecrement => "-= 1"
      case _ => ""
    })
    n.getExpr.accept(this, arg)
    printer.print(n.getOperator match {
      case Op.posIncrement => " += 1"
      case Op.posDecrement => " -= 1"
      case _ => ""
    })
  }

  def visit(n: ConstructorDeclaration, arg: Context) {
    printConstructor(n, arg, false)
  }

  private def printConstructor(n: ConstructorDeclaration, arg: Context, first: Boolean) {
    if (!first) {
      printJavadoc(n.getJavaDoc, arg)  
    }    
    printMemberAnnotations(n.getAnnotations, arg)
    if (first && n.getModifiers.isPrivate) {
      printer.print(" ")
    }
    printModifiers(n.getModifiers)
    if (!first) {
      printer.print("def this")
      printTypeParameters(n.getTypeParameters, arg)
    }    
    printer.print("(")
    if (n.getParameters != null) {
      var lineBreaks = n.getParameters.size > 3
      var i = n.getParameters.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
          if (lineBreaks) {
            printer.printLn()
            printer.print("    ")
          }
        }
      }
    }
    printer.print(")")
    if (!first) {         
      printer.print(" ")
      n.getBlock.accept(this, arg)
    }
  }

  def visit(n: MethodDeclaration, arg: Context) {
    arg.inObjectEquals = n.getName == "equals" && n.getParameters.size == 1
    printJavadoc(n.getJavaDoc, arg)
    var hasOverride = printMemberAnnotations(n.getAnnotations, arg)
    printMethodModifiers(n.getModifiers)
    if (hasOverride || isHashCode(n) || isEquals(n) || isToString(n)) {
      printer.print("override ")
    }
    printer.print("def ")
    visitName(n.getName)
    printTypeParameters(n.getTypeParameters, arg)
    printer.print("(")
    if (n.getParameters != null) {
      var lineBreaks = n.getParameters.size > 3
      var i = n.getParameters.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
          if (lineBreaks) {
            printer.printLn()
            printer.print("    ")
          }
        }
      }
    }
    printer.print(")")
    if (!(n.getType.isInstanceOf[VoidType]) || n.getBody == null) {
      printer.print(": ")
      n.getType.accept(this, arg)
    }
    if (n.getBody != null) {
      if (!(n.getType.isInstanceOf[VoidType])) {
        printer.print(" = ")
        if (n.getBody.getStmts.size == 1) {
          val str = print(n.getBody.getStmts.get(0), arg)
          if (str.length < 40) {
            printer.print(str)
          } else {
            n.getBody.accept(this, arg)
          }
        } else {
          n.getBody.accept(this, arg)
        }
      } else {
        printer.print(" ")
        n.getBody.accept(this, arg)
      }      
    }
  }
  
  def visit(n: Parameter, arg: Context) {
    printAnnotations(n.getAnnotations, arg)
    printModifiers(n.getModifiers)
    if (n.getModifiers.isProperty) {
       printer.print(if (n.getModifiers.isFinal) "val " else "var ")
    }   
    n.getId.accept(this, arg)
    printer.print(": ")
    n.getType.accept(this, arg)
    if (n.isVarArgs) {
      printer.print("*")
    }
  }

  def visit(n: ExplicitConstructorInvocationStmt, arg: Context) {
    if (n.isThis) {
      printTypeArgs(n.getTypeArgs, arg)
      printer.print("this")
    } else {
      if (n.getExpr != null) {
        n.getExpr.accept(this, arg)
        printer.print(".")
      }
      printTypeArgs(n.getTypeArgs, arg)
      printer.print("super")
    }
    printArguments(n.getArgs, arg)
  }

  def visit(n: VariableDeclarationExpr, arg: Context) {
    var modifier = if (ModifierSet.isFinal(n.getModifiers)) "val " else "var "
    var i = n.getVars.iterator()
    while (i.hasNext) {
      var v = i.next()
      printAnnotations(n.getAnnotations, arg)
      printer.print(modifier)
      if (v.getInit == null || v.getInit.isInstanceOf[NullLiteralExpr]){
        v.getId.accept(this, arg)
        printer.print(": ")
        n.getType.accept(this, arg)
        printer.print(" = ")
        printer.print(if (v.getInit() == null) "_" else "null")
      } else {
        v.accept(this, arg)
      }
      if (i.hasNext) {
        printer.printLn()
      }
    }
  }

  def visit(n: TypeDeclarationStmt, arg: Context) {
    n.getTypeDeclaration.accept(this, arg)
  }

  def visit(n: AssertStmt, arg: Context) {
    printer.print("assert ")
    n.getCheck.accept(this, arg)
    if (n.getMessage != null) {
      printer.print(" : ")
      n.getMessage.accept(this, arg)
    }
  }

  def visit(n: BlockStmt, arg: Context) {
    printer.printLn("{")
    if (n.getStmts != null) {
      printer.indent()
      val s = n.getStmts.iterator()
      val returnOn = arg.returnOn
      def print(stmt: Statement) {
        stmt.accept(this,arg) 
        printer.printLn()
      }
      while (s.hasNext) {
        val stmt = s.next
        arg.returnOn = returnOn || s.hasNext
        stmt match {
          case b: BlockStmt => b.getStmts.foreach(print) 
          case _ => print(stmt)          
        }
      }
      arg.returnOn = returnOn
      printer.unindent()
    }
    printer.print("}")
  }
  
  def visit(n: LabeledStmt, arg: Context) {
    printer.print(n.getLabel)
    printer.print(": ")
    n.getStmt.accept(this, arg)
  }

  def visit(n: EmptyStmt, arg: Context) {
  }

  def visit(n: ExpressionStmt, arg: Context) {
    n.getExpression.accept(this, arg)
  }

  def visit(n: SwitchStmt, arg: Context) {
    val oldSkip = arg.skip
    arg.skip = false
    n.getSelector.accept(this, arg)
    printer.printLn(" match {")
    if (n.getEntries != null) {
      printer.indent()
      for (e <- n.getEntries) {
        e.accept(this, arg)
        if (!arg.skip) {
          printer.printLn()  
        }        
      }
      printer.unindent()
    }
    printer.print("}")
    arg.skip = oldSkip
  }

  def visit(n: SwitchEntryStmt, arg: Context) {
    if (arg.skip) {
      printer.print(" | ")
      n.getLabel.accept(this, arg)
    } else {
      printer.print("case ")
      if (n.getLabel != null) {        
        n.getLabel.accept(this, arg)
      } else {
        printer.print("_")
      }        
    }  
    arg.skip = n.getStmts == null
    if (n.getStmts != null) {
      printer.print(" => ") 
      if (n.getStmts.size == 1) {
        n.getStmts.get(0).accept(this, arg)
      } else {
        printer.printLn()
        printer.indent()
        for (s <- n.getStmts) {
          s.accept(this, arg)
          printer.printLn()
        }
        printer.unindent()  
      }      
    }
  }

  def visit(n: BreakStmt, arg: Context) {
    printer.print("//break")
//    if (n.getId != null) {
//      printer.print(" ")
//      printer.print(n.getId)
//    }
  }

  def visit(n: ReturnStmt, arg: Context) {
    if (n.getExpr != null) {
      if (arg.returnOn) {
        printer.print("return ")
      }
      n.getExpr.accept(this, arg)
    } else {
      printer.print("return")
    }
  }

  def visit(n: EnumDeclaration, arg: Context) {
    // Java syntax
    printJavadoc(n.getJavaDoc, arg)
    printMemberAnnotations(n.getAnnotations, arg)
    printModifiers(n.getModifiers)
    printer.print("enum ")
    printer.print(n.getName)
    if (n.getImplements != null) {
      printer.print(" implements ")
      var i = n.getImplements.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    printer.printLn(" {")
    printer.indent()
    if (n.getEntries != null) {
      printer.printLn()
      var i = n.getEntries.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    if (n.getMembers != null) {
      printer.printLn(";")
      printMembers(n.getMembers, arg)
    } else {
      if (n.getEntries != null) {
        printer.printLn()
      }
    }
    printer.unindent()
    printer.print("}")
  }

  def visit(n: EnumConstantDeclaration, arg: Context) {
    // Java syntax
    printJavadoc(n.getJavaDoc, arg)
    printMemberAnnotations(n.getAnnotations, arg)
    printer.print(n.getName)
    if (n.getArgs != null) {
      printArguments(n.getArgs, arg)
    }
    if (n.getClassBody != null) {
      printer.printLn(" {")
      printer.indent()
      printMembers(n.getClassBody, arg)
      printer.unindent()
      printer.printLn("}")
    }
  }

  def visit(n: EmptyMemberDeclaration, arg: Context) {
    printJavadoc(n.getJavaDoc, arg)
  }

  def visit(n: InitializerDeclaration, arg: Context) {
    if (n.getBlock.getStmts != null) {
      val i = n.getBlock.getStmts.iterator
      while (i.hasNext) {
        i.next.accept(this, arg)
        if (i.hasNext) {
          printer.printLn()
          printer.printLn()
        }
      }
    }
  }

  def visit(n: IfStmt, arg: Context) {
    printer.print("if (")
    n.getCondition.accept(this, arg)
    printer.print(") ")
    n.getThenStmt.accept(this, arg)
    if (n.getElseStmt != null) {
      printer.print(" else ")
      n.getElseStmt.accept(this, arg)
    }
  }

  def visit(n: WhileStmt, arg: Context) {
    printer.print("while (")
    n.getCondition.accept(this, arg)
    printer.print(") ")
    n.getBody.accept(this, arg)
  }

  def visit(n: ContinueStmt, arg: Context) {
    printer.print("//continue")
//    if (n.getId != null) {
//      printer.print(" ")
//      printer.print(n.getId)
//    }
//    printer.print(";")
  }

  def visit(n: DoStmt, arg: Context) {
    printer.print("do ")
    n.getBody.accept(this, arg)
    printer.print(" while (")
    n.getCondition.accept(this, arg)
    printer.print(");")
  }

  def visit(n: ForeachStmt, arg: Context) {
    printer.print("for (")
    n.getVariable.getVars.get(0).accept(this, arg)
    printer.print(" <- ")
    n.getIterable.accept(this, arg)
    
    var body = n.getBody
    while (isUnwrapped(body)) {
      extract(body) match {
        case fe: ForeachStmt => {
          printer.print("; ")
          fe.getVariable.getVars.get(0).accept(this, arg)
          printer.print(" <- ")
          fe.getIterable.accept(this, arg)
          body = fe.getBody
        }
        case ifStmt: IfStmt => {
          printer.print(" if ")
          ifStmt.getCondition.accept(this, arg)
          body = ifStmt.getThenStmt
        }
      }
    }
           
    printer.print(") ")
    body.accept(this, arg)
    
  }
  
  private def isUnwrapped(stmt: Statement): Boolean = extract(stmt) match {
    case foreach: ForeachStmt => true
    case ifStmt: IfStmt => ifStmt.getElseStmt() == null
    case _ => false
  }
      
  def visit(n: ForStmt, arg: Context) {
    // init
    if (n.getInit != null) {
      n.getInit.foreach { i =>
        i.accept(this, arg)
        printer.printLn()
      }
    }
    
    // comparison
    printer.print("while (")
    if (n.getCompare != null) {
      n.getCompare.accept(this, arg)
    } else {
      printer.print("true")
    }
    printer.print(") ")
    
    if (n.getUpdate != null && n.getBody.isInstanceOf[BlockStmt]) {
      // merge updates into block
      val block = n.getBody.asInstanceOf[BlockStmt]
      block.getStmts.addAll(n.getUpdate.map(new ExpressionStmt(_)))
      block.accept(this, arg)
      
    } else {
      if (n.getUpdate != null) {
        printer.print("{")
      }    
      n.getBody.accept(this, arg)
    
      // update
      if (n.getUpdate != null) {
        n.getUpdate.foreach { u => 
          u.accept(this, arg)
          printer.printLn()
        }
        printer.print("}")
      }  
    }    
  }

  def visit(n: ThrowStmt, arg: Context) {
    printer.print("throw ")
    n.getExpr.accept(this, arg)
  }

  def visit(n: SynchronizedStmt, arg: Context) {
    printer.print("synchronized (")
    n.getExpr.accept(this, arg)
    printer.print(") ")
    n.getBlock.accept(this, arg)
  }

  def visit(n: TryStmt, arg: Context) {
    printer.print("try ")
    n.getTryBlock.accept(this, arg)
    if (n.getCatchs != null) {
      printer.printLn(" catch {")
      printer.indent()
      for (c <- n.getCatchs) {
        c.accept(this, arg)
      }
      printer.unindent()
      printer.print("}")
    }
    if (n.getFinallyBlock != null) {
      printer.print(" finally ")
      n.getFinallyBlock.accept(this, arg)
    }
  }

  def visit(n: CatchClause, arg: Context) {
    printer.print("case ")
    n.getExcept.accept(this, arg)
    printer.print(" => ")
    if (n.getCatchBlock.getStmts != null) {
      if (n.getCatchBlock.getStmts.size == 1) {
        n.getCatchBlock.getStmts.get(0).accept(this, arg);
      } else {
        n.getCatchBlock.accept(this, arg)  
      }  
    }        
    printer.printLn()
  }

  def visit(n: AnnotationDeclaration, arg: Context) {
    // Java syntax
    printJavadoc(n.getJavaDoc, arg)
    printMemberAnnotations(n.getAnnotations, arg)
    printModifiers(n.getModifiers)
    printer.print("@interface ")
    printer.print(n.getName)
    printer.printLn(" {")
    printer.indent()
    if (n.getMembers != null) {
      printMembers(n.getMembers, arg)
    }
    printer.unindent()
    printer.print("}")
  }

  def visit(n: AnnotationMemberDeclaration, arg: Context) {
    // Java syntax
    printJavadoc(n.getJavaDoc, arg)
    printMemberAnnotations(n.getAnnotations, arg)
    printModifiers(n.getModifiers)
    n.getType.accept(this, arg)
    printer.print(" ")
    visitName(n.getName)
    printer.print("()")
    if (n.getDefaultValue != null) {
      printer.print(" default ")
      n.getDefaultValue.accept(this, arg)
    }
  }

  def visit(n: MarkerAnnotationExpr, arg: Context) {
    printer.print("@")
    n.getName.accept(this, arg)
  }

  def visit(n: SingleMemberAnnotationExpr, arg: Context) {
    printer.print("@")
    n.getName.accept(this, arg)
    printer.print("(")
    n.getMemberValue.accept(this, arg)
    printer.print(")")
  }

  def visit(n: NormalAnnotationExpr, arg: Context) {
    printer.print("@")
    n.getName.accept(this, arg)
    printer.print("(")
    if (n.getPairs != null) {
      var i = n.getPairs.iterator()
      while (i.hasNext) {
        i.next().accept(this, arg)
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    printer.print(")")
  }

  def visit(n: MemberValuePair, arg: Context) {
    visitName(n.getName)
    printer.print(" = ")
    n.getValue.accept(this, arg)
  }

  def visit(n: LineComment, arg: Context) {
    printer.print("//")
    printer.printLn(n.getContent)
  }

  def visit(n: BlockComment, arg: Context) {
    printer.print("/*")
    printer.print(n.getContent)
    printer.printLn("*/")
  }
}

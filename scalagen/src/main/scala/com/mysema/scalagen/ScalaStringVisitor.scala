/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mysema.scalagen

import com.github.javaparser.ast._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.visitor.{GenericVisitor, GenericVisitorAdapter, VoidVisitor}
import java.util.ArrayList
import java.util.HashMap
import java.util.Iterator
import java.util.List

import org.apache.commons.lang3.StringUtils
import com.mysema.scalagen.ast.BeginClosureExpr
object ScalaStringVisitor {
  private val NL_THRESHOLD = 100

  private val PARAMETRIZED = Set("Class","Comparable","Enum","Iterable")

  private val UTIL_PARAMETRIZED = Set("Collection","List","Set","Map")

  private val METHOD_REPLACEMENTS = Map("equals"->"==")

  private val SKIPPED_ANNOTATIONS = Set("Override","SuppressWarnings","Nullable")

  private val PRIMITIVES = Map("Boolean"->"Boolean","Byte"->"Byte","Character"->"Char","Double"->"Double",
      "Float"->"Float","Integer"->"Int","Long"->"Long","Short"->"Short")

  private val NO_ARGS_SHORT = Set("toString","asc","desc","hashCode","hasNext","keys","keySet","length","size","values")

  private val SHORT_FORM = Set("eq","equals","gt","lt","ne","query","until","!=")

  private val RESERVED = Set("def","match","object","type","val","var")

  private val JAVA_TYPES = Set("Iterable")

  private val DEFAULTS = Map(
      PrimitiveType.Primitive.Boolean -> "false",
      PrimitiveType.Primitive.Byte -> "0",
      PrimitiveType.Primitive.Char -> "0",
      PrimitiveType.Primitive.Double -> "0.0",
      PrimitiveType.Primitive.Float -> "0.0f",
      PrimitiveType.Primitive.Int -> "0",
      PrimitiveType.Primitive.Long -> "0l",
      PrimitiveType.Primitive.Short -> "0.0")
  case class Context(
    var arrayAccess: Boolean = false,
    var classOf: Boolean = false,
    var label: String = null,
    var skip: Boolean = false,
    var assignType: Type = null,
    var inObjectEquals: Boolean = false,
    var returnOn: Boolean = false,
    var typeArg: Boolean = false,
    var imports: Map[String, String] = Map[String, String](),
    var noUnwrap: Set[Any] = Set[Any]()
  )
}


/**
 * ScalaDumpVisitor is a serializing visitor for CompilationUnit instances
 *
 */
class ScalaStringVisitor(settings: ConversionSettings) extends GenericVisitor[String, ScalaStringVisitor.Context] with Helpers {
  import ScalaStringVisitor._
  private def stringify(node: Node, arg: Context): String = {
    val v = new ScalaStringVisitor(settings)
    node.accept(v, arg)
  }

  private def methodModifiersString(m: Int): String = {
    modifiersString(ModifierSet.removeModifier(m, ModifierSet.ABSTRACT))
  }

  private def modifiersString(m: Int): String = {
    val printer = new SourcePrinter()
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
    printer.source
  }

  private def membersString(members: List[BodyDeclaration], arg: Context): String = {
    val printer = new SourcePrinter()
    for (member <- members) {
      printer.printLn()
      printer.print(member.accept(this, arg))
      printer.printLn()
    }
    printer.source
  }

  private def memberAnnotationsString(annotations: List[AnnotationExpr], arg: Context): (String, Boolean) = {
    val printer = new SourcePrinter()
    var hasOverride = false
    if (annotations != null) {
      for (a <- annotations) {
        if (!SKIPPED_ANNOTATIONS.contains(a.getName.getName)) {
          printer.print(a.accept(this, arg))
          printer.printLn()
        } else {
          hasOverride |= a.getName.getName == "Override"
        }
      }
    }
    (printer.source, hasOverride)
  }

  private def annotationsString(annotations: List[AnnotationExpr], arg: Context): String = {
    val printer = new SourcePrinter()
    if (annotations != null) {
      for (a <- annotations; if !SKIPPED_ANNOTATIONS.contains(a.getName.getName)) {
        printer.print(a.accept(this, arg))
        printer.print(" ")
      }
    }
    printer.source
  }

  private def typeArgsString(args: List[Type], arg: Context): String = {
    val printer = new SourcePrinter()
    if (args != null && !args.isEmpty) {
      val typeArg = arg.typeArg
      arg.typeArg = true
      printer.print("[")
      var i = args.iterator()
      while (i.hasNext) {
        var t = i.next()
        printer.print(t.accept(this, arg))
        if (i.hasNext) {
          printer.print(", ")
        }
      }
      printer.print("]")
      arg.typeArg = typeArg
    }
    printer.source
  }

  private def typeParametersString(args: List[TypeParameter], arg: Context): String = {
    val printer = new SourcePrinter()
    if (args != null && !args.isEmpty) {
      printer.print("[")
      var i = args.iterator()
      while (i.hasNext) {
        var t = i.next()
        printer.print(t.accept(this, arg))
        if (i.hasNext) {
          printer.print(", ")
        }
      }
      printer.print("]")
    }
    printer.source
  }

  private def argumentsString(args: List[Expression], arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("(")
    if (args != null) {
      val i = args.iterator()
      while (i.hasNext) {
        i.next() match {
          case closure: BeginClosureExpr =>
            printer.print(closure.params + " => ")
          case e =>
            printer.print(e.accept(this, arg))
            if (i.hasNext) {
              printer.print(", ")
            }
        }
        
        if (i.hasNext && settings.splitLongLines && printer.lineLength > NL_THRESHOLD) {
          printer.printLn()
          printer.print("  ")
        }
      }
    }
    printer.print(")")
    printer.source
  }

  private def javadocString(javadoc: JavadocComment, arg: Context): String = {
    if (javadoc != null) {
      javadoc.accept(this, arg)
    } else {
      ""
    }
  }

  def visit(n: CompilationUnit, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getPackage != null) {
      printer.print(n.getPackage.accept(this, arg))
    }
    for (i <- n.getImports) {
      printer.print(i.accept(this, arg))
    }

    arg.imports = n.getImports
      .filter(i => !i.isAsterisk && !i.isStatic)
      .map(i => split(i.getName).swap).toMap

    printer.printLn("//remove if not needed")
    printer.printLn("import scala.collection.JavaConversions._")
    if (hasTryWithResources(n)) {
      printer.printLn("import resource._ //use scala-arm from http://jsuereth.com/scala-arm/")
    }
    printer.printLn()
    
    if (n.getPackage != null && !isEmpty(n.getPackage.getAnnotations)) {
      printer.print(memberAnnotationsString(n.getPackage.getAnnotations, arg)._1)
      printer.printLn("package object " + split(n.getPackage.getName)._2 + " {")
      printer.printLn("}")
      printer.printLn()
    }

    if (n.getTypes != null) {
      var i = n.getTypes.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
        printer.printLn()
        if (i.hasNext) {
          printer.printLn()
        }
      }
    }

    arg.imports = Map[String,String]()
    printer.source
  }
  
  private def hasTryWithResources(n: CompilationUnit): Boolean = {
    val hasResourcesVisitor = new GenericVisitorAdapter[java.lang.Boolean, Null]() {
      override def visit(n: TryStmt, arg: Null): java.lang.Boolean = {
        if (n.getResources.isEmpty) null
        else true
      }
    }
    Option(n.accept(hasResourcesVisitor, null)).map(_.booleanValue).getOrElse(false)
  }

  private def split(name: NameExpr): (String, String) = {
    val str = name.toString
    val separator = str.lastIndexOf('.')
    (str.substring(0,separator), str.substring(separator+1))
  }

  def visit(n: PackageDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("package ")
    if (!isEmpty(n.getAnnotations)) {
      printer.print(split(n.getName)._1)
    } else {
      printer.print(n.getName.accept(this, arg))
    }
    printer.printLn()
    printer.printLn()
    printer.source
  }

  def visit(n: NameExpr, arg: Context): String = {
    visitName(n.getName)
  }

  def visitName(name: String): String = {
    if (RESERVED.contains(name)) {
      "`" + name + "`"
    } else if (PRIMITIVES.contains(name)) {
      "java.lang." + name
    } else {
      name
    }
  }

  def visit(n: QualifiedNameExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getQualifier.accept(this, arg))
    printer.print(".")
    printer.print(visitName(n.getName))
    printer.source
  }

  def visit(n: ImportDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("import ")
    if (n.getName.getName.endsWith(".Array") && !n.isAsterisk) {
      val className = n.getName.getName
      val pkg = className.substring(0, className.lastIndexOf('.'))
      printer.print(pkg + ".{Array => _Array}")
    } else {
      printer.print(n.getName.accept(this, arg))
      if (n.isAsterisk) {
        printer.print("._")
      }
    }

    printer.printLn()
    printer.source
  }

  def visit(n: ClassOrInterfaceDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(javadocString(n.getJavaDoc, arg))
    printer.print(memberAnnotationsString(n.getAnnotations, arg)._1)
    printer.print(modifiersString(n.getModifiers))
    if (n.getModifiers.isObject) {
      printer.print("object ")
    } else if (n.isInterface) {
      printer.print("trait ")
    } else {
      printer.print("class ")
    }
    printer.print(n.getName)
    printer.print(typeParametersString(n.getTypeParameters, arg))
    var constr = getFirstConstructor(n.getMembers)
    if (constr != null) {
      n.setMembers(n.getMembers.filterNot(_ == constr))
    }
    var superInvocation: Option[ExplicitConstructorInvocationStmt] = None
    if (constr != null) {
      if (!isEmpty(constr.getParameters) || !constr.getModifiers.isPublic) {
        printer.print(printConstructor(constr, arg, true))
      }
      superInvocation = constr.getBlock.getStmts
          .collect({ case x: ExplicitConstructorInvocationStmt => x })
          .filter(!_.isThis).headOption
    }
    var superTypes = new ArrayList[ClassOrInterfaceType]()
    if (n.getExtends != null) {
      superTypes.addAll(n.getExtends)
    }
    if (n.getImplements != null) {
      superTypes.addAll(n.getImplements)
    }
    if (settings.splitLongLines && printer.lineLength > 75) {
      printer.printLn()
      printer.print("   ")
    }
    if (!superTypes.isEmpty) {
      printer.print(" extends ")
      var i = superTypes.iterator()
      printer.print(i.next().accept(this, arg))
      superInvocation.foreach { s => 
        constr.getBlock.remove(s)
        printer.print(argumentsString(s.getArgs, arg))
      }
      while (i.hasNext) {
        printer.print(" with ")
        printer.print(i.next().accept(this, arg))
      }
    }

    if (!isEmpty(n.getMembers)) {
      printer.printLn(" {")
      printer.indent()
      printer.print(membersString(n.getMembers, arg))
      printer.unindent()
      printer.print("}")
    }
    printer.source
  }

  private def getFirstConstructor(members: List[BodyDeclaration]): ConstructorDeclaration = {
    if (members == null) {
      return null
    }
    members.collectFirst({ case c: ConstructorDeclaration => c }).getOrElse(null)
  }

  def visit(n: EmptyTypeDeclaration, arg: Context): String = {
    javadocString(n.getJavaDoc, arg)
  }

  def visit(n: JavadocComment, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.printLn("/**")
    for (line <- StringUtils.split(n.getContent.trim, '\n')) {
      printer.printLn(" " + line.trim)
    }
    printer.printLn(" */")
    printer.source
  }

  def visit(n: ClassOrInterfaceType, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getScope != null) {
      printer.print(n.getScope.accept(this, arg))
      printer.print(".")
    } else if (!arg.classOf && !arg.typeArg && PRIMITIVES.contains(n.getName)) {
      // primitive types are favored for class literals and type arguments
      printer.print("java.lang.")
    } else if (JAVA_TYPES.contains(n.getName)) {
      printer.print("java.lang.")
    }
    if (n.getName == "Object") {
      printer.print(if (arg.inObjectEquals || arg.typeArg) "Any" else "AnyRef")
    } else if (n.getScope == null && n.getName == "Array") {
      // TODO : only if Array import is present
      printer.print("_Array")
//    } else if (PRIMITIVES.contains(n.getName) && (arg.classOf || arg.typeArg)) {
//      printer.print(PRIMITIVES(n.getName))
    } else {
      printer.print(n.getName)
    }
    if (isEmpty(n.getTypeArgs)) {
      if (PARAMETRIZED.contains(n.getName)) {
        printer.print("[_]")
      } else if (UTIL_PARAMETRIZED.contains(n.getName) && arg.imports.getOrElse(n.getName, "") == "java.util") {
        printer.print(if (n.getName == "Map") "[_,_]" else "[_]")
      }
    }
    printer.print(typeArgsString(n.getTypeArgs, arg))
    printer.source
  }

  def visit(n: TypeParameter, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getName)
    if (n.getTypeBound != null && n.getTypeBound.size() > 0) {
      printer.print(" <: ")
      var i = n.getTypeBound.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
        if (i.hasNext) {
          printer.print(" with ")
        }
      }
    }
    printer.source
  }

  def visit(n: PrimitiveType, arg: Context): String = {
    n.getType.name
  }

  def visit(n: ReferenceType, arg: Context): String = {
    val printer = new SourcePrinter()
    val typeArg = arg.typeArg
    for (i <- 0 until n.getArrayCount) {
      printer.print("Array[")
      arg.typeArg = true
    }
    printer.print(n.getType.accept(this, arg))
    arg.typeArg = typeArg
    for (i <- 0 until n.getArrayCount) {
      printer.print("]")
    }
    printer.source
  }

  def visit(n: WildcardType, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("_")
    if (n.getExtends != null) {
      printer.print(" <: ")
      printer.print(n.getExtends.accept(this, arg))
    }
    if (n.getSuper != null) {
      printer.print(" >: ")
      printer.print(n.getSuper.accept(this, arg))
    }
    printer.source
  }

  def visit(n: FieldDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    val oldType = arg.assignType
    arg.assignType = n.getType
    printer.print(javadocString(n.getJavaDoc, arg))
    val modifier = if (ModifierSet.isFinal(n.getModifiers)) "val " else "var "
    val i = n.getVariables.iterator()
    while (i.hasNext) {
      var v = i.next()
      printer.print(memberAnnotationsString(n.getAnnotations, arg)._1)
      printer.print(modifiersString(n.getModifiers))
      printer.print(modifier)
      printer.print(v.getId.accept(this, arg))
      if (v.getInit == null || modifier != "val ") {
        if (v.getId.getName.endsWith("_")) {
          printer.print(" ")
        }
        printer.print(": ")
        printer.print(n.getType.accept(this, arg))
      }
      if (v.getInit == null) {
        printer.print(" = _")
      } else {
        printer.print(" = ")
        printer.print(v.getInit.accept(this, arg))
      }

      if (i.hasNext) {
        printer.printLn()
        printer.printLn()
      }
    }
    arg.assignType = oldType
    printer.source
  }

  def visit(n: VariableDeclarator, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getId.accept(this, arg))
    if (n.getInit != null) {
      printer.print(" = ")
      printer.print(n.getInit.accept(this, arg))
    }
    printer.source
  }

  def visit(n: VariableDeclaratorId, arg: Context): String = {
    visitName(n.getName)
//    for (i <- 0 until n.getArrayCount) {
//      printer.print("[]")
//    }
  }

  def visit(n: ArrayInitializerExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("Array(")
    if (n.getValues != null) {
      var i = n.getValues.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    printer.print(")")
    printer.source
  }

  def visit(n: VoidType, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("Unit")
    printer.source
  }

  def visit(n: ArrayAccessExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    val arrayAccess = arg.arrayAccess
    arg.arrayAccess = true
    printer.print(n.getName.accept(this, arg))
    arg.arrayAccess = arrayAccess
    printer.print("(")
    printer.print(n.getIndex.accept(this, arg))
    printer.print(")")
    printer.source
  }

  def visit(n: ArrayCreationExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getDimensions != null && !n.getDimensions.isEmpty) {

      if (arg.assignType != null) {
        printer.print("new ")
        printer.print(arg.assignType.accept(this, arg))
      } else {
        val max = n.getArrayCount + 1
        val dimString = (0 until max).map { _ =>
          val typeArg = arg.typeArg
          arg.typeArg = true
          val str = n.getType.accept(this, arg)
          arg.typeArg = typeArg
          str
        }.mkString(",")
        printer.print(s"Array.ofDim[$dimString]")
      }

      printer.print(n.getDimensions.map(stringify(_, arg)).mkString("(", ", ", ")"))
    } else {
      printer.print(n.getInitializer.accept(this, arg))
    }
    printer.source
  }

  def visit(n: AssignExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getTarget.accept(this, arg))
    printer.print(" ")
    import AssignExpr.{ Operator => Op }
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
    printer.print(n.getValue.accept(this, arg))
    printer.source
  }

  def visit(n: BinaryExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getLeft.accept(this, arg))
    printer.print(" ")
    import BinaryExpr.{ Operator => Op }
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
    if (settings.splitLongLines && (stringify(n.getLeft, arg).length > 50 || stringify(n.getRight, arg).length > 50)) {
      printer.printLn()
      printer.print("  ")
    }
    printer.print(n.getRight.accept(this, arg))
    printer.source
  }

  def visit(n: CastExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getExpr.accept(this, arg))
    if (n.getType.isInstanceOf[PrimitiveType]) {
      printer.print(".to")
      printer.print(n.getType.accept(this, arg))
    } else {
      printer.print(".asInstanceOf[")
      printer.print(n.getType.accept(this, arg))
      printer.print("]")
    }
    printer.source
  }

  def visit(n: ClassExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("classOf[")
    arg.classOf = true
    printer.print(n.getType.accept(this, arg))
    arg.classOf = false
    printer.print("]")
    printer.source
  }

  def visit(n: ConditionalExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("if (")
    printer.print(n.getCondition.accept(this, arg))
    printer.print(") ")
    printer.print(n.getThenExpr.accept(this, arg))
    printer.print(" else ")
    printer.print(n.getElseExpr.accept(this, arg))
    printer.source
  }

  def visit(n: EnclosedExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getInner.isInstanceOf[CastExpr]) {
      printer.print(n.getInner.accept(this, arg))
    } else {
      printer.print("(")
      printer.print(n.getInner.accept(this, arg))
      printer.print(")")
    }
    printer.source
  }

  def visit(n: FieldAccessExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getScope.accept(this, arg))
    printer.print(".")
    printer.print(visitName(n.getField))
    printer.source
  }

  def visit(n: InstanceOfExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getExpr.accept(this, arg))
    printer.print(".isInstanceOf[")
    printer.print(n.getType.accept(this, arg))
    printer.print("]")
    printer.source
  }

  def visit(n: CharLiteralExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("'")
    printer.print(n.getValue)
    printer.print("'")
    printer.source
  }

  def visit(n: DoubleLiteralExpr, arg: Context): String = {
    removeUnderscores(n.getValue)
  }

  def visit(n: IntegerLiteralExpr, arg: Context): String = {
    numberValue(n.getValue, "Integer.parseInt")
  }

  def visit(n: LongLiteralExpr, arg: Context): String = {
    numberValue(n.getValue, "java.lang.Long.parseLong")
  }
  
  private def removeUnderscores(n: String) = n.replaceAllLiterally("_", "")
  
  private def numberValue(n: String, parseMethod: String) = {
    var number = removeUnderscores(n)
    if (number.startsWith("0b") || number.startsWith("0B")) {
      number = number.drop(2)
      if (number.endsWith("L") || number.endsWith("l")) {
        number = number.dropRight(1)
      }
      parseMethod + "(\"" + number + "\", 2)" 
    } else {
      number
    }
  }

  def visit(n: IntegerLiteralMinValueExpr, arg: Context): String = {
    n.getValue
  }

  def visit(n: LongLiteralMinValueExpr, arg: Context): String = {
    n.getValue
  }

  def visit(n: StringLiteralExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("\"")
    printer.print(n.getValue)
    printer.print("\"")
    printer.source
  }

  def visit(n: BooleanLiteralExpr, arg: Context): String = {
    String.valueOf(n.getValue)
  }

  def visit(n: NullLiteralExpr, arg: Context): String = {
    "null"
  }

  def visit(n: ThisExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getClassExpr != null) {
      printer.print(n.getClassExpr.accept(this, arg))
      printer.print(".")
    }
    printer.print("this")
    printer.source
  }

  def visit(n: SuperExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getClassExpr != null) {
      printer.print(n.getClassExpr.accept(this, arg))
      printer.print(".")
    }
    printer.print("super")
    printer.source
  }

    //val split = arg.split
  def visit(n: MethodCallExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    var args = if (n.getArgs == null) 0 else n.getArgs.size
    val shortForm = SHORT_FORM.contains(n.getName) && args < 2 && !n.getArgs.get(0).isInstanceOf[LiteralExpr] || NO_ARGS_SHORT.contains(n.getName) && args == 0
    if (n.getScope != null) {
      val split = settings.splitLongLines && stringify(n.getScope, arg).length > 50
      printer.print(n.getScope.accept(this, arg))
      if (split) {
        printer.printLn()
        printer.print("  ")
      }
      printer.print(if ((shortForm && args == 1)) " " else ".")
    }
    if (METHOD_REPLACEMENTS.contains(n.getName)) {
      printer.print(METHOD_REPLACEMENTS(n.getName))
    } else {
      printer.print(visitName(n.getName))
    }
    printer.print(typeArgsString(n.getTypeArgs, arg))
    if (n.getName == "asList" && n.getScope != null && n.getScope.toString == "Arrays" && args == 1) {
      // assume Arrays.asList is called with an array argument
      printer.print("(")
      printer.print(n.getArgs().get(0).accept(this, arg))
      printer.print(":_*)")
    } else if (arg.arrayAccess) {
      printer.print(argumentsString(n.getArgs, arg))
    } else if (shortForm) {
      if (args == 1) {
        printer.print(" ")
        printer.print(n.getArgs.get(0).accept(this, arg))
      }
    } else if (!(n.getName.startsWith("get") || n.getName.startsWith("is")) || args > 0) {
      printer.print(argumentsString(n.getArgs, arg))
    }
    //arg.split = split
    printer.source
  }

  def visit(n: ObjectCreationExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getScope != null) {
      printer.print(n.getScope.accept(this, arg))
      printer.print(".")
    }
    printer.print("new ")
    printer.print(typeArgsString(n.getTypeArgs, arg))
    printer.print(n.getType.accept(this, arg))
    printer.print(argumentsString(n.getArgs, arg))
    if (n.getAnonymousClassBody != null) {
      printer.printLn(" {")
      printer.indent()
      printer.print(membersString(n.getAnonymousClassBody, arg))
      printer.unindent()
      printer.print("}")
    }
    printer.source
  }

  def visit(n: UnaryExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    import UnaryExpr.{ Operator => Op }
    if (n.getOperator == Op.not && n.getExpr.isInstanceOf[MethodCallExpr] && n.getExpr.asInstanceOf[MethodCallExpr].getName == "equals") {
      val method = n.getExpr.asInstanceOf[MethodCallExpr]
      printer.print(new MethodCallExpr(method.getScope, "!=", method.getArgs).accept(this, arg))
      return printer.source
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
    if (n.getOperator == Op.posIncrement || n.getOperator == Op.posDecrement) {
      printer.print("{")
    }
    printer.print(n.getExpr.accept(this, arg))
    printer.print(n.getOperator match {
      case Op.posIncrement => " += 1"
      case Op.posDecrement => " -= 1"
      case _ => ""
    })
    if (n.getOperator == Op.posIncrement || n.getOperator == Op.posDecrement) {
      printer.print("; ")
      printer.print(n.getExpr.accept(this, arg))
      printer.print(n.getOperator match {
        case Op.posIncrement => " - 1"
        case Op.posDecrement => " + 1"
        case _ => ""
      })
      printer.print("}")
    }
    printer.source
  }
  def visit(n: ConstructorDeclaration, arg: Context): String = {
    printConstructor(n, arg, false)
  }
  private def printConstructor(n: ConstructorDeclaration, arg: Context, first: Boolean): String = {
    val printer = new SourcePrinter()
    if (!first) {
      printer.print(javadocString(n.getJavaDoc, arg))
    }
    val (annotationString, _) = memberAnnotationsString(n.getAnnotations, arg)
    printer.print(annotationString)
    if (first && (n.getModifiers.isPrivate || n.getModifiers.isProtected)) {
      printer.print(" ")
    }
    printer.print(modifiersString(n.getModifiers))
    if (!first) {
      printer.print("def this")
      printer.print(typeParametersString(n.getTypeParameters, arg))
    }
    printer.print("(")
    if (n.getParameters != null) {
      val lineBreaks = settings.splitLongLines && n.getParameters.size > 3
      val i = n.getParameters.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
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
      printer.print(n.getBlock.accept(this, arg))
    }
    printer.source
  }
  def visit(n: MethodDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    arg.inObjectEquals = n.getName == "equals" && n.getParameters.size == 1
    printer.print(javadocString(n.getJavaDoc, arg))
    var (annotationSource, hasOverride) = memberAnnotationsString(n.getAnnotations, arg)
    printer.print(annotationSource)
    printer.print(methodModifiersString(n.getModifiers))
    if (hasOverride || isHashCode(n) || isEquals(n) || isToString(n)) {
      printer.print("override ")
    }
    printer.print("def ")
    printer.print(visitName(n.getName))
    printer.print(typeParametersString(n.getTypeParameters, arg))
    printer.print("(")
    if (n.getParameters != null) {
      val lineBreaks = settings.splitLongLines && n.getParameters.size > 3
      val i = n.getParameters.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
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
    if (!n.getType.isInstanceOf[VoidType] || n.getBody == null) {
      printer.print(": ")
      printer.print(n.getType.accept(this, arg))
    }
    if (n.getBody != null) {
      if (!n.getType.isInstanceOf[VoidType]) {
        printer.print(" = ")
        if (n.getBody.getStmts.size == 1 && printer.lineLength < NL_THRESHOLD) {
          val str = stringify(n.getBody.getStmts.get(0), arg)
          if (str.length < 40) {
            printer.print(str)
          } else {
            printer.print(n.getBody.accept(this, arg))
          }
        } else {
          printer.print(n.getBody.accept(this, arg))
        }
      } else {
        printer.print(" ")
        val origUnwrap = arg.noUnwrap
        arg.noUnwrap = arg.noUnwrap.+(n.getBody)
        printer.print(n.getBody.accept(this, arg))
        arg.noUnwrap = origUnwrap
      }
    }
    printer.source
  }

  def visit(n: Parameter, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(annotationsString(n.getAnnotations, arg))
    printer.print(modifiersString(n.getModifiers))
    if (n.getModifiers.isProperty) {
      printer.print(if (n.getModifiers.isFinal) "val " else "var ")
    }
    printer.print(n.getId.accept(this, arg))
    printer.print(": ")
    for (i <- 0 until n.getId.getArrayCount) {
      printer.print("Array[")
    }
    printer.print(n.getType.accept(this, arg))
    for (i <- 0 until n.getId.getArrayCount) {
      printer.print("]")
    }
    if (n.isVarArgs) {
      printer.print("*")
    }
    printer.source
  }

  def visit(n: MultiTypeParameter, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(annotationsString(n.getAnnotations, arg))
    printer.print(modifiersString(n.getModifiers))
    if (n.getModifiers.isProperty) {
      printer.print(if (n.getModifiers.isFinal) "val " else "var ")
    }

    printer.print(n.getId.accept(this, arg))
    n.getType.getElements.toList match {
      case tpe :: Nil =>
        printer.print(": ")
        printer.print(tpe.accept(this, arg))
      case types =>
        printer.print(" @ (")
        for ((tpe, i) <- types.zipWithIndex) {
          val last = i == types.length - 1
          printer.print("_: ")
          printer.print(tpe.accept(this, arg))
          if (!last) {
            printer.print(" | ")
          }
        }
        printer.print(")")
    }
    printer.source
  }

  def visit(n: ExplicitConstructorInvocationStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.isThis) {
      printer.print(typeArgsString(n.getTypeArgs, arg))
      printer.print("this")
    } else {
      if (n.getExpr != null) {
        printer.print(n.getExpr.accept(this, arg))
        printer.print(".")
      }
      printer.print(typeArgsString(n.getTypeArgs, arg))
      printer.print("super")
    }
    printer.print(argumentsString(n.getArgs, arg))
    printer.source
  }
  def isTypeInitMatch(n: VariableDeclarationExpr, v: VariableDeclarator) = {
    import PrimitiveType.Primitive
    val init = v.getInit
    if (init.isInstanceOf[LiteralExpr]) {
      n.getType match {
        case ptype: PrimitiveType =>
          ptype.getType match {
            case Primitive.Boolean => init.isInstanceOf[BooleanLiteralExpr]
            case Primitive.Byte => false
            case Primitive.Char => init.isInstanceOf[CharLiteralExpr]
            case Primitive.Double => init.isInstanceOf[DoubleLiteralExpr]
            case Primitive.Float => false
            case Primitive.Int => init.isInstanceOf[IntegerLiteralExpr]
            case Primitive.Long => init.isInstanceOf[LongLiteralExpr]
            case Primitive.Short => false
          }
        case _ =>
          true
      }
    } else {
      true

    }
  }
  def visit(n: VariableDeclarationExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    val asParameter = n.getModifiers == -1
    var modifier = if (ModifierSet.isFinal(n.getModifiers)) "val " else "var "
    var i = n.getVars.iterator()
    while (i.hasNext) {
      var v = i.next()
      printer.print(annotationsString(n.getAnnotations, arg))
      if (!asParameter) {
        printer.print(modifier)
      }
      if (v.getInit == null || v.getInit.isInstanceOf[NullLiteralExpr] || !isTypeInitMatch(n, v)) {
        printer.print(v.getId.accept(this, arg))
        printer.print(": ")
        for (i <- 0 until v.getId.getArrayCount) {
          printer.print("Array[")
        }
        printer.print(n.getType.accept(this, arg))
        for (i <- 0 until v.getId.getArrayCount) {
          printer.print("]")
        }
        if (!asParameter) {
          printer.print(" = ")
          if (n.getType.isInstanceOf[PrimitiveType]) {
            if (v.getInit != null) {
              printer.print(v.getInit.accept(this, arg))
            } else {
              val ptype = n.getType.asInstanceOf[PrimitiveType]
              printer.print(DEFAULTS(ptype.getType))
            }
          } else {
            printer.print("null")
          }
          //printer.print(if (v.getInit() == null) "_" else "null")
        }
      } else {
        v.getInit match {
          case newObj: ObjectCreationExpr =>
            if (newObj.getType() != null && (newObj.getType.getTypeArgs() == null || newObj.getType.getTypeArgs.isEmpty)) {
              n.getType match {
                case ref: ReferenceType =>
                  ref.getType match {
                    case tpe: ClassOrInterfaceType => newObj.getType.setTypeArgs(tpe.getTypeArgs())
                    case _ =>
                  }
                case _ =>
              }
            }
          case _ =>
        }
        printer.print(v.accept(this, arg))
      }
      if (i.hasNext) {
        printer.printLn()
      }
    }
    printer.source
  }

  def visit(n: TypeDeclarationStmt, arg: Context): String = {
    n.getTypeDeclaration.accept(this, arg)
  }

  def visit(n: AssertStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("assert(")
    printer.print(n.getCheck.accept(this, arg))
    printer.print(")")
    if (n.getMessage != null) {
      printer.print(" : ")
      printer.print(n.getMessage.accept(this, arg))
    }
    printer.source
  }

  def visit(n: BlockStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    if (!isEmpty(n.getStmts) && !arg.noUnwrap.contains(n) && n.getStmts.size == 1 && n.getStmts.get(0).isInstanceOf[SwitchStmt]) {
      return n.getStmts.get(0).accept(this, arg)
    }
    printer.printLn("{")
    if (n.getStmts != null) {
      printer.indent()
      val s = n.getStmts.iterator()
      val returnOn = arg.returnOn
      def print(stmt: Statement): Unit = {
        printer.print(stmt.accept(this, arg))
        printer.printLn()
      }
      while (s.hasNext) {
        val stmt = s.next()
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
    printer.source
  }

  def visit(n: LabeledStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(n.getLabel)
    printer.print(": ")
    printer.print(n.getStmt.accept(this, arg))
    printer.source
  }

  def visit(n: EmptyStmt, arg: Context): String = {
    ""
  }

  def visit(n: ExpressionStmt, arg: Context): String = {
    n.getExpression.accept(this, arg)
  }

  def visit(n: SwitchStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    val oldSkip = arg.skip
    arg.skip = false
    printer.print(n.getSelector.accept(this, arg))
    printer.printLn(" match {")
    if (n.getEntries != null) {
      printer.indent()
      for (e <- n.getEntries) {
        printer.print(e.accept(this, arg))
        if (!arg.skip) {
          printer.printLn()
        }
      }
      printer.unindent()
    }
    printer.print("}")
    arg.skip = oldSkip
    printer.source
  }

  def visit(n: SwitchEntryStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    if (arg.skip) {
      printer.print(" | ")
      if (n.getLabel != null) {
        printer.print(n.getLabel.accept(this, arg))
      }
    } else {
      printer.print("case ")
      if (n.getLabel != null) {
        printer.print(n.getLabel.accept(this, arg))
      } else {
        printer.print("_")
      }
    }
    arg.skip = n.getStmts == null || n.getStmts.size() == 0
    if (!arg.skip) {
      printer.print(" => ")
      if (n.getStmts.size == 1) {
        printer.print(n.getStmts.get(0).accept(this, arg))
      } else {
        printer.printLn()
        printer.indent()
        for (s <- n.getStmts) {
          printer.print(s.accept(this, arg))
          printer.printLn()
        }
        printer.unindent()
      }
    }
    printer.source
  }

  def visit(n: BreakStmt, arg: Context): String = {
    "//break"
  }

  def visit(n: ReturnStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getExpr != null) {
      if (arg.returnOn) {
        printer.print("return ")
      }
      printer.print(n.getExpr.accept(this, arg))
    } else {
      printer.print("return")
    }
    printer.source
  }

  def visit(n: EnumDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(javadocString(n.getJavaDoc, arg))
    printer.print(memberAnnotationsString(n.getAnnotations, arg)._1)
    printer.print(modifiersString(n.getModifiers))
    printer.print("enum ")
    printer.print(n.getName)
    if (n.getImplements != null) {
      printer.print(" implements ")
      var i = n.getImplements.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
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
        printer.print(i.next().accept(this, arg))
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    if (n.getMembers != null) {
      printer.printLn(";")
      printer.print(membersString(n.getMembers, arg))
    } else {
      if (n.getEntries != null) {
        printer.printLn()
      }
    }
    printer.unindent()
    printer.print("}")
    printer.source
  }

  def visit(n: EnumConstantDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(javadocString(n.getJavaDoc, arg))
    printer.print(memberAnnotationsString(n.getAnnotations, arg)._1)
    printer.print(n.getName)
    if (n.getArgs != null) {
      printer.print(argumentsString(n.getArgs, arg))
    }
    if (n.getClassBody != null) {
      printer.printLn(" {")
      printer.indent()
      printer.print(membersString(n.getClassBody, arg))
      printer.unindent()
      printer.printLn("}")
    }
    printer.source
  }

  def visit(n: EmptyMemberDeclaration, arg: Context): String = {
    javadocString(n.getJavaDoc, arg)
  }

  def visit(n: InitializerDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getBlock.getStmts != null) {
      val i = n.getBlock.getStmts.iterator
      while (i.hasNext) {
        val stmt = i.next()
        if (!stmt.isInstanceOf[ExplicitConstructorInvocationStmt]) {
          printer.print(stmt.accept(this, arg))
          if (i.hasNext) {
            printer.printLn()
            printer.printLn()
          }
        }
      }
    }
    printer.source
  }

  def visit(n: IfStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("if (")
    printer.print(n.getCondition.accept(this, arg))
    printer.print(") ")
    printer.print(n.getThenStmt.accept(this, arg))
    if (n.getElseStmt != null) {
      printer.print(" else ")
      printer.print(n.getElseStmt.accept(this, arg))
    }
    printer.source
  }

  def visit(n: WhileStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("while (")
    printer.print(n.getCondition.accept(this, arg))
    printer.print(") ")
    printer.print(n.getBody.accept(this, arg))
    printer.source
  }

  def visit(n: ContinueStmt, arg: Context): String = {
    "//continue"
  }

  def visit(n: DoStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("do ")
    printer.print(n.getBody.accept(this, arg))
    printer.print(" while (")
    printer.print(n.getCondition.accept(this, arg))
    printer.print(");")
    printer.source
  }

  def visit(n: ForeachStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("for (")
    printer.print(n.getVariable.getVars.get(0).accept(this, arg))
    printer.print(" <- ")
    printer.print(n.getIterable.accept(this, arg))
    var body = n.getBody
    while (isUnwrapped(body)) {
      Types.extract(body) match {
        case fe: ForeachStmt =>
          printer.print("; ")
          if (settings.splitLongLines && printer.lineLength > NL_THRESHOLD) {
            printer.printLn()
            printer.print("     ")
          }
          printer.print(fe.getVariable.getVars.get(0).accept(this, arg))
          printer.print(" <- ")
          printer.print(fe.getIterable.accept(this, arg))
          body = fe.getBody
        case ifStmt: IfStmt =>
          if (settings.splitLongLines && printer.lineLength > NL_THRESHOLD) {
            printer.printLn()
            printer.print("   ")
          }
          printer.print(" if ")
          printer.print(ifStmt.getCondition.accept(this, arg))
          body = ifStmt.getThenStmt
      }
    }
    printer.print(") ")
    printer.print(body.accept(this, arg))
    printer.source
  }

  private def isUnwrapped(stmt: Statement): Boolean = Types.extract(stmt) match {
    case foreach: ForeachStmt => true
    case ifStmt: IfStmt => ifStmt.getElseStmt() == null
    case _ => false
  }

  def visit(n: ForStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getInit != null) {
      n.getInit.foreach { i => 
        printer.print(i.accept(this, arg))
        printer.printLn()
      }
    }

    // comparison
    printer.print("while (")
    if (n.getCompare != null) {
      printer.print(n.getCompare.accept(this, arg))
    } else {
      printer.print("true")
    }
    printer.print(") ")

    if (n.getUpdate != null && n.getBody.isInstanceOf[BlockStmt]) {
      // merge updates into block
      val block = n.getBody.asInstanceOf[BlockStmt]
      block.addAll(n.getUpdate.map(new ExpressionStmt(_)))

      printer.print(block.accept(this, arg))
    } else {
      if (n.getUpdate != null) {
        printer.print("{")
      }

      // update
      printer.print(n.getBody.accept(this, arg))
      if (n.getUpdate != null) {
        n.getUpdate.foreach { u => 
          printer.print(u.accept(this, arg))
          printer.printLn()
        }
        printer.print("}")
      }
    }
    printer.source
  }

  def visit(n: ThrowStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("throw ")
    printer.print(n.getExpr.accept(this, arg))
    printer.source
  }

  def visit(n: SynchronizedStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    if (n.getExpr != null) {
      printer.print("synchronized (")
      printer.print(n.getExpr.accept(this, arg))
      printer.print(") ")
    } else {
      printer.print("synchronized ")
    }
    printer.print(n.getBlock.accept(this, arg))
    printer.source
  }
  
  def visit(n: TryStmt, arg: Context): String = {
    val printer = new SourcePrinter()
    val wrapInTry = !isEmpty(n.getCatchs()) || n.getFinallyBlock() != null
    if (wrapInTry) {
      printer.print("try ")
    }
    def printResource(rd: VariableDeclarationExpr): Unit = {
      for (resource <- rd.getVars()) {
        printer.print(resource.getId.accept(this, arg))
        printer.print(" <- managed(")
        printer.print(resource.getInit.accept(this, arg))
        printer.print(")")
      }
    }

    if (!n.getResources.isEmpty) {
      if (wrapInTry) {
        printer.printLn("{")
        printer.indent()
      }
      printer.print("for ")
      if (n.getResources.size == 1) {
        printer.print("(")
        val rd = n.getResources.get(0)
        printResource(rd)
        printer.print(")")
      } else {
        printer.printLn("{")
        printer.indent()
        for (rd <- n.getResources) {
          printResource(rd)
          printer.printLn()
        }
        printer.unindent()
        printer.print("} ")
      }
      printer.print(n.getTryBlock.accept(this, arg))
      if (wrapInTry) {
        printer.printLn()
        printer.unindent()
        printer.print("}")
      }
    } else {
      printer.print(n.getTryBlock.accept(this, arg))
    }

    if (n.getCatchs != null && !n.getCatchs.isEmpty) {
      printer.printLn(" catch {")
      printer.indent()
      for (c <- n.getCatchs) {
        printer.print(c.accept(this, arg))
      }
      printer.unindent()
      printer.print("}")
    }
    if (n.getFinallyBlock != null) {
      printer.print(" finally ")
      printer.print(n.getFinallyBlock.accept(this, arg))
    }
    printer.source
  }

  def visit(n: CatchClause, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("case ")
    printer.print(n.getParam.accept(this, arg))
    printer.print(" => ")
    if (n.getCatchBlock.getStmts != null) {
      if (n.getCatchBlock.getStmts.size == 1) {
        printer.print(n.getCatchBlock.getStmts.get(0).accept(this, arg))
      } else {
        printer.print(n.getCatchBlock.accept(this, arg))
      }
    }
    printer.printLn()
    printer.source
  }

  def visit(n: AnnotationDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(javadocString(n.getJavaDoc, arg))
    printer.print(memberAnnotationsString(n.getAnnotations, arg)._1)
    printer.print(modifiersString(n.getModifiers))
    printer.print("@interface ")
    printer.print(n.getName)
    printer.printLn(" {")
    printer.indent()
    if (n.getMembers != null) {
      printer.print(membersString(n.getMembers, arg))
    }
    printer.unindent()
    printer.print("}")

    printer.source
  }
  def visit(n: AnnotationMemberDeclaration, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(javadocString(n.getJavaDoc, arg))
    printer.print(memberAnnotationsString(n.getAnnotations, arg)._1)
    printer.print(modifiersString(n.getModifiers))
    printer.print(visitName(n.getName))
    printer.print(": ")
    printer.print(n.getType.accept(this, arg))
    if (n.getDefaultValue != null) {
      printer.print("= ")
      printer.print(n.getDefaultValue.accept(this, arg))
    }
    printer.source
  }

  def visit(n: MarkerAnnotationExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("@")
    printer.print(n.getName.accept(this, arg))
    printer.source
  }

  def visit(n: SingleMemberAnnotationExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("@")
    printer.print(n.getName.accept(this, arg))
    printer.print("(")
    printer.print(n.getMemberValue.accept(this, arg))
    printer.print(")")
    printer.source
  }

  def visit(n: NormalAnnotationExpr, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("@")
    printer.print(n.getName.accept(this, arg))
    printer.print("(")
    if (n.getPairs != null) {
      var i = n.getPairs.iterator()
      while (i.hasNext) {
        printer.print(i.next().accept(this, arg))
        if (i.hasNext) {
          printer.print(", ")
        }
      }
    }
    printer.print(")")
    printer.source
  }

  def visit(n: MemberValuePair, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print(visitName(n.getName))
    printer.print(" = ")
    printer.print(n.getValue.accept(this, arg))
    printer.source
  }

  def visit(n: LineComment, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("//")
    printer.printLn(n.getContent)
    printer.source
  }

  def visit(n: BlockComment, arg: Context): String = {
    val printer = new SourcePrinter()
    printer.print("/*")
    printer.print(n.getContent)
    printer.printLn("*/")

    printer.source
  }
  def visit(x: TypeExpr, y: Context): String = ???
  def visit(x: MethodReferenceExpr, y: Context): String = ???
  def visit(x: LambdaExpr, y: Context): String = ???
  def visit(x: UnknownType, y: Context): String = ???
  def visit(x: UnionType, y: Context): String = ???
  def visit(x: IntersectionType, y: Context): String = ???
}
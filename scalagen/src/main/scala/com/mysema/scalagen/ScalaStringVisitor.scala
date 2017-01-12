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
import com.github.javaparser.ast.visitor.{GenericVisitor, GenericVisitorAdapter}
import java.util.List

import com.mysema.scalagen.Types.MaybeInBlock

import scala.collection.JavaConverters._
import org.apache.commons.lang3.StringUtils
import com.mysema.scalagen.ast.BeginClosureExpr
object ScalaStringVisitor {
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
    val arrayAccess: Boolean = false,
    val classOf: Boolean = false,
    var skip: Boolean = false,
    val assignType: Type = null,
    val inObjectEquals: Boolean = false,
    val returnOn: Boolean = false,
    val typeArg: Boolean = false,
    val imports: Map[String, String] = Map[String, String](),
    val mustWrap: Boolean = false
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
  private val modifierMatchings: Seq[(RichModifiers => Boolean, String)] = Seq[(RichModifiers => Boolean, String)](
    (_.isTransient, "@transient"),
    (_.isVolatile, "@volatile"),
    (_.isPrivate, "private"),
    (_.isProtected, "protected"),
    (_.isLazy, "lazy"),
    (_.isImplicit, "implicit"),
    (_.isAbstract, "abstract"),
    //(_.isStatic, ""),
    //(_.isFinal, ""),
    (_.isNative, "/* native */"),
    (_.isStrictfp, "/* strictfp */"),
    (_.isSynchronized, "/* synchronized */")
  )
  private def modifiersString(m: Int): String = {
    val modifiers: RichModifiers = new RichModifiers(m)
    val matchingModifiers = modifierMatchings.flatMap { case (predicate, string) => if (predicate(modifiers)) Some(string) else None }
    if (matchingModifiers.isEmpty) "" else matchingModifiers.mkString(" ") + " "
  }

  private def membersString(members: List[BodyDeclaration], arg: Context): String =
    members.map(member => s"\n${member.accept(this, arg)}\n").mkString

  private def memberAnnotationsString(annotations: List[AnnotationExpr], arg: Context): String = {
    Option(annotations.asScala).toList.flatten.foldLeft("") { case (str, a) =>
      str + (if (!SKIPPED_ANNOTATIONS.contains(a.getName.getName)) {
          a.accept(this, arg) + "\n"
        } else {
          ""
        })
    }
  }

  private def withMemberAnnotations(n: AnnotableNode, arg: Context)(restString: => String): String =
    memberAnnotationsString(n.getAnnotations, arg) + restString

  private def hasOverride(n: AnnotableNode) =
    Option(n.getAnnotations).toList.flatten.find(_.getName.getName == "Override").isDefined

  private def annotationsString(annotations: List[AnnotationExpr], arg: Context): String = {
    val matchingAnnotations = Option(annotations).toList.flatten.filter(
      a => !SKIPPED_ANNOTATIONS.contains(a.getName.getName)
    ).map(_.accept(this, arg))
    if (matchingAnnotations.isEmpty) "" else matchingAnnotations.mkString(" ") + " "
  }

  private def withAnnotations(n: AnnotableNode, arg: Context)(restText: => String) =
    annotationsString(n.getAnnotations, arg) + restText

  private def typeArgsString(args: List[Type], arg: Context): String = {
    if (args != null && !args.isEmpty) {
      args.map(_.accept(this, arg.copy(typeArg = true))).mkString("[", ", ", "]")
    } else {
      ""
    }
  }

  private def typeParametersString(args: List[TypeParameter], arg: Context): String = {
    if (args != null && !args.isEmpty) {
      args.map(_.accept(this, arg)).mkString("[", ", ", "]")
    } else {
      ""
    }
  }

  private def argumentsString(args: List[Expression], arg: Context): String = {
    Option(args.asScala).toList.flatten.map { e =>
      e match {
        case closure: BeginClosureExpr => closure.params + " => "
        case e => e.accept(this, arg)
      }
    }.mkString("(", ", ", ")").replaceAll("=> , ", "=> ")
  }

  private def javadocString(javadoc: JavadocComment, arg: Context): String =
    Option(javadoc).map(_.accept(this, arg)).getOrElse("")

  private def withJavaDoc(n: DocumentableNode, arg: Context)(restText: => String = ""): String = {
    javadocString(n.getJavaDoc, arg) + restText
  }

  private def withComments(n: Node, arg: Context)(restText: => String = ""): String = {
    val commentBefore = Option(n.getComment).map(_.accept(this, arg))
    val commentsAfter = n.getOrphanComments.map(_.accept(this, arg))
    (commentBefore.toSeq ++ Seq(restText) ++ commentsAfter).mkString("\n")
  }

  def visit(n: CompilationUnit, arg: Context): String = withComments(n, arg) {
    val packageString = Option(n.getPackage).map(_.accept(this, arg)).getOrElse("")
    val importsString = (n.getImports.map(_.accept(this, arg)) ++ Seq(
      "//remove if not needed",
      "import scala.collection.JavaConversions._"
    ) ++ (if (hasTryWithResources(n)) {
      Seq("import resource._ //use scala-arm from http://jsuereth.com/scala-arm/")
    } else
      Seq()
      )
      ).mkString("\n")
    val argWithFilteredImport = arg.copy(imports = n.getImports
      .filter(i => !i.isAsterisk && !i.isStatic)
      .map(i => split(i.getName).swap).toMap)


    val pkgAnnotationObjectString = if (n.getPackage != null && !isEmpty(n.getPackage.getAnnotations)) {
      memberAnnotationsString(n.getPackage.getAnnotations, arg) +
      s"package object ${split(n.getPackage.getName)._2} {\n}\n"
    } else ""

    val typesString = Option(n.getTypes).map(_.map(_.accept(this, argWithFilteredImport)).mkString("\n\n")).getOrElse("")

    s"""$packageString
       |
       |$importsString
       |
       |$pkgAnnotationObjectString
       |
       |$typesString
     """.stripMargin
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

  def visit(n: PackageDeclaration, arg: Context): String =
    s"package ${if (!isEmpty(n.getAnnotations)) split(n.getName)._1 else n.getName.accept(this, arg)}\n\n"

  def visit(n: NameExpr, arg: Context): String = visitName(n.getName)

  def visitName(name: String): String =
    if (RESERVED.contains(name)) {
      "`" + name + "`"
    } else if (PRIMITIVES.contains(name)) {
      "java.lang." + name
    } else {
      name
    }

  def visit(n: QualifiedNameExpr, arg: Context): String =
    s"${n.getQualifier.accept(this, arg)}.${visitName(n.getName)}"

  def visit(n: ImportDeclaration, arg: Context): String = withComments(n, arg) {
    val toImport = if (n.getName.getName.endsWith(".Array") && !n.isAsterisk) {
      val className = n.getName.getName
      val pkg = className.substring(0, className.lastIndexOf('.'))
      s"$pkg.{Array => _Array}"
    } else {
      n.getName.accept(this, arg) + (if (n.isAsterisk) "._" else "")
    }

    s"import $toImport\n"
  }

  def visit(n: ClassOrInterfaceDeclaration, arg: Context): String =
    withJavaDoc(n, arg) {
      withMemberAnnotations(n, arg) {
        val objectType = if (n.getModifiers.isObject) {
          "object"
        } else if (n.isInterface) {
          "trait"
        } else {
          "class"
        }
        val constructorOption = getFirstConstructor(n.getMembers)
        constructorOption.foreach { c => n.setMembers(n.getMembers.filterNot(_ == c)) }
        val superInvocation: Option[ExplicitConstructorInvocationStmt] = constructorOption.flatMap { cons =>
          cons.getBlock.getStmts
            .collect({ case x: ExplicitConstructorInvocationStmt => x })
            .filter(!_.isThis).headOption
        }
        val superTypes = Seq(
          Option(n.getExtends.asScala),
          Option(n.getImplements.asScala)
        ).flatten.flatten.toList
        val constructorString = (for {
          cons <- constructorOption if (!isEmpty(cons.getParameters) || !cons.getModifiers.isPublic)
        } yield printConstructor(cons, arg, true)
          ).getOrElse("")
        val superTypesString = if (!superTypes.isEmpty) {
          superInvocation.foreach { s =>
            constructorOption.get.getBlock.remove(s)
          }
          s" extends ${superTypes.head.accept(this, arg)}${superInvocation.map(s => argumentsString(s.getArgs, arg)).getOrElse("")}" +
            ("" :: superTypes.tail.map(_.accept(this, arg))).mkString(" with ")
        } else ""
        val declaredTypeString = s"${modifiersString(n.getModifiers)}$objectType ${n.getName}${typeParametersString(n.getTypeParameters, arg)}$constructorString"
        val spacingAfterTypeString = if (settings.splitLongLines && declaredTypeString.length > 75) "\n   " else ""
        val bodyString = if (!isEmpty(n.getMembers)) {
          s" {\n${membersString(n.getMembers, arg)}\n}"
        } else ""
        declaredTypeString +
          spacingAfterTypeString +
          superTypesString +
          bodyString
      }
    }

  private def getFirstConstructor(members: List[BodyDeclaration]): Option[ConstructorDeclaration] = {
    if (members == null) {
      return null
    }
    members.collectFirst({ case c: ConstructorDeclaration => c })
  }

  def visit(n: EmptyTypeDeclaration, arg: Context): String = javadocString(n.getJavaDoc, arg)

  def visit(n: JavadocComment, arg: Context): String = withComments(n, arg) {
    val comment = StringUtils.split(n.getContent.trim, '\n').map(" " + _.trim).mkString("\n")
    s"/**\n$comment\n */"
  }

  def visit(n: ClassOrInterfaceType, arg: Context): String = withComments(n, arg) {
    val scopeString = if (n.getScope != null) {
      n.getScope.accept(this, arg) + "."
    } else if (!arg.classOf && !arg.typeArg && PRIMITIVES.contains(n.getName)) {
      // primitive types are favored for class literals and type arguments
      "java.lang."
    } else if (JAVA_TYPES.contains(n.getName)) {
      "java.lang."
    } else ""
    val scalaTypeString = if (n.getName == "Object") {
      if (arg.inObjectEquals || arg.typeArg) "Any" else "AnyRef"
    } else if (n.getScope == null && n.getName == "Array") {
      // TODO : only if Array import is present
      "_Array"
//    } else if (PRIMITIVES.contains(n.getName) && (arg.classOf || arg.typeArg)) {
//      printer.print(PRIMITIVES(n.getName))
    } else {
      n.getName
    }
    val typeArgString = if (isEmpty(n.getTypeArgs)) {
      if (PARAMETRIZED.contains(n.getName)) {
        "[_]"
      } else if (UTIL_PARAMETRIZED.contains(n.getName) && arg.imports.getOrElse(n.getName, "") == "java.util") {
        if (n.getName == "Map") "[_,_]" else "[_]"
      } else ""
    } else ""
    Seq(
      scopeString,
      scalaTypeString,
      typeArgString,
      typeArgsString(n.getTypeArgs, arg)
    ).mkString
  }

  def visit(n: TypeParameter, arg: Context): String = withComments(n, arg) {
    n.getName + (if (n.getTypeBound != null && n.getTypeBound.size() > 0) {
      " <: " + n.getTypeBound.map(_.accept(this, arg)).mkString(" with ")
    } else "")
  }

  def visit(n: PrimitiveType, arg: Context): String = n.getType.name

  def visit(n: ReferenceType, arg: Context): String = withComments(n, arg) {
    val adaptedArg = if(n.getArrayCount > 0) arg.copy(typeArg = true) else arg
    val prefix = "Array[" * n.getArrayCount
    val postFix = "]" * n.getArrayCount
    s"$prefix${n.getType.accept(this, adaptedArg)}$postFix"
  }

  def visit(n: WildcardType, arg: Context): String = withComments(n, arg) {
    val maybeExtends = Option(n.getExtends).map { ext =>
      s"<: ${ext.accept(this, arg)}"
    }
    val maybeSuper = Option(n.getSuper).map { sup =>
      s">: ${sup.accept(this, arg)}"
    }
    Seq(Some("_"), maybeExtends, maybeSuper).flatten.mkString(" ")
  }

  def visit(n: FieldDeclaration, arg: Context): String = {
    val argWithType = arg.copy(assignType = n.getType)
    withJavaDoc(n, argWithType) {
      withComments(n, arg) {
        val modifier = if (ModifierSet.isFinal(n.getModifiers)) "val " else "var "
        val variablesString = n.getVariables.map { v =>
          val typeString = if (true) {
            (if (v.getId.getName.endsWith("_")) " " else "") +
              ": " + n.getType.accept(this, argWithType)
          } else ""
          val initializerString = if (v.getInit == null) {
            " = _"
          } else {
            " = " + v.getInit.accept(this, argWithType)
          }
          memberAnnotationsString(n.getAnnotations, argWithType) +
            modifiersString(n.getModifiers) +
            modifier +
            v.getId.accept(this, argWithType) + typeString + initializerString
        }.mkString("\n\n")
        variablesString
      }
    }
  }

  def visit(n: VariableDeclarator, arg: Context): String = withComments(n, arg) {
    n.getId.accept(this, arg) + Option(n.getInit).map(" = " + _.accept(this, arg)).getOrElse("")
  }

  def visit(n: VariableDeclaratorId, arg: Context): String = withComments(n, arg) {
    visitName(n.getName)
  }

  def visit(n: ArrayInitializerExpr, arg: Context): String = withComments(n, arg) {
    val values = Option(n.getValues).toList.flatMap(_.map(_.accept(this, arg))).mkString(", ")
    s"Array($values)"
  }

  def visit(n: VoidType, arg: Context): String = "Unit"

  def visit(n: ArrayAccessExpr, arg: Context): String = withComments(n, arg) {
    val name = n.getName.accept(this, arg.copy(arrayAccess = true))
    val index = n.getIndex.accept(this, arg)
    s"$name($index)"
  }

  def visit(n: ArrayCreationExpr, arg: Context): String = withComments(n, arg) {
    if (n.getDimensions != null && !n.getDimensions.isEmpty) {
      val withoutArguments = if (arg.assignType != null) {
        s"new ${arg.assignType.accept(this, arg)}"
      } else {
        val max = n.getArrayCount + 1
        val dimString = (0 until max).map { _ =>
          n.getType.accept(this, arg.copy(typeArg = true))
        }.mkString(",")
        s"Array.ofDim[$dimString]"
      }
      withoutArguments + n.getDimensions.map(stringify(_, arg)).mkString("(", ", ", ")")
    } else {
      n.getInitializer.accept(this, arg)
    }
  }

  def visit(n: AssignExpr, arg: Context): String = withComments(n, arg) {
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
    s"${n.getTarget.accept(this, arg)} $symbol ${n.getValue.accept(this, arg)}"
  }

  def visit(n: BinaryExpr, arg: Context): String = withComments(n, arg) {
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
    s"${n.getLeft.accept(this, arg)} $symbol " + (
    if (settings.splitLongLines && (stringify(n.getLeft, arg).length > 50 || stringify(n.getRight, arg).length > 50)) {
      "\n  "
    } else "") + n.getRight.accept(this, arg)
  }

  def visit(n: CastExpr, arg: Context): String = withComments(n, arg) {
    n.getExpr.accept(this, arg) + (
    if (n.getType.isInstanceOf[PrimitiveType]) {
      s".to${n.getType.accept(this, arg)}"
    } else {
      s".asInstanceOf[${n.getType.accept(this, arg)}]"
    })
  }

  def visit(n: ClassExpr, arg: Context): String = s"classOf[${n.getType.accept(this, arg.copy(classOf = true))}]"

  def visit(n: ConditionalExpr, arg: Context): String = withComments(n, arg) {
    s"if (${n.getCondition.accept(this, arg)}) ${n.getThenExpr.accept(this, arg.copy(mustWrap = true))} else ${n.getElseExpr.accept(this, arg.copy(mustWrap = true))}"
  }

  def visit(n: EnclosedExpr, arg: Context): String = withComments(n, arg) {
    if (n.getInner.isInstanceOf[CastExpr]) {
      n.getInner.accept(this, arg)
    } else {
      s"(${n.getInner.accept(this, arg)})"
    }
  }

  def visit(n: FieldAccessExpr, arg: Context): String = withComments(n, arg) {
    s"${n.getScope.accept(this, arg)}.${visitName(n.getField)}"
  }

  def visit(n: InstanceOfExpr, arg: Context): String = withComments(n, arg) {
    s"${n.getExpr.accept(this, arg)}.isInstanceOf[${n.getType.accept(this, arg)}]"
  }

  def visit(n: CharLiteralExpr, arg: Context): String = s"'${n.getValue}'"

  def visit(n: DoubleLiteralExpr, arg: Context): String = removeUnderscores(n.getValue)

  def visit(n: IntegerLiteralExpr, arg: Context): String = numberValue(n.getValue, "Integer.parseInt")

  def visit(n: LongLiteralExpr, arg: Context): String = numberValue(n.getValue, "java.lang.Long.parseLong")

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

  def visit(n: IntegerLiteralMinValueExpr, arg: Context): String = n.getValue

  def visit(n: LongLiteralMinValueExpr, arg: Context): String = n.getValue

  def visit(n: StringLiteralExpr, arg: Context): String = s""""${n.getValue}""""

  def visit(n: BooleanLiteralExpr, arg: Context): String = String.valueOf(n.getValue)

  def visit(n: NullLiteralExpr, arg: Context): String = "null"

  def visit(n: ThisExpr, arg: Context): String = withComments(n, arg) {
    Option(n.getClassExpr).map { classExpr =>
      s"${classExpr.accept(this, arg)}."
    }.getOrElse("") + "this"
  }

  def visit(n: SuperExpr, arg: Context): String = withComments(n, arg) {
    Option(n.getClassExpr).map { classExpr =>
      s"${classExpr.accept(this, arg)}."
    }.getOrElse("") + "super"
  }

  def visit(n: MethodCallExpr, arg: Context): String = withComments(n, arg) {
    val args = if (n.getArgs == null) 0 else n.getArgs.size
    val shortForm = SHORT_FORM.contains(n.getName) && args < 2 && !n.getArgs.get(0).isInstanceOf[LiteralExpr] || NO_ARGS_SHORT.contains(n.getName) && args == 0
    val scopeString = Option(n.getScope).map { scope =>
      scope.accept(this, arg) + (if ((shortForm && args == 1)) " " else ".")
    }.getOrElse("")
    val methodName = if (METHOD_REPLACEMENTS.contains(n.getName)) {
      METHOD_REPLACEMENTS(n.getName)
    } else {
      visitName(n.getName)
    }
    val argsString = if (n.getName == "asList" && n.getScope != null && n.getScope.toString == "Arrays" && args == 1) {
      // assume Arrays.asList is called with an array argument
      s"(${n.getArgs().get(0).accept(this, arg)}:_*)"
    } else if (arg.arrayAccess) {
      argumentsString(n.getArgs, arg)
    } else if (shortForm) {
      if (args == 1) {
        s" ${n.getArgs.get(0).accept(this, arg)}"
      } else ""
    } else if (!(n.getName.startsWith("get") || n.getName.startsWith("is")) || args > 0) {
      argumentsString(n.getArgs, arg)
    } else ""
    scopeString +
      methodName +
      typeArgsString(n.getTypeArgs, arg) +
      argsString
  }

  def visit(n: ObjectCreationExpr, arg: Context): String = withComments(n, arg) {
    Option(n.getScope).map(_.accept(this, arg) + ".").getOrElse("") +
      "new " +
      typeArgsString(n.getTypeArgs, arg) +
      n.getType.accept(this, arg) +
      argumentsString(n.getArgs, arg) +
      Option(n.getAnonymousClassBody).toList.map { anonClassBody =>
        s" {${membersString(anonClassBody, arg)}}"
      }.mkString
  }

  def visit(n: UnaryExpr, arg: Context): String = withComments(n, arg) {
    import UnaryExpr.{ Operator => Op }
    if (n.getOperator == Op.not && n.getExpr.isInstanceOf[MethodCallExpr] && n.getExpr.asInstanceOf[MethodCallExpr].getName == "equals") {
      val method = n.getExpr.asInstanceOf[MethodCallExpr]
      return new MethodCallExpr(method.getScope, "!=", method.getArgs).accept(this, arg)
    }

    val exprString = n.getExpr.accept(this, arg)
    n.getOperator match {
      case Op.posIncrement => s"{$exprString += 1; $exprString - 1}"
      case Op.posDecrement => s"{$exprString -= 1; $exprString + 1}"
      case Op.positive => s"+$exprString"
      case Op.negative => s"-$exprString"
      case Op.inverse => s"~$exprString"
      case Op.not => s"!$exprString"
      case _ => ""
    }
  }
  def visit(n: ConstructorDeclaration, arg: Context): String = printConstructor(n, arg, false)

  private def printConstructor(n: ConstructorDeclaration, arg: Context, first: Boolean): String = withComments(n, arg) {
    val annotationString = memberAnnotationsString(n.getAnnotations, arg)
    val paramsString = Option(n.getParameters.asScala).toList.flatten.map(_.accept(this, arg)).mkString("(", ", ", ")")
    if (!first) {
      javadocString(n.getJavaDoc, arg) +
        annotationString +
        "def this" +
        typeParametersString(n.getTypeParameters, arg) +
        modifiersString(n.getModifiers) +
        paramsString +
        " = " +
        n.getBlock.accept(this, arg)
    } else {
      annotationString +
        (if (first && (n.getModifiers.isPrivate || n.getModifiers.isProtected)) {
          " "
        } else "") +
        modifiersString(n.getModifiers) +
        paramsString
    }
  }
  def visit(n: MethodDeclaration, arg: Context): String = {
    val argWithInCopyEquals = arg.copy(inObjectEquals = n.getName == "equals" && n.getParameters.size == 1)
    withComments(n, argWithInCopyEquals) {
      val annotationSource = memberAnnotationsString(n.getAnnotations, argWithInCopyEquals)
      val typeString = n.getType match {
        case _: VoidType => "Unit"
        case t => t.accept(this, argWithInCopyEquals)
      }
      val bodyString = Option(n.getBody).map(" = " + _.accept(this, argWithInCopyEquals.copy(mustWrap = n.getType.isInstanceOf[VoidType]))).getOrElse("")

      annotationSource +
        methodModifiersString(n.getModifiers) +
        (if (hasOverride(n) || isHashCode(n) || isEquals(n) || isToString(n)) "override " else "") +
        "def " +
        visitName(n.getName) +
        typeParametersString(n.getTypeParameters, argWithInCopyEquals) +
        n.getParameters.asScala.map(_.accept(this, argWithInCopyEquals)).mkString("(", ", ", ")") +
        ": " + typeString +
        bodyString
    }
  }

  def visit(n: Parameter, arg: Context): String = withAnnotations(n, arg) {
    val valVarString = if (n.getModifiers.isProperty) {
      if (n.getModifiers.isFinal) "val " else "var "
    } else ""
    val typeDeclarationString = if(n.getType.isInstanceOf[UnknownType])
      ""
    else {
      val typeString = "Array[" * n.getId.getArrayCount +
        n.getType.accept(this, arg) +
        "]" * n.getId.getArrayCount +
        (if (n.isVarArgs) "*" else "")
      if (n.getType.isInstanceOf[UnionType])
        " @ " + typeString
      else
        ": " + typeString
    }
    modifiersString(n.getModifiers) +
      valVarString +
      n.getId.accept(this, arg) +
      typeDeclarationString
  }

  def visit(n: MultiTypeParameter, arg: Context): String = withAnnotations(n, arg) {
    val valVarString = if (n.getModifiers.isProperty) {
      if (n.getModifiers.isFinal) "val " else "var "
    } else ""
    modifiersString(n.getModifiers) +
      valVarString +
      n.getId.accept(this, arg) +
      (n.getType.getElements.toList match {
        case tpe :: Nil =>
          ": " + tpe.accept(this, arg)
        case types =>
          " @ " + types.map("_: " + _.accept(this, arg)).mkString("(", " | ", ")")
      })
  }

  def visit(n: ExplicitConstructorInvocationStmt, arg: Context): String = withComments(n, arg) {
    if (n.isThis) {
      typeArgsString(n.getTypeArgs, arg) + "this" + argumentsString(n.getArgs, arg)
    } else {
      s"""this(???) /* TODO: Scala does not allow multiple super constructor calls
         | * Change this code to call a constructor of the current class instead.
         | * For your convenience, here is the invalid super constructor call:
         | * ${Option(n.getExpr).map(_.accept(this, arg) + ".").getOrElse("")}${typeArgsString(n.getTypeArgs, arg)}}super${argumentsString(n.getArgs, arg)}
         | */
       """.stripMargin
    }
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

  def visit(n: VariableDeclarationExpr, arg: Context): String = withComments(n, arg) {
    val asParameter = n.getModifiers == -1
    val valVarString = if (ModifierSet.isFinal(n.getModifiers)) "val " else "var "
    n.getVars.map { v =>
      val typeAndInit = if (v.getInit == null || v.getInit.isInstanceOf[NullLiteralExpr] || !isTypeInitMatch(n, v)) {
        val initString = if (!asParameter) {
          " = " + (n.getType match {
            case prim: PrimitiveType => Option(v.getInit).map(_.accept(this, arg)).getOrElse(DEFAULTS(prim.getType))
            case _ => null
          })
          //printer.print(if (v.getInit() == null) "_" else "null")
        } else ""
        v.getId.accept(this, arg) + ": " +
          "Array[" * v.getId.getArrayCount + n.getType.accept(this, arg) + "]" * v.getId.getArrayCount +
          initString
      } else {
        v.getInit match {
          case newObj: ObjectCreationExpr if (newObj.getType() != null && (newObj.getType.getTypeArgs() == null || newObj.getType.getTypeArgs.isEmpty)) =>
            n.getType match {
              case ref: ReferenceType =>
                ref.getType match {
                  case tpe: ClassOrInterfaceType => newObj.getType.setTypeArgs(tpe.getTypeArgs())
                  case _ =>
                }
              case _ =>
            }
          case _ =>
        }
        v.getId.accept(this, arg) + ": " +
          "Array[" * v.getId.getArrayCount + n.getType.accept(this, arg) + "]" * v.getId.getArrayCount +
          Option(v.getInit).map(" = " + _.accept(this, arg)).getOrElse("")
      }
      annotationsString(n.getAnnotations, arg) +
        (if (!asParameter) valVarString else "") +
        typeAndInit
    }.mkString("\n")
  }

  def visit(n: TypeDeclarationStmt, arg: Context): String = withComments(n, arg) {
    n.getTypeDeclaration.accept(this, arg)
  }

  def visit(n: AssertStmt, arg: Context): String = withComments(n, arg) {
    s"assert(${n.getCheck.accept(this, arg)})" + Option(n.getMessage).map { msg =>
      s" : ${msg.accept(this, arg)}"
    }.getOrElse("")
  }

  def visit(n: BlockStmt, arg: Context): String = withComments(n, arg) {
    if (!isEmpty(n.getStmts) && !arg.mustWrap && n.getStmts.size == 1 && n.getStmts.get(0).isInstanceOf[SwitchStmt]) {
      return n.getStmts.get(0).accept(this, arg)
    } else if (!isEmpty(n.getStmts) && !arg.mustWrap && n.getStmts.size == 1) {
      return n.getStmts.get(0).accept(this, arg)
    }
    val argNoNeedForWrapping = arg.copy(mustWrap = false)
    Option(n.getStmts).map { stmts =>
      def processStatement(arg: Context, stmt: Statement): String =
        stmt match {
          case b: BlockStmt => b.getStmts.map(_.accept(this, argNoNeedForWrapping)).mkString("\n")
          case s => s.accept(this, argNoNeedForWrapping)
        }

      val stmtStrings = stmts.dropRight(1).map(processStatement(argNoNeedForWrapping.copy(returnOn = true), _)) ++
        stmts.lastOption.map(processStatement(argNoNeedForWrapping, _)).toList
      if (stmtStrings.size == 1 && !arg.mustWrap)
        stmtStrings.head
      else
        stmtStrings.mkString("{\n", "\n", "\n}")
    }.getOrElse("")
  }

  def visit(n: LabeledStmt, arg: Context): String =
    s"${n.getLabel}: ${n.getStmt.accept(this, arg)}"

  def visit(n: EmptyStmt, arg: Context): String = withComments(n, arg) {
    ""
  }

  def visit(n: ExpressionStmt, arg: Context): String = withComments(n, arg) {
    n.getExpression.accept(this, arg)
  }

  def visit(n: SwitchStmt, arg: Context): String = withComments(n, arg) {
    val argNoSkip = arg.copy(skip = false)
    n.getSelector.accept(this, argNoSkip) + " match {\n" +
      Option(n.getEntries).map { entries =>
        // argNoSkip.skip is modified in-place, so we can't just mkString("")
        entries.map(_.accept(this, argNoSkip) + (if (argNoSkip.skip) "" else "\n")).mkString
      }.getOrElse("") +
      "\n}"
  }

  def visit(n: SwitchEntryStmt, arg: Context): String = withComments(n, arg) {
    val matchExpr = if (arg.skip) {
      " | " + Option(n.getLabel).map(_.accept(this, arg)).getOrElse("")
    } else {
      "case " + Option(n.getLabel).map(_.accept(this, arg)).getOrElse("_")
    }
    arg.skip = n.getStmts == null || n.getStmts.size() == 0
    val resultExpr =
      if (!arg.skip) {
        " => " +
          (if (n.getStmts.size == 1) {
            n.getStmts.get(0).accept(this, arg)
          } else {
            "\n" +
              n.getStmts.map(_.accept(this, arg)).mkString("\n")
          })
      } else ""
    matchExpr + resultExpr
  }

  def visit(n: BreakStmt, arg: Context): String = withComments(n, arg) {
    "//break"
  }

  def visit(n: ReturnStmt, arg: Context): String = withComments(n, arg) {
    Option(n.getExpr).map { expr =>
      (if (arg.returnOn) {
        "return "
      } else "") +
        expr.accept(this, arg)
    }.getOrElse("return")
  }

  def visit(n: EnumDeclaration, arg: Context): String = withJavaDoc(n, arg) {
    val implementsString = Option(n.getImplements).map { impl =>
      " implements " + impl.map(_.accept(this, arg)).mkString(", ")
    }.getOrElse("")
    val entriesString = Option(n.getEntries).map(_.map(_.accept(this, arg)).mkString(", ")).getOrElse("")
    val memberString = Option(n.getMembers).map { members =>
      ";" + membersString(members, arg)
    }.getOrElse(Option(n.getEntries).map(_ => "\n").getOrElse(""))
    memberAnnotationsString(n.getAnnotations, arg) +
      modifiersString(n.getModifiers) +
      "enum " +
      n.getName +
      implementsString +
      " {\n" +
      entriesString + "\n" +
      memberString + "\n" +
      "}"
  }

  def visit(n: EnumConstantDeclaration, arg: Context): String = withJavaDoc(n, arg) {
    memberAnnotationsString(n.getAnnotations, arg) +
      n.getName +
      Option(n.getArgs).map(args => argumentsString(args, arg)).getOrElse("") +
      Option(n.getClassBody).map { cb =>
        s" {\n${membersString(n.getClassBody, arg)}\n}"
      }.getOrElse("")
  }

  def visit(n: EmptyMemberDeclaration, arg: Context): String = withJavaDoc(n, arg)()

  def visit(n: InitializerDeclaration, arg: Context): String = withComments(n, arg) {
    Option(n.getBlock.getStmts).map { stmts =>
      stmts.flatMap {
        case _: ExplicitConstructorInvocationStmt => None
        case stmt => Some(stmt.accept(this, arg))
      }.mkString("\n\n")
    }.getOrElse("")
  }

  def visit(n: IfStmt, arg: Context): String = withComments(n, arg) {
    s"if (${n.getCondition.accept(this, arg)}) ${n.getThenStmt.accept(this, arg.copy(mustWrap = true))}" +
      Option(n.getElseStmt).map { elStmt => s" else ${elStmt.accept(this, arg.copy(mustWrap = true))}" }.getOrElse("")
  }

  def visit(n: WhileStmt, arg: Context): String = withComments(n, arg) {
    s"while (${n.getCondition.accept(this, arg)}) ${n.getBody.accept(this, arg)}"
  }

  def visit(n: ContinueStmt, arg: Context): String = withComments(n, arg) {
    "//continue"
  }

  def visit(n: DoStmt, arg: Context): String = withComments(n, arg) {
    s"do ${n.getBody.accept(this, arg)} while (${n.getCondition.accept(this, arg)});"
  }

  def visit(n: ForeachStmt, arg: Context): String = withComments(n, arg) {
    val forExpressionDescent = Iterator.iterate((n: Statement, "", 0)) { case (bdy, _, idx) =>
      bdy match {
        case MaybeInBlock(fe: ForeachStmt) =>
          (
            fe.getBody,
            s"${if (idx > 0) "; " else ""}${fe.getVariable.getVars.get(0).accept(this, arg)} <- ${fe.getIterable.accept(this, arg)}",
            idx + 1
          )
        case MaybeInBlock(ifStmt: IfStmt) =>
          (
            ifStmt.getThenStmt,
            " if " + ifStmt.getCondition.accept(this, arg),
            idx + 1
          )
        case _ => (null, "", idx + 1)
      }
    case o => o
    }.takeUpToWhere { case(bdy, _, _) =>
      !isUnwrapped(bdy)
    }.toVector

    "for (" +
      forExpressionDescent.map(_._2).mkString +
      ") " +
      forExpressionDescent.map(_._1).takeWhile(_ != null).last.accept(this, arg.copy(mustWrap = true))
  }

  private def isUnwrapped(stmt: Statement): Boolean = Types.extract(stmt) match {
    case foreach: ForeachStmt => true
    case ifStmt: IfStmt => ifStmt.getElseStmt() == null
    case _ => false
  }

  def visit(n: ForStmt, arg: Context): String = withComments(n, arg) {
    val loopCondition = Option(n.getCompare).map(_.accept(this, arg)).getOrElse("true")
    val body = if (n.getUpdate != null && n.getBody.isInstanceOf[BlockStmt]) {
      // merge updates into block
      val block = n.getBody.asInstanceOf[BlockStmt]
      block.addAll(n.getUpdate.map(new ExpressionStmt(_)))
      block.accept(this, arg)
    } else {
      val bodyString = n.getBody.accept(this, arg)
      Option(n.getUpdate).map { update =>
        s"{$bodyString" +
          update.map(_.accept(this, arg)).mkString("\n") +
          "\n}"
      }.getOrElse(bodyString)
    }
    Option(n.getInit).toList.flatten.map { i =>
      i.accept(this, arg)
    }.mkString("", "\n", "\n") +
      s"while ($loopCondition)" +
      body
  }

  def visit(n: ThrowStmt, arg: Context): String = withComments(n, arg) {
    "throw " + n.getExpr.accept(this, arg)
  }

  def visit(n: SynchronizedStmt, arg: Context): String = withComments(n, arg) {
    Option(n.getExpr).map(expr => s"synchronized (${expr.accept(this, arg.copy(mustWrap = true))}) ")
      .getOrElse("synchronized ") +
      n.getBlock.accept(this, arg.copy(mustWrap = true))
  }

  def visit(n: TryStmt, arg: Context): String = withComments(n, arg) {
    val wrapInTry = !isEmpty(n.getCatchs()) || n.getFinallyBlock() != null

    def resourceString(rd: VariableDeclarationExpr): String =
      rd.getVars.map(res => s"${res.getId.accept(this, arg)} <- managed(${res.getInit.accept(this, arg)})").mkString("\n")


    val resourcesString = if (!n.getResources.isEmpty) {
      val forString = "for " +
        (if (n.getResources.size == 1) {
          "(" + resourceString(n.getResources.get(0)) + ")"
        } else {
          n.getResources.map(resourceString).mkString("{", "\n", "} ")
        }) +
        n.getTryBlock.accept(this, arg)
      if (wrapInTry) {
        s"{\n$forString\n}"
      } else
        forString
    } else {
      n.getTryBlock.accept(this, arg)
    }
    val catchString = Option(n.getCatchs).filterNot(_.isEmpty).map { catchs =>
      s""" catch {
         |   ${catchs.map(_.accept(this, arg)).mkString("\n")}
         |}""".stripMargin
    }.getOrElse("")
    val finallyString = Option(n.getFinallyBlock).map(" finally " + _.accept(this, arg)).getOrElse("")
    (if (wrapInTry) {
      "try "
    } else "") +
      resourcesString +
      catchString +
      finallyString
  }

  def visit(n: CatchClause, arg: Context): String = withComments(n, arg) {
    val catchBlockString = Option(n.getCatchBlock.getStmts).map { stmts =>
      if (stmts.size == 1) {
        stmts.get(0).accept(this, arg)
      } else {
        n.getCatchBlock.accept(this, arg)
      }
    }.getOrElse("")

    s"case ${n.getParam.accept(this, arg)} => $catchBlockString\n"
  }

  def visit(n: AnnotationDeclaration, arg: Context): String = withJavaDoc(n, arg) {
    memberAnnotationsString(n.getAnnotations, arg) +
      modifiersString(n.getModifiers) +
      "@interface " +
      n.getName +
      " {\n" +
      Option(n.getMembers).map(m => membersString(m, arg)).toList.mkString +
      "}"
  }

  def visit(n: AnnotationMemberDeclaration, arg: Context): String = withJavaDoc(n, arg) {
    memberAnnotationsString(n.getAnnotations, arg) +
      modifiersString(n.getModifiers) +
      visitName(n.getName) +
      ": " +
      n.getType.accept(this, arg) +
      Option(n.getDefaultValue).map("= " + _.accept(this, arg)).toList.mkString
  }

  def visit(n: MarkerAnnotationExpr, arg: Context): String = s"@${n.getName.accept(this, arg)}"

  def visit(n: SingleMemberAnnotationExpr, arg: Context): String =
    s"@${n.getName.accept(this, arg)}(${n.getMemberValue.accept(this, arg)})"

  def visit(n: NormalAnnotationExpr, arg: Context): String = withComments(n, arg) {
    val pairsString = Option(n.getPairs).toList.flatten.map(_.accept(this, arg)).mkString("(", ", ", ")")
    s"@${n.getName.accept(this, arg)}$pairsString"
  }

  def visit(n: MemberValuePair, arg: Context): String = s"${visitName(n.getName)} = ${n.getValue.accept(this, arg)}"

  def visit(n: LineComment, arg: Context): String = s"//${n.getContent}"

  def visit(n: BlockComment, arg: Context): String = s"/*${n.getContent}*/\n"

  def visit(n: TypeExpr, arg: Context): String = n.getType.accept(this, arg)

  def visit(n: MethodReferenceExpr, arg: Context): String = withComments(n, arg) {
    val typeParams = if (n.getTypeArguments.getTypeArguments.isEmpty()) ""
                     else n.getTypeArguments.getTypeArguments.map(_.accept(this, arg)).mkString("[", ", ", "]")
    s"${n.getScope.accept(this, arg)}.${visitName(n.getIdentifier)}$typeParams"
  }

  def visit(n: LambdaExpr, arg: Context): String = withComments(n, arg) {
    val params = n.getParameters.map(_.accept(this, arg)).mkString("(", ", ", ")")
    val body = n.getBody.accept(this, arg)
    s"${params} => $body"
  }

  def visit(n: UnknownType, y: Context): String = s"Nothing /* Unknown type $n */"

  def visit(n: UnionType, y: Context): String = n.getElements.map("_: " + _.accept(this, y)).mkString("(", " | ", ")")

  def visit(n: IntersectionType, y: Context): String = s"Nothing /* Intersection type $n */"
}

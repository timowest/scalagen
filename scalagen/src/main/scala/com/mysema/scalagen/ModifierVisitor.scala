package com.mysema.scalagen

import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.DocumentableNode
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.PackageDeclaration
import com.github.javaparser.ast.TypeParameter
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.visitor.GenericVisitor
import java.util.{ArrayList, Collections}
import com.mysema.scalagen.ast.BeginClosureExpr

/**
 * 
 */
abstract class ModifierVisitor[A] extends GenericVisitor[Node, A] {
  
  protected def filter[T <: Node](node: T, arg: A): T = {
    if (node != null) node.accept(this, arg).asInstanceOf[T] else node
  }
  
  protected def filter[T <: Node](list: JavaList[T], arg: A): JavaList[T]  = {
    if (list == null) {
      return null
    } else if (list.isEmpty) {
       Collections.emptyList[T]() 
    } else {
      //list.map(_.accept(this, arg).asInstanceOf[T]).filter(_ != null)
      val rv = new ArrayList[T](list.size)
      val it = list.iterator()
      while (it.hasNext) {
        val node = it.next().accept(this, arg).asInstanceOf[T]
        if (node != null) rv.add(node)
      }
      rv
    }    
  }

  def withCommentsFrom[T <: Node](origNode: Node, arg: A)(node: => T): T = {
    val newNode = node
    val comment = origNode match {
      case d: DocumentableNode => Option(javadocFor(d, arg))
      case _ => None
    }
    newNode.setComment(comment.getOrElse(origNode.getComment))
    origNode.getOrphanComments.foreach(newNode.addOrphanComment)

    newNode
  }

  def visitName(name: String, arg: A) = name

  def visit(n: AnnotationDeclaration, arg: A) : Node = withCommentsFrom(n, arg) {
    val rv = new AnnotationDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv    
  }

  def visit(n: AnnotationMemberDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AnnotationMemberDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setDefaultValue(filter(n.getDefaultValue, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setType(filter(n.getType, arg))
    
    n
  }

  def visit(n: ArrayAccessExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayAccessExpr()
    rv.setIndex(filter(n.getIndex, arg))
    rv.setName(filter(n.getName, arg))    
    rv
  }

  def visit(n: ArrayCreationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayCreationExpr()    
    rv.setArrayCount(n.getArrayCount)
    rv.setDimensions(filter(n.getDimensions, arg))
    rv.setInitializer(filter(n.getInitializer, arg))
    rv.setType(filter(n.getType, arg))   
    rv
  }

  def visit(n: ArrayInitializerExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayInitializerExpr()
    rv.setValues(filter(n.getValues, arg))   
    rv
  }

  def visit(n: AssertStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AssertStmt()
    rv.setCheck(filter(n.getCheck, arg))
    rv.setMessage(filter(n.getMessage, arg))    
    rv
  }

  def visit(n: AssignExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AssignExpr()
    rv.setOperator(n.getOperator)
    rv.setTarget(filter(n.getTarget, arg))
    rv.setValue(filter(n.getValue, arg))    
    rv
  }

  def visit(n: BinaryExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new BinaryExpr()
    rv.setOperator(n.getOperator)
    rv.setLeft(filter(n.getLeft, arg))
    rv.setRight(filter(n.getRight, arg))    
    rv
  }

  def visit(n: BlockStmt, arg: A): Node = withCommentsFrom(n, arg) { new BlockStmt(filter(n.getStmts, arg)) }

  def visit(n: BooleanLiteralExpr, arg: A): Node = new BooleanLiteralExpr(n.getValue)

  def visit(n: BreakStmt, arg: A): Node = withCommentsFrom(n, arg) { new BreakStmt(n.getId) }

  def visit(n: CastExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new CastExpr(filter(n.getType, arg), filter(n.getExpr, arg))    
  }

  def visit(n: CatchClause, arg: A): Node = withCommentsFrom(n, arg) {
    new CatchClause(filter(n.getParam, arg), filter(n.getCatchBlock, arg))
  }

  def visit(n: CharLiteralExpr, arg: A): Node = new CharLiteralExpr(n.getValue)

  def visit(n: ClassExpr, arg: A): Node = withCommentsFrom(n, arg) { new ClassExpr(filter(n.getType, arg)) }

  def visit(n: ClassOrInterfaceDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ClassOrInterfaceDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setExtends(filter(n.getExtends, arg))
    rv.setImplements(filter(n.getImplements, arg))
    rv.setInterface(n.isInterface)
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setTypeParameters(filter(n.getTypeParameters, arg))        
    rv
  }

  private def javadocFor(n: DocumentableNode, arg: A) = {
    val filtered = filter(n.getJavaDoc, arg)
    if (filtered == null)
      null
    else
      new JavadocComment(filtered.getContent)
  }

  def visit(n: ClassOrInterfaceType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ClassOrInterfaceType()
    rv.setName(n.getName)
    rv.setScope(filter(n.getScope, arg))
    rv.setTypeArgs(filter(n.getTypeArgs, arg))
    rv
  }

  def visit(n: CompilationUnit, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new CompilationUnit()
    rv.setPackage(filter(n.getPackage, arg))
    rv.setImports(filter(n.getImports, arg))
    rv.setTypes(filter(n.getTypes, arg))
    rv
  }

  def visit(n: ConditionalExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ConditionalExpr()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setThenExpr(filter(n.getThenExpr, arg))
    rv.setElseExpr(filter(n.getElseExpr, arg))
    rv
  }

  def visit(n: ConstructorDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ConstructorDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBlock(filter(n.getBlock, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setParameters(filter(n.getParameters, arg))
    rv.setThrows(filter(n.getThrows, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))
    rv
  }

  def visit(n: ContinueStmt, arg: A): Node = withCommentsFrom(n, arg) { new ContinueStmt(n.getId) }

  def visit(n: DoStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new DoStmt()
    rv.setBody(filter(n.getBody, arg))
    rv.setCondition(filter(n.getCondition, arg))
    rv
  }

  def visit(n: DoubleLiteralExpr, arg: A): Node = new DoubleLiteralExpr(n.getValue)

  def visit(n: EmptyMemberDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val em = new EmptyMemberDeclaration()
    em.setComment(javadocFor(n, arg))
    em
  }

  def visit(n: EmptyStmt, arg: A): Node = new EmptyStmt()

  def visit(n: EmptyTypeDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val em = new EmptyTypeDeclaration()
    em.setComment(javadocFor(n, arg))
    em
  }

  def visit(n: EnclosedExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new EnclosedExpr(filter(n.getInner, arg))
  }

  def visit(n: EnumConstantDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new EnumConstantDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setArgs(filter(n.getArgs, arg))
    rv.setClassBody(filter(n.getClassBody, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setName(n.getName)
    rv
  }

  def visit(n: EnumDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new EnumDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setEntries(filter(n.getEntries, arg))
    rv.setImplements(filter(n.getImplements, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)    
    rv
  }

  def visit(n: ExplicitConstructorInvocationStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ExplicitConstructorInvocationStmt()
    rv.setArgs(filter(n.getArgs, arg))
    rv.setExpr(filter(n.getExpr, arg))
    rv.setThis(n.isThis)    
    rv.setTypeArgs(filter(n.getTypeArgs, arg))
    rv
  }

  def visit(n: ExpressionStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ExpressionStmt(filter(n.getExpression, arg))
  }

  def visit(n: FieldAccessExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new FieldAccessExpr(filter(n.getScope, arg), visitName(n.getField, arg))
  }

  def visit(n: FieldDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new FieldDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setModifiers(n.getModifiers)
    rv.setType(filter(n.getType, arg))
    rv.setVariables(filter(n.getVariables, arg))
    rv
  }

  def visit(n: ForeachStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ForeachStmt()
    rv.setVariable(filter(n.getVariable, arg))
    rv.setIterable(filter(n.getIterable, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: ForStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ForStmt()
    rv.setInit(filter(n.getInit, arg))
    rv.setCompare(filter(n.getCompare, arg))
    rv.setUpdate(filter(n.getUpdate, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: IfStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new IfStmt()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setThenStmt(filter(n.getThenStmt, arg))
    rv.setElseStmt(filter(n.getElseStmt, arg))
    rv
  }

  def visit(n: ImportDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    new ImportDeclaration(n.getName, n.isStatic, n.isAsterisk)    
  }

  def visit(n: InitializerDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new InitializerDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBlock(filter(n.getBlock, arg))
    rv.setStatic(n.isStatic)
    rv
  }

  def visit(n: InstanceOfExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new InstanceOfExpr()
    rv.setExpr(filter(n.getExpr, arg))
    rv.setType(filter(n.getType, arg))
    rv
  }

  def visit(n: IntegerLiteralExpr, arg: A): Node = new IntegerLiteralExpr(n.getValue)

  def visit(n: IntegerLiteralMinValueExpr, arg: A): Node = new IntegerLiteralMinValueExpr()

  def visit(n: JavadocComment, arg: A): Node = new JavadocComment(n.getContent)

  def visit(n: LabeledStmt, arg: A): Node = new LabeledStmt(n.getLabel, filter(n.getStmt, arg))    

  def visit(n: LongLiteralExpr, arg: A): Node = new LongLiteralExpr(n.getValue)

  def visit(n: LongLiteralMinValueExpr, arg: A): Node = new LongLiteralMinValueExpr()

  def visit(n: MarkerAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new MarkerAnnotationExpr(filter(n.getName, arg))
  } 

  def visit(n: MemberValuePair, arg: A): Node = withCommentsFrom(n, arg) {
    new MemberValuePair(n.getName, filter(n.getValue, arg))
  }

  def visit(n: MethodCallExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MethodCallExpr()
    rv.setArgs(filter(n.getArgs, arg))
    rv.setName(n.getName)
    rv.setScope(filter(n.getScope, arg))
    rv.setTypeArgs(filter(n.getTypeArgs, arg))
    rv
  }

  def visit(n: MethodDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MethodDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setArrayCount(n.getArrayCount)
    rv.setBody(filter(n.getBody, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(n.getName)
    rv.setParameters(filter(n.getParameters, arg))
    rv.setThrows(filter(n.getThrows, arg))
    rv.setType(filter(n.getType, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))
    rv
  }

  def visit(n: NameExpr, arg: A): Node = withCommentsFrom(n, arg) {
    n match {
      case closure: BeginClosureExpr => closure
      case _ => new NameExpr(visitName(n.getName, arg))
    }
  }

  def visit(n: NormalAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new NormalAnnotationExpr()
    rv.setName(filter(n.getName, arg))
    rv.setPairs(filter(n.getPairs, arg))
    rv
  }

  def visit(n: NullLiteralExpr, arg: A): Node = new NullLiteralExpr()

  def visit(n: ObjectCreationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ObjectCreationExpr()
    rv.setAnonymousClassBody(filter(n.getAnonymousClassBody, arg))
    rv.setArgs(filter(n.getArgs, arg))
    rv.setScope(filter(n.getScope, arg))
    rv.setType(filter(n.getType, arg))
    rv.setTypeArgs(filter(n.getTypeArgs, arg))
    rv
  }

  def visit(n: PackageDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new PackageDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setName(filter(n.getName, arg))
    rv    
  }
  
  def visit(n: Parameter, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new Parameter()
    visit(n, rv, arg)
    rv.setType(filter(n.getType, arg))
    rv.setVarArgs(n.isVarArgs)
    rv
  }

  def visit(n: MultiTypeParameter, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MultiTypeParameter()
    visit(n, rv, arg)
    rv.setType(new UnionType(n.getType().getElements.map(tpe => filter(tpe, arg))))
    rv
  }

  def visit(n: BaseParameter, rv: BaseParameter, arg: A): Node = withCommentsFrom(n, arg) {
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setId(filter(n.getId, arg))
    rv.setModifiers(n.getModifiers)
    rv
  }

  def visit(n: PrimitiveType, arg: A): Node = new PrimitiveType(n.getType)      

  def visit(n: QualifiedNameExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new QualifiedNameExpr()
    rv.setName(n.getName)
    rv.setQualifier(filter(n.getQualifier, arg))
    rv
  }

  def visit(n: ReferenceType, arg: A): Node = withCommentsFrom(n, arg) {
    new ReferenceType(filter(n.getType, arg), n.getArrayCount)
  }

  def visit(n: ReturnStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ReturnStmt(filter(n.getExpr, arg))
  }

  def visit(n: SingleMemberAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new SingleMemberAnnotationExpr(filter(n.getName, arg), filter(n.getMemberValue, arg))
  }

  def visit(n: StringLiteralExpr, arg: A): Node = new StringLiteralExpr(n.getValue)

  def visit(n: SuperExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new SuperExpr(filter(n.getClassExpr, arg))
  }

  def visit(n: SwitchEntryStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SwitchEntryStmt()
    rv.setLabel(filter(n.getLabel, arg))
    rv.setStmts(filter(n.getStmts, arg))
    rv
  }

  def visit(n: SwitchStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SwitchStmt()
    rv.setSelector(filter(n.getSelector, arg))
    rv.setEntries(filter(n.getEntries, arg))
    rv
  }

  def visit(n: SynchronizedStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SynchronizedStmt() 
    rv.setExpr(filter(n.getExpr, arg))
    rv.setBlock(filter(n.getBlock, arg))
    rv
  }

  def visit(n: ThisExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new ThisExpr(filter(n.getClassExpr, arg))
  }

  def visit(n: ThrowStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ThrowStmt(filter(n.getExpr, arg))
  }

  def visit(n: TryStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new TryStmt()
    rv.setResources(filter(n.getResources, arg))
    rv.setTryBlock(filter(n.getTryBlock, arg))
    rv.setCatchs(filter(n.getCatchs, arg))
    rv.setFinallyBlock(filter(n.getFinallyBlock, arg))
    rv
  }

  def visit(n: TypeDeclarationStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new TypeDeclarationStmt(filter(n.getTypeDeclaration, arg))
  }

  def visit(n: TypeParameter, arg: A): Node = withCommentsFrom(n, arg) {
    new TypeParameter(n.getName, filter(n.getTypeBound, arg))
  }

  def visit(n: UnaryExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new UnaryExpr(filter(n.getExpr, arg), n.getOperator)    
  }

  def visit(n: VariableDeclarationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new VariableDeclarationExpr()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setModifiers(n.getModifiers)
    rv.setType(filter(n.getType, arg))
    rv.setVars(filter(n.getVars, arg))
    rv
  }

  def visit(n: VariableDeclarator, arg: A): Node = withCommentsFrom(n, arg) {
    new VariableDeclarator(filter(n.getId, arg), filter(n.getInit, arg))
  }

  def visit(n: VariableDeclaratorId, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new VariableDeclaratorId()
    rv.setArrayCount(n.getArrayCount)
    rv.setName(visitName(n.getName, arg))
    rv
  }

  def visit(n: VoidType, arg: A): Node = new VoidType()

  def visit(n: WhileStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new WhileStmt()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  def visit(n: WildcardType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new WildcardType()
    rv.setExtends(filter(n.getExtends, arg))
    rv.setSuper(filter(n.getSuper, arg))
    rv
  }

  def visit(n: BlockComment, arg: A): Node = new BlockComment(n.getContent)

  def visit(n: LineComment, arg: A): Node = new LineComment(n.getContent)

  def visit(n: TypeExpr, arg: A): Node =
    new TypeExpr(
      n.getRange,
      filter(n.getType, arg)
    )

  def visit(n: MethodReferenceExpr, arg: A): Node =
    new MethodReferenceExpr(
      n.getRange,
      filter(n.getScope, arg),
      n.getTypeArguments,
      visitName(n.getIdentifier, arg)
    )
  def visit(n: LambdaExpr, arg: A): Node =
    new LambdaExpr(
      n.getRange,
      filter(n.getParameters, arg),
      filter(n.getBody, arg),
      n.isParametersEnclosed()
    )

  def visit(n: UnknownType, arg: A): Node =
    new UnknownType()

  def visit(n: UnionType, arg: A): Node =
    new UnionType(filter(n.getElements, arg))

  def visit(n: IntersectionType, arg: A): Node =
    new IntersectionType(filter(n.getElements, arg))

}

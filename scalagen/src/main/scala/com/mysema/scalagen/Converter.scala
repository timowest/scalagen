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

import java.io.File
import japa.parser.JavaParser
import japa.parser.ast.{ImportDeclaration, CompilationUnit}
import org.apache.commons.io.FileUtils
import java.util.ArrayList

object Converter {
  
  /**
   * default instance for Converter type
   */
  lazy val instance = new Converter("UTF-8",List[UnitTransformer](
    Rethrows, 
    RemoveAsserts, 
    Annotations,
    Enums,
    Primitives,
    SerialVersionUID,
    ControlStatements, 
    CompanionObject,
    BeanProperties, 
    Constructors, 
    Initializers))  
  
}

/**
 * Converter converts Java sources into Scala sources
 */
class Converter(encoding: String, transformers: List[UnitTransformer]) {
    
  def convert(inFolder: File, outFolder: File) {
    val inFolderLength = inFolder.getPath.length + 1
    val inToOut = getJavaFiles(inFolder)
      .map(in => (in, toOut(inFolderLength, outFolder, in))) 
      
    // create out folders
    inToOut.foreach(_._2.getParentFile.mkdirs() )  
    // convert files in parallel
    JavaParser.setCacheParser(false)
    inToOut.par.foreach{ case (in,out) => convertFile(in,out) }
  }
  
  def convertFile(in: File, out: File) {
    val compilationUnit = JavaParser.parse(in, encoding)
    val sources = toScala(compilationUnit)   
    FileUtils.writeStringToFile(out, sources, "UTF-8")
  }
    
  def toScala(unit: CompilationUnit): String = {
    if (unit.getImports == null) {
      unit.setImports(new ArrayList[ImportDeclaration]())  
    }    
    val transformed = transformers.foldLeft(unit) { case (u,t) => t.transform(u) }    
    var visitor = new ScalaDumpVisitor()
    unit.accept(visitor, new Context())
    visitor.getSource
  }
  
  private def toOut(inFolderLength: Int, outFolder: File, in: File): File = {
    val offset = if (in.getName == "package-info.java") 10 else 5
    new File(outFolder, in.getPath.substring(inFolderLength, in.getPath.length-offset)+".scala")
  }
  
  private def getJavaFiles(file: File): Seq[File] = {
    if (file.isDirectory) {
      file.listFiles.toSeq
        .filter(f => f.isDirectory || f.getName.endsWith(".java"))
        .flatMap(f => getJavaFiles(f))
    } else {
      List(file)
    }
  }
  
}
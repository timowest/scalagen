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

import com.mysema.scala.BeanUtils
import Types._

/**
 * 
 */
trait BeanHelpers extends Helpers {
    
  private val getter = "get\\w+".r
  
  private val setter = "set\\w+".r
  
  private val booleanGetter = "is\\w+".r
  
  def isBeanGetter(method: Method): Boolean = method match {
    case Method(getter(_*), t, Nil, Return(field(_))) => true
    case Method(getter(_*), t, Nil, b: Block) => isLazyCreation(b, getProperty(method))
    case _ => false
  }
  
  def isBooleanBeanGetter(method: Method): Boolean = method match {
    case Method(booleanGetter(_*), Type.Boolean, Nil, Return(field(_))) => true
    case Method(booleanGetter(_*), Type.Boolean, Nil, b: Block) => isLazyCreation(b, getProperty(method))
    case _ => false
  }
      
  def isBeanSetter(method: Method): Boolean = method match {
    case Method(setter(_*), Type.Void, _ :: Nil, Stmt(_ set _)) => true
    case _ => false
  }  
  
  def getProperty(method: Method) = {
    val name = method.getName
    BeanUtils.uncapitalize(name.substring(if (name.startsWith("is")) 2 else 3))
  }
}
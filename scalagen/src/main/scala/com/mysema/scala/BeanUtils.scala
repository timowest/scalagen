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
package com.mysema.scala

import java.beans.Introspector
import org.apache.commons.lang3.StringUtils

object BeanUtils {

  def capitalize(name: String): String = {
    if (name.length > 1 && Character.isUpperCase(name.charAt(1))) {
       name
    } else {
       StringUtils.capitalize(name)
    }
  }

  def uncapitalize(name: String): String = Introspector.decapitalize(name);

}
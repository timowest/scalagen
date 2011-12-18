package com.mysema.scala

import java.beans.Introspector
import org.apache.commons.lang3.StringUtils

object BeanUtils {

  def capitalize(name: String): String = {
    if (name.length() > 1 && name.charAt(1).isUpperCase) {
       name
    } else {
       StringUtils.capitalize(name)
    }
  }

  def uncapitalize(name: String ) = Introspector.decapitalize(name);

}
/*
 * Copyright (C) 2011, James McMahon
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

/**
 * Simple harness to facilitate running scalagen from the command line
 */
object Cli {
  val usage = "USAGE: scalagen <src-directory> <target-directory>"

  def main(args: Array[String]) {
    if (args.length != 2) {
      println(usage)
      return
    }

    val in = new File(args(0))
    if (in.exists) {
      val out = new File(args(1))
      Converter.instance.convert(in, out)
    }
  }
}

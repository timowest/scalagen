package com.mysema.scalagen

sealed trait ScalaVersion

case object Scala29 extends ScalaVersion

case object Scala210 extends ScalaVersion

object ScalaVersion {
  def getVersion(versionNumberString: String) = {
    if (versionNumberString.startsWith("2.9.")) Scala29
    else if (versionNumberString.startsWith("2.10.")) Scala210
    else throw new IllegalArgumentException("Unsupported scala version: " + versionNumberString)
  }
}

package frawa.typesjson.formats

import scala.util.Try
import scala.scalajs.js

object Platform:
  val platform: String = "js"

  def verifyHostname(hostname: String): Boolean =
    Try {
      val puny = Punycode.toASCII(hostname)
      //   println(s"FW c ${js.JSON.stringify(c)}")
      true
    }.getOrElse(false)

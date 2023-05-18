package frawa.typesjson.formats

import java.net.IDN
import scala.util.Try

object Platform:
  val platform: String = "jvm"

  def verifyHostname(hostname: String): Boolean =
    Try(IDN.toASCII(hostname, IDN.USE_STD3_ASCII_RULES)).isSuccess

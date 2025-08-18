// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.sso.service

import cats.implicits.*
import ciris.*
import com.comcast.ip4s.Port
import lucuma.sso.client.util.GpgPublicKeyReader
import lucuma.sso.service.util.GpgPrivateKeyReader

import java.net.URI
import java.net.URISyntaxException
import java.security.PrivateKey
import java.security.PublicKey
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.util.UUID

package object config {

  def privateKey(passphrase: String): ConfigDecoder[String, PrivateKey] =
    ConfigDecoder[String].mapOption("Private Key") { s =>
      GpgPrivateKeyReader.privateKey(s, passphrase).toOption
    }

  implicit val publicKey: ConfigDecoder[String, PublicKey] =
    ConfigDecoder[String].mapOption("Public Key") { s =>
      GpgPublicKeyReader.publicKey(s).toOption
    }

  implicit val uri: ConfigDecoder[String, URI] =
    ConfigDecoder[String].mapOption("URI") { s =>
      try Some(new URI(s))
      catch { case _: URISyntaxException => None }
    }

  implicit val uuid: ConfigDecoder[String, UUID] =
    ConfigDecoder[String].mapOption("UUID") { s =>
      try Some(UUID.fromString(s))
      catch {
        case _: IllegalArgumentException => None
      }
    }

  implicit val port: ConfigDecoder[Int, Port] =
    ConfigDecoder[Int].mapOption("Port")(Port.fromInt)

  // not implicit, we may end up with more than one of these
  val isoLocalDateTime: ConfigDecoder[String, LocalDateTime] =
    ConfigDecoder[String].mapOption("ISO local datetime") { s =>
      try Some(LocalDateTime.parse(s, DateTimeFormatter.ISO_DATE_TIME))
      catch {
        case _: DateTimeParseException => None
      }
    }

  /**
   * Ciris config value that is read from the environment, falling back to Java system properties
   * if the environment variable is not set.
   */
  def envOrProp(name: String): ConfigValue[Effect, String] =
    env(name) or prop(name)

}

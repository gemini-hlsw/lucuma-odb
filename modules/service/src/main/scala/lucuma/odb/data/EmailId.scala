// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import io.circe.Decoder

opaque type EmailId = NonEmptyString

object EmailId:
  def from(nes: NonEmptyString) = nes
  def fromString(s: String): Either[String, EmailId] = NonEmptyString.from(s)
  def unsafeFromString(s: String): EmailId = NonEmptyString.unsafeFrom(s)

  extension (id: EmailId)
    def value: NonEmptyString = id

  // When sending email, the id comes wrapped in superfluous angle brackes. Remove them and make sure we still have something...
  // But, also handle the case where there are no brackets - such as in the webhooks. Or if mailgun drops them in the future.
  private val angleRegex = "^<(.*?)>$".r

  given Decoder[EmailId] = 
    Decoder.decodeString
    .emap(s => NonEmptyString.from(angleRegex.replaceAllIn(s, _.group(1))).leftMap(_ => "Email ID is empty."))



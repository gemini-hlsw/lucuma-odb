// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import io.circe.Encoder
import monocle.Prism

opaque type EmailAddress = String

extension (self: EmailAddress) def value: String = self

object EmailAddress:

  // N.B. this is the same pattern used in the database constraint; if we change one we need to change the other.
  private val Pat = """^\w+@[a-zA-Z_]+?\.[a-zA-Z]{2,3}$""".r

  def fromString: Prism[String, EmailAddress] =
    Prism((s: String) => Option.when(Pat.matches(s))(s))(identity)

  given Encoder[EmailAddress] =
    Encoder.encodeString

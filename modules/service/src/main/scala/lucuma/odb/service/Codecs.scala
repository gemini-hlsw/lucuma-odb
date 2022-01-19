package lucuma.odb.service

// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

import lucuma.core.model._
import lucuma.core.util.Gid
import skunk._
import skunk.data.Type

// Codecs for some atomic types.
trait Codecs {

  val orcid_id: Codec[OrcidId] =
    Codec.simple[OrcidId](
      _.value.toString(),
      OrcidId.fromValue(_),
      Type.varchar
    )

  val user_id: Codec[User.Id] = {
    val prism = Gid[User.Id].fromString
    Codec.simple(
      prism.reverseGet,
      s => prism.getOption(s).toRight(s"Invalid user id: $s"),
      Type.varchar
    )
  }

}

object Codecs extends Codecs
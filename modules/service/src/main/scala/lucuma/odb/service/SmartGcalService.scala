// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Sync
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import fs2.Chunk
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Flag
import fs2.io.file.Flags
import fs2.io.file.Path
import lucuma.core.model.sequence.StepConfig
import lucuma.odb.smartgcal.data.GmosNorth
import lucuma.odb.util.Codecs
import skunk.*
import skunk.implicits.*

trait SmartGcalService[F[_]] {
   // placeholder
}

object SmartGcalService {

  def fromSession[F[_]: Sync](
    session: Session[F]
  ): SmartGcalService[F] =

    new SmartGcalService[F] {

    }


  object Statements {

  }

}



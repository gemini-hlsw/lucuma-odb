// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.hashes

import cats.Hash
import cats.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

import java.time.Duration

given hashEnumerated[A: Enumerated]: Hash[A] = Hash.by(summon[Enumerated[A]].tag)

given hashRefined[A: Hash, B]: Hash[A Refined B] =
  Hash.by(_.value)

given Hash[Redshift]        = Hash.by(_.z)
given Hash[Duration]        = Hash.by(_.getNano())
given Hash[NonNegDuration]  = Hash.by(_.value)
given Hash[SourceProfile]   = Hash.fromUniversalHashCode[SourceProfile]
given Hash[SignalToNoise]   = Hash.by(_.toBigDecimal)
given Hash[Wavelength]      = Hash.by(_.toPicometers.value)
given Hash[TimeSpan]        = Hash.by(_.toMilliseconds)
given Hash[GmosAmpCount]    = Hash.by(_.tag)
given Hash[GmosAmpGain]     = Hash.by(_.tag)
given Hash[GmosAmpReadMode] = Hash.by(_.tag)
given Hash[GmosCcdMode]     = Hash.by(x => (x.xBin, x.yBin, x.ampCount, x.ampGain, x.ampReadMode))
given Hash[Attachment.Id]   = Hash.by(_.value)

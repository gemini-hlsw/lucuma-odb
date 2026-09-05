// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.hashes

import cats.Hash
import cats.implicits.*
import eu.timepit.refined.*
import eu.timepit.refined.api.Refined
import lucuma.core.enums.GmosAmpCount
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.math.Angle
import lucuma.core.math.Redshift
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.Attachment
import lucuma.core.model.GmosIfuAnalysis
import lucuma.core.model.NonNegDuration
import lucuma.core.model.SourceProfile
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.core.model.sequence.gmos.GmosCcdMode
import lucuma.core.model.sequence.gnirs.GnirsFpu
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan

import java.time.Duration

given hashEnumerated[A: Enumerated]: Hash[A] = Hash.by(summon[Enumerated[A]].tag)

given hashRefined[A: Hash, B]: Hash[A Refined B] =
  Hash.by(_.value)

given Hash[Angle]           = Hash.by(_.toMicroarcseconds)
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

given Hash[Flamingos2FpuMask] = Hash.by:
  case Flamingos2FpuMask.Imaging         => (0, "img")
  case Flamingos2FpuMask.Builtin(b)      => (1, b.tag)
  case Flamingos2FpuMask.Custom(_, mask) => (2, mask.tag)

given Hash[GnirsFpu.Spectroscopy] = Hash.by:
  case GnirsFpu.Spectroscopy.Slit(s) => (0, s.tag)
  case GnirsFpu.Spectroscopy.Ifu(i)  => (1, i.tag)

given Hash[GmosIfuAnalysis] = Hash.by:
  case GmosIfuAnalysis.Sum(radius)    => (0, radius.toMicroarcseconds)
  case GmosIfuAnalysis.Single(offset) => (1, offset.toMicroarcseconds)

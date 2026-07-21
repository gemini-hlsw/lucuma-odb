// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.goa.GoaParams
import lucuma.catalog.goa.syntax.*
import lucuma.core.enums.Flamingos2LyotWheel
import lucuma.core.enums.Instrument
import lucuma.core.geom.ShapeExpression
import lucuma.core.geom.flamingos2.scienceArea as flamingos2ScienceArea
import lucuma.core.geom.ghost.scienceArea as ghostScienceArea
import lucuma.core.geom.gmos.scienceArea as gmosScienceArea
import lucuma.core.geom.gnirs.scienceArea as gnirsScienceArea
import lucuma.core.geom.igrins2.scienceArea as igrins2ScienceArea
import lucuma.core.geom.jts.interpreter.given
import lucuma.core.geom.visitors.visitorScienceArea
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.math.Offset
import lucuma.core.model.Target
import lucuma.core.model.sequence.flamingos2.Flamingos2FpuMask
import lucuma.odb.data.GoaSearchCenter
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.exchange.Config as Exchange
import lucuma.odb.sequence.flamingos2.imaging.Config as Flamingos2Imaging
import lucuma.odb.sequence.flamingos2.longslit.Config as Flamingos2LongSlit
import lucuma.odb.sequence.ghost.ifu.Config as GhostIfu
import lucuma.odb.sequence.gmos.imaging.Config.GmosNorth as GmosNorthImaging
import lucuma.odb.sequence.gmos.imaging.Config.GmosSouth as GmosSouthImaging
import lucuma.odb.sequence.gmos.longslit.Config.GmosNorth as GmosNorthLongSlit
import lucuma.odb.sequence.gmos.longslit.Config.GmosSouth as GmosSouthLongSlit
import lucuma.odb.sequence.gnirs.imaging.Config as GnirsImaging
import lucuma.odb.sequence.gnirs.spectroscopy.Config as GnirsSpectroscopy
import lucuma.odb.sequence.igrins2.longslit.Config as Igrins2LongSlit
import lucuma.odb.sequence.visitor.Config as Visitor

/**
 * The science policy that turns an observation into the set of GOA queries an
 * Archive Duplication Search runs: which instruments to search against, where
 * to search, and how wide.
 *
 * Everything here is pure — loading the observation is the caller's job.  These
 * are the decisions expected to change as science policy evolves, so they are
 * deliberately kept apart from the service that runs the queries.
 */
object GoaQueryPolicy:

  /** One asterism member, reduced to what the search center depends on. */
  enum TargetPointing:
    case Sidereal
    case NonSidereal(name: NonEmptyString)
    case Unresolvable

  object TargetPointing:

    def fromTarget(target: Target): TargetPointing =
      target match
        case _: Target.Sidereal    => Sidereal
        case n: Target.Nonsidereal => NonSidereal(n.name)
        case _: Target.Opportunity => Unresolvable

  /**
   * The Duplication-Equivalence Group: every instrument an observation taken
   * with `instrument` is searched against, itself included.
   *
   * GOA offers a single `GMOS` umbrella covering both sites, but `GoaParams`
   * carries an `Instrument` and the client derives the archive name from
   * `goaName`, so the umbrella is expressed here as one query per site.  The
   * union of the two is the same record set; the consequence is that GOA's
   * 500-record cap applies to each site separately rather than to the pair.
   *
   * Instruments GOA does not know about (MAROON-X) yield no queries at all, so
   * the observation is reported as not checked.
   */
  def equivalenceGroup(instrument: Instrument): List[Instrument] =
    val group = instrument match
      case Instrument.GmosNorth | Instrument.GmosSouth  =>
        List(Instrument.GmosNorth, Instrument.GmosSouth)
      case Instrument.Alopeke | Instrument.Zorro        =>
        List(Instrument.Alopeke, Instrument.Zorro)
      case Instrument.Flamingos2 | Instrument.Gnirs     =>
        List(Instrument.Flamingos2, Instrument.Gnirs)
      case i                                            =>
        List(i)
    group.filter(_.goaName.isDefined)

  /**
   * Where to search.  An explicit base wins; otherwise a wholly non-sidereal
   * asterism is searched by target name and anything else by the asterism
   * center evaluated at the observation's reference time.  `None` means the
   * observation has no resolvable pointing and no usable name.
   */
  def searchCenter(
    explicitBase:   Option[Coordinates],
    asterismCenter: Option[Coordinates],
    asterism:       List[TargetPointing]
  ): Option[GoaSearchCenter] =
    val movingTargetName =
      asterism.toNel.flatMap:
        _.traverse:
          case TargetPointing.NonSidereal(n) => n.some
          case _                             => none
        .map(_.head)

    explicitBase.map(GoaSearchCenter.Sidereal(_))
      .orElse(movingTargetName.map(GoaSearchCenter.NonSidereal(_)))
      .orElse(asterismCenter.map(GoaSearchCenter.Sidereal(_)))

  /**
   * How wide to search: half the observation's field of view, taken as the
   * angular distance from the science area's origin to its most distant vertex.
   * There is deliberately no minimum, so small-aperture modes search narrowly.
   *
   * `None` when the mode has no Gemini science area to measure (exchange
   * observations, GNIRS acquisition and pupil-viewer apertures).
   */
  def searchRadius(mode: ObservingMode): Option[Angle] =
    scienceAreas(mode)
      .map(_.eval.radius)
      .maximumOption(using Angle.AngleOrder)
      .filter(_.toMicroarcseconds > 0L)

  /**
   * The queries to run for an observation, empty when it cannot be checked at
   * all.  `dateRange` is left unset in v1: we persist a snapshot of the results
   * rather than pinning the query to the submission deadline.
   */
  def queries(
    mode:           ObservingMode,
    explicitBase:   Option[Coordinates],
    asterismCenter: Option[Coordinates],
    asterism:       List[TargetPointing]
  ): List[GoaParams] =
    val params =
      for
        instrument <- mode.instrument
        center     <- searchCenter(explicitBase, asterismCenter, asterism)
        radius     <- searchRadius(mode)
      yield equivalenceGroup(instrument).map: i =>
        center match
          case GoaSearchCenter.Sidereal(c)    => GoaParams.Sidereal(c, i, radius)
          case GoaSearchCenter.NonSidereal(n) => GoaParams.NonSidereal(n.value, i, radius)
    params.orEmpty

  private def scienceAreas(mode: ObservingMode): List[ShapeExpression] =
    val pa  = Angle.Angle0
    val off = Offset.Zero
    mode match
      case _: Exchange           =>
        Nil
      case _: Flamingos2Imaging  =>
        List(flamingos2ScienceArea.shapeAt(pa, off, Flamingos2LyotWheel.F16, Flamingos2FpuMask.Imaging))
      case c: Flamingos2LongSlit =>
        List(flamingos2ScienceArea.shapeAt(pa, off, Flamingos2LyotWheel.F16, Flamingos2FpuMask.Builtin(c.fpu)))
      case _: GhostIfu           =>
        List(ghostScienceArea.fov)
      case _: GmosNorthImaging   =>
        List(gmosScienceArea.imaging)
      case _: GmosSouthImaging   =>
        List(gmosScienceArea.imaging)
      case c: GmosNorthLongSlit  =>
        List(gmosScienceArea.longSlitMode.shapeAt(pa, off, c.fpu.asLeft))
      case c: GmosSouthLongSlit  =>
        List(gmosScienceArea.longSlitMode.shapeAt(pa, off, c.fpu.asRight))
      case c: GnirsImaging       =>
        c.filters.toList.map(f => gnirsScienceArea.imagingShapeAt(pa, off, c.camera, f.filter))
      case c: GnirsSpectroscopy  =>
        gnirsScienceArea.shapeAt(pa, off, c.fpu, c.camera, c.prism).toList
      case _: Igrins2LongSlit    =>
        List(igrins2ScienceArea.scienceSlitFOV)
      case c: Visitor            =>
        List(visitorScienceArea.fov(c.scienceFovDiameter))

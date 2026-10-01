// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.effect.IO
import cats.syntax.either.*
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.enums.StepStage
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.odb.util.Codecs.observation_id
import lucuma.odb.util.Codecs.program_id
import lucuma.odb.util.Codecs.sequence_type
import skunk.Query
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8
import skunk.codec.text.text
import skunk.syntax.all.*

class cloneObservationSequence extends query.ExecutionTestSupportForGmos with ReplaceGmosNorthSequenceOps:

  import GmosNorthFilter.*

  private def cloneQuery(oid: Observation.Id, mode: Option[String], set: Option[String]): String =
    s"""
      mutation {
        cloneObservation(input: {
          observationId: "$oid"
          ${mode.fold("")(m => s"sequence: $m")}
          ${set.fold("")(s => s"SET: $s")}
        }) {
          newObservation { id }
        }
      }
    """

  private def cloneAs(oid: Observation.Id, mode: Option[String], set: Option[String] = None): IO[Observation.Id] =
    query(pi, cloneQuery(oid, mode, set)).map: json =>
      json.hcursor.downFields("cloneObservation", "newObservation", "id").require[Observation.Id]

  private def replaceScience(oid: Observation.Id, atoms: String*): IO[List[(Atom.Id, List[Step.Id])]] =
    query(pi, mutation(Instrument.GmosNorth, input(oid, SequenceType.Science, atoms*)))
      .map(mutationOutput(Instrument.GmosNorth, _))

  private def runStep(sid: Step.Id, vid: Visit.Id, last: StepStage): IO[Unit] =
    addStepEventAs(serviceUser, sid, vid, StepStage.StartStep) *>
    addStepEventAs(serviceUser, sid, vid, last).void

  private def complete(sid: Step.Id, vid: Visit.Id): IO[Unit] =
    runStep(sid, vid, StepStage.EndStep)

  private def abort(sid: Step.Id, vid: Visit.Id): IO[Unit] =
    runStep(sid, vid, StepStage.Abort)

  private def start(sid: Step.Id, vid: Visit.Id): IO[Unit] =
    addStepEventAs(serviceUser, sid, vid, StepStage.StartStep).void

  private val ObservationCount: Query[Program.Id, Long] =
    sql"SELECT COUNT(*) FROM t_observation WHERE c_program_id = $program_id".query(int8)

  private def observationCount(pid: Program.Id): IO[Long] =
    withSession(_.unique(ObservationCount)(pid))

  private val IsMaterialized: Query[(Observation.Id, SequenceType), Boolean] =
    sql"""
      SELECT EXISTS (
        SELECT 1 FROM t_sequence_materialization
        WHERE c_observation_id = $observation_id AND c_sequence_type = $sequence_type
      )
    """.query(bool)

  private def isMaterialized(oid: Observation.Id, st: SequenceType): IO[Boolean] =
    withSession(_.unique(IsMaterialized)(oid, st))

  // (atom description, filter, has execution row) in stored order
  private val StoredSteps: Query[(Observation.Id, SequenceType), (Option[String], String, Boolean)] =
    sql"""
      SELECT a.c_description, d.c_filter::text, se.c_step_id IS NOT NULL
      FROM t_atom a
      JOIN t_step s ON s.c_atom_id = a.c_atom_id
      JOIN t_gmos_north_dynamic d ON d.c_step_id = s.c_step_id
      LEFT JOIN t_step_execution se ON se.c_step_id = s.c_step_id
      WHERE a.c_observation_id = $observation_id AND a.c_sequence_type = $sequence_type
      ORDER BY a.c_atom_index, s.c_step_index
    """.query(text.opt *: text *: bool)

  private def storedScience(oid: Observation.Id): IO[List[(String, String)]] =
    withSession(_.execute(StoredSteps)((oid, SequenceType.Science))).map: rows =>
      assert(rows.forall(!_._3), s"clone has executed steps: $rows")
      rows.map((d, f, _) => (d.getOrElse(""), f))

  private val FrozenItc: Query[Observation.Id, Boolean] =
    sql"""
      SELECT EXISTS (
        SELECT 1 FROM t_itc_result WHERE c_observation_id = $observation_id AND c_is_frozen
      )
    """.query(bool)

  private def hasFrozenItc(oid: Observation.Id): IO[Boolean] =
    withSession(_.unique(FrozenItc)(oid))

  private val longSlit: IO[Observation.Id] =
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
    yield o

  private def filterTag(f: GmosNorthFilter): String = f.tag

  test("default (NONE) does not copy a materialized sequence"):
    for
      o <- longSlit
      _ <- replaceScience(o, atomInput("A", stepInput(GPrime)))
      c <- cloneAs(o, None)
      m <- isMaterialized(c, SequenceType.Science)
    yield assert(!m)

  test("ALL_STEPS copies an edited, never-executed sequence"):
    for
      o <- longSlit
      _ <- replaceScience(o, atomInput("A", stepInput(GPrime), stepInput(RPrime)), atomInput("B", stepInput(IPrime)))
      c <- cloneAs(o, Some("ALL_STEPS"))
      s <- storedScience(c)
      a <- isMaterialized(c, SequenceType.Acquisition)
    yield
      assertEquals(s, List("A" -> filterTag(GPrime), "A" -> filterTag(RPrime), "B" -> filterTag(IPrime)))
      assert(!a, "acquisition was not materialized on the source")

  test("ALL_STEPS copies every step as pending, atoms ordered by first execution"):
    for
      o   <- longSlit
      ids <- replaceScience(o, atomInput("A", stepInput(GPrime), stepInput(RPrime)), atomInput("B", stepInput(IPrime), stepInput(ZPrime)))
      v   <- recordVisitAs(serviceUser, o)
      _   <- complete(ids(1)._2(0), v)
      _   <- abort(ids(0)._2(0), v)
      c   <- cloneAs(o, Some("ALL_STEPS"))
      s   <- storedScience(c)
    yield assertEquals(s, List("B" -> filterTag(IPrime), "B" -> filterTag(ZPrime), "A" -> filterTag(GPrime), "A" -> filterTag(RPrime)))

  test("PENDING_STEPS copies only steps that have not run"):
    for
      o   <- longSlit
      ids <- replaceScience(o, atomInput("A", stepInput(GPrime), stepInput(RPrime)), atomInput("B", stepInput(IPrime)))
      v   <- recordVisitAs(serviceUser, o)
      _   <- complete(ids(0)._2(0), v)
      c   <- cloneAs(o, Some("PENDING_STEPS"))
      s   <- storedScience(c)
    yield assertEquals(s, List("A" -> filterTag(RPrime), "B" -> filterTag(IPrime)))

  test("PENDING_STEPS skips a step that is ongoing"):
    for
      o   <- longSlit
      ids <- replaceScience(o, atomInput("A", stepInput(GPrime), stepInput(RPrime)))
      v   <- recordVisitAs(serviceUser, o)
      _   <- start(ids(0)._2(0), v)
      c   <- cloneAs(o, Some("PENDING_STEPS"))
      s   <- storedScience(c)
    yield assertEquals(s, List("A" -> filterTag(RPrime)))

  test("PENDING_STEPS with nothing left generates the sequence"):
    for
      o   <- longSlit
      ids <- replaceScience(o, atomInput("A", stepInput(GPrime)))
      v   <- recordVisitAs(serviceUser, o)
      _   <- complete(ids(0)._2(0), v)
      c   <- cloneAs(o, Some("PENDING_STEPS"))
      m   <- isMaterialized(c, SequenceType.Science)
    yield assert(!m)

  test("ALL_STEPS on an unmaterialized source is a no-op"):
    for
      o <- longSlit
      c <- cloneAs(o, Some("ALL_STEPS"))
      a <- isMaterialized(c, SequenceType.Acquisition)
      s <- isMaterialized(c, SequenceType.Science)
    yield assert(!a && !s)

  test("ALL_STEPS on a source without an observing mode is a no-op"):
    for
      p <- createProgram
      o <- createObservationAs(pi, p)
      c <- cloneAs(o, Some("ALL_STEPS"))
      s <- isMaterialized(c, SequenceType.Science)
    yield assert(!s)

  test("ALL_STEPS copies the materialized acquisition but not the frozen ITC result"):
    for
      o  <- longSlit
      _  <- recordVisitAs(serviceUser, o)
      a0 <- isMaterialized(o, SequenceType.Acquisition)
      f0 <- hasFrozenItc(o)
      c  <- cloneAs(o, Some("ALL_STEPS"))
      a1 <- isMaterialized(c, SequenceType.Acquisition)
      f1 <- hasFrozenItc(c)
    yield
      assert(a0 && f0, "source setup")
      assert(a1, "acquisition not copied")
      assert(!f1, "frozen ITC result copied")

  test("editing the targets while copying the sequence copies only the science sequence"):
    for
      p  <- createProgram
      t  <- createTargetWithProfileAs(pi, p)
      t2 <- createTargetWithProfileAs(pi, p)
      o  <- createGmosNorthLongSlitObservationAs(pi, p, List(t))
      _  <- replaceScience(o, atomInput("A", stepInput(GPrime)))
      _  <- recordVisitAs(serviceUser, o)
      a0 <- isMaterialized(o, SequenceType.Acquisition)
      c  <- cloneAs(o, Some("ALL_STEPS"), Some(s"""{ targetEnvironment: { asterism: ["$t2"] } }"""))
      s  <- storedScience(c)
      a1 <- isMaterialized(c, SequenceType.Acquisition)
    yield
      assert(a0, "source setup")
      assertEquals(s, List("A" -> filterTag(GPrime)))
      assert(!a1, "acquisition copied despite a target edit")

  test("editing the observing mode while copying the sequence fails"):
    longSlit.flatMap: o =>
      expect(
        user     = pi,
        query    = cloneQuery(o, Some("ALL_STEPS"), Some("""{ observingMode: { gmosNorthLongSlit: { filter: R_PRIME } } }""")),
        expected = List("The observing mode and science requirements cannot be edited when cloning an observation's sequence.").asLeft
      )

  test("editing the science requirements while copying the sequence fails"):
    longSlit.flatMap: o =>
      expect(
        user     = pi,
        query    = cloneQuery(o, Some("ALL_STEPS"), Some("{ scienceRequirements: { exposureTimeMode: { signalToNoise: { value: 75, at: { nanometers: 410 } } } } }")),
        expected = List("The observing mode and science requirements cannot be edited when cloning an observation's sequence.").asLeft
      )

  test("making the clone unsplittable with a multi-atom copied sequence fails"):
    for
      p <- createProgram
      t <- createTargetWithProfileAs(pi, p)
      o <- createGmosNorthImagingObservationAs(pi, p, t)
      _ <- replaceScience(o, atomInput("A", imagingStepInput(GPrime)), atomInput("B", imagingStepInput(IPrime)))
      n <- observationCount(p)
      _ <- expect(
             user     = pi,
             query    = cloneQuery(o, Some("ALL_STEPS"), Some("{ schedulingConstraints: { schedulingMode: NO_SPLITTING } }")),
             expected = List("Unsplittable observations may only contain a single atom.").asLeft
           )
      m <- observationCount(p)
    yield assertEquals(m, n, "failed clone left an observation behind")

// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Path
import grackle.Predicate
import grackle.Predicate.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.Site
import lucuma.core.model.Observation
import lucuma.core.util.Enumerated
import lucuma.odb.graphql.binding.*
import lucuma.odb.syntax.instrument.*

object WhereObservation {

  def binding(path: Path): Matcher[Predicate] = {

    // Site filters are defined in terms of the observation's instrument.
    // For example, a site of GN means that the observation's instrument must be
    // set to one of the Gemini North instruments.
    def siteBinding(binding: Matcher[Site]): Matcher[Predicate] =
      val instrumentPath = path / "instrument"

      def instrumentsForSite(site: Site): List[Instrument] =
        Enumerated[Instrument].all.filter(_.site.contains(site))

      def instrumentsForSites(sites: List[Site]): List[Instrument] =
        sites.flatMap(instrumentsForSite).distinct

      ObjectFieldsBinding.rmap:
        case List(
          BooleanBinding.Option("IS_NULL", rIsNull),
          binding.Option("EQ", rEQ),
          binding.Option("NEQ", rNEQ),
          binding.List.Option("IN", rIN),
          binding.List.Option("NIN", rNIN)
        ) =>
          (rIsNull, rEQ, rNEQ, rIN, rNIN).parMapN {
            (isNull, EQ, NEQ, IN, NIN) =>
              and(List(
                isNull.map(IsNull(instrumentPath, _)),
                EQ.map(site => In(instrumentPath, instrumentsForSite(site).map(_.some))),
                NEQ.map(site => Not(In(instrumentPath, instrumentsForSite(site).map(_.some)))),
                IN.map(sites => In(instrumentPath, instrumentsForSites(sites).map(_.some))),
                NIN.map(sites => Not(In(instrumentPath, instrumentsForSites(sites).map(_.some))))
              ).flatten)
          }

    val SubtitleBinding = WhereOptionString.binding(path / "subtitle")
    val WhereOrderObservationIdBinding = WhereOrder.binding[Observation.Id](path / "id", ObservationIdBinding)
    val WhereReferenceBinding = WhereObservationReference.binding(path / "reference")
    val WhereProgramBinding = WhereProgram.binding(path / "program")
    val ScienceBandBinding = WhereOptionOrder.binding(path / "scienceBand", enumeratedBinding[ScienceBand])
    val InstrumentBinding = WhereOptionEq.binding(path / "instrument", enumeratedBinding[Instrument])
    val SiteBinding = siteBinding(enumeratedBinding[Site])
    val WorkflowBinding = WhereCalculatedObservationWorkflow.binding(path / "workflow")

    lazy val WhereObservationBinding = binding(path)
    ObjectFieldsBinding.rmap {
      case List(
        WhereObservationBinding.List.Option("AND", rAND),
        WhereObservationBinding.List.Option("OR", rOR),
        WhereObservationBinding.Option("NOT", rNOT),
        WhereOrderObservationIdBinding.Option("id", rId),
        WhereReferenceBinding.Option("reference", rRef),
        WhereProgramBinding.Option("program", rProgram),
        SubtitleBinding.Option("subtitle", rSubtitle),
        ScienceBandBinding.Option("scienceBand", rScienceBand),
        InstrumentBinding.Option("instrument", rInstrument),
        SiteBinding.Option("site", rSite),
        WorkflowBinding.Option("workflow", rWorkflow)
      ) =>
        (rAND, rOR, rNOT, rId, rRef, rProgram, rSubtitle, rScienceBand, rInstrument, rSite, rWorkflow).parMapN {
          (AND, OR, NOT, id, ref, program, subtitle, scienceBand, instrument, site, workflow) =>
            and(List(
              AND.map(and),
              OR.map(or),
              NOT.map(Not(_)),
              id,
              ref,
              program,
              subtitle,
              scienceBand,
              instrument,
              site,
              workflow
            ).flatten)
        }
    }
  }

}


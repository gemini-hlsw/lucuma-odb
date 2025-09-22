// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.itc.service.syntax

import cats.data.*
import grackle.Problem
import grackle.Query.Environment
import lucuma.core.math.SignalToNoise
import lucuma.core.math.SingleSN
import lucuma.core.math.TotalSN
import lucuma.itc.ItcCcd
import lucuma.itc.ItcGraph
import lucuma.itc.ItcGraphGroup
import lucuma.itc.ItcSeries
import lucuma.itc.SignificantFigures
import lucuma.itc.math.roundToSignificantFigures
import monocle.Focus
import monocle.std.these.*

import scala.annotation.targetName

trait ItcSyntax:

  extension [A](self: Option[A])
    def toRightIorNec[E](e: => E): Ior[NonEmptyChain[E], A] =
      self match
        case Some(a) => Ior.Right(a)
        case None    => Ior.Left(NonEmptyChain.of(e))

  extension [A](self: A)
    def leftIorNec[B]: Ior[NonEmptyChain[A], B] =
      Ior.Left(NonEmptyChain.of(self))

    def rightIorNec[E]: Ior[NonEmptyChain[E], A] =
      Ior.Right(self)

  extension [A](self: IorNec[Problem, A])
    def addProblem(problem: String): Ior[NonEmptyChain[Problem], A] =
      self.addLeft(NonEmptyChain.of(Problem(problem)))

  extension [A](self: IorNec[String, A])
    def leftProblems: Ior[NonEmptyChain[Problem], A] =
      self.leftMap(_.map(Problem(_)))

  extension (self: String)
    def fromScreamingSnakeCase: String =
      self.split("_").map(_.toLowerCase.capitalize).mkString("")

  def cursorEnv[A] = theseRight[A, Environment].andThen(Focus[Environment](_.env))

  def cursorEnvAdd[A, B](key: String, value: B): Ior[A, Environment] => Ior[A, Environment] =
    cursorEnv[A].modify(_.add((key, value)))

end ItcSyntax

trait ItcGraphSyntax:
  extension (series: ItcSeries)
    def adjustSignificantFigures(figures: SignificantFigures): ItcSeries =
      val data: List[(Double, Double)] =
        series.data.map((x, y) =>
          (figures.xAxis.fold(x)(xDigits => roundToSignificantFigures(x, xDigits.value)),
           figures.yAxis.fold(y)(yDigits => roundToSignificantFigures(y, yDigits.value))
          )
        )
      ItcSeries(series.title, series.seriesType, data)

  extension (graph: ItcGraph)
    def adjustSignificantFigures(figures: SignificantFigures): ItcGraph =
      graph.copy(series = graph.series.map(_.adjustSignificantFigures(figures)))

  extension (sn: TotalSN)
    @targetName("finalAdjust")
    def adjustSignificantFigures(figures: SignificantFigures): TotalSN =
      TotalSN(sn.value.adjustSignificantFigures(figures))

  extension (sn: SingleSN)
    @targetName("finalSingle")
    def adjustSignificantFigures(figures: SignificantFigures): SingleSN =
      SingleSN(sn.value.adjustSignificantFigures(figures))

  extension (sn: SignalToNoise)
    def adjustSignificantFigures(figures: SignificantFigures): SignalToNoise =
      figures.ccd match
        case Some(v) =>
          SignalToNoise.FromBigDecimalRounding
            .getOption(
              roundToSignificantFigures(sn.toBigDecimal, v.value)
            )
            .getOrElse(sn)
        case _       => sn

  extension (group: ItcGraphGroup)
    def adjustSignificantFigures(figures: SignificantFigures): ItcGraphGroup =
      group.copy(graphs = group.graphs.map(_.adjustSignificantFigures(figures)))

  extension (ccd: ItcCcd)
    def adjustSignificantFigures(figures: SignificantFigures): ItcCcd =
      figures.ccd.fold(ccd)(c =>
        ccd.copy(
          singleSNRatio = SingleSN(
            SignalToNoise.FromBigDecimalRounding
              .getOption(roundToSignificantFigures(ccd.singleSNRatio.value.toBigDecimal, c.value))
              .getOrElse(ccd.singleSNRatio.value)
          ),
          maxSingleSNRatio =
            ccd.maxSingleSNRatio.map(d => roundToSignificantFigures(d, c.value).toDouble),
          totalSNRatio = TotalSN(
            SignalToNoise.FromBigDecimalRounding
              .getOption(roundToSignificantFigures(ccd.totalSNRatio.value.toBigDecimal, c.value))
              .getOrElse(ccd.totalSNRatio.value)
          ),
          maxTotalSNRatio =
            ccd.maxTotalSNRatio.map(d => roundToSignificantFigures(d, c.value).toDouble),
          peakPixelFlux = roundToSignificantFigures(ccd.peakPixelFlux, c.value).toDouble,
          wellDepth = roundToSignificantFigures(ccd.wellDepth, c.value).toDouble,
          ampGain = roundToSignificantFigures(ccd.ampGain, c.value).toDouble
        )
      )

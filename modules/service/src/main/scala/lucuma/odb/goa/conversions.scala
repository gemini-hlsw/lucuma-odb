// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.goa

import cats.syntax.all.*
import lucuma.catalog.goa.syntax.*
import lucuma.core.enums.DatasetQaState
import lucuma.core.enums.Instrument
import lucuma.core.enums.ObserveClass

/*
 * Reads the Gemini Observatory Archive's vocabulary in GPP's terms.
 *
 * GOA holds of data written by instruments and classifications done on ocs,
 * so its vocabulary is a superset of gpp.
 *
 * This file contains some conversions of GOA's vocabulary to lucuma-core
 * so as possible we can provide the original goa value as string and a possible
 * interpretation as of gpp
 */

extension (s: String)
  private inline def normalized: String =
    s.trim.toLowerCase

private val instrumentsByArchiveName: Map[String, Instrument] =
  Instrument.values.toList.flatMap(i => i.goaName.map(_.normalized -> i)).toMap

/**
 * `None` for any instrument lucuma-core has no case for — old IGRINS,
 * Michelle, NIFS and the rest of the archive's back catalogue.
 */
def instrument(archiveName: String): Option[Instrument] =
  instrumentsByArchiveName.get(archiveName.normalized)

/**
 * GOA reports the legacy OCS observation class, which draws finer distinctions
 * than `ObserveClass` does, e.g. `partnerCal` and `acqCal`.  These are not present in gpp
 */
def observeClass(archiveClass: String): Option[ObserveClass] =
  archiveClass.normalized match
    case "science" => ObserveClass.Science.some
    case "daycal"  => ObserveClass.DayCal.some
    case "acq"     => ObserveClass.Acquisition.some
    case "progcal" => ObserveClass.NightCal.some
    case _         => none

def qaState(archiveQaState: String): Option[DatasetQaState] =
  archiveQaState.normalized match
    case "pass"   => DatasetQaState.Pass.some
    case "usable" => DatasetQaState.Usable.some
    // The queries require NotFail but let's keep this for completness.
    case "fail"   => DatasetQaState.Fail.some
    case _        => none

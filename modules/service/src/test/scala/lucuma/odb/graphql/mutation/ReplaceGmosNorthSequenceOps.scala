// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.mutation

import lucuma.core.enums.GmosNorthFilter
import lucuma.core.syntax.string.*

trait ReplaceGmosNorthSequenceOps extends ReplaceSequenceOps:

  def stepInput(filter: GmosNorthFilter): String =
    s"""
          {
            instrumentConfig: {
              exposure: {
                seconds: 20
              }
              readout: {
                xBin: ONE
                yBin: ONE
                ampCount: TWELVE
                ampGain: LOW
                ampReadMode: SLOW
              }
              dtax: ZERO
              roi: FULL_FRAME
              gratingConfig: {
                grating: R831_G5302
                order: ZERO
                wavelength: {
                  nanometers: 500.0
                }
              }
              filter: ${filter.tag.toScreamingSnakeCase}
              fpu: {
                builtin: LONG_SLIT_0_50
              }
            }
            stepConfig: {
              science: true
            }
            observeClass: SCIENCE
          }
    """

  def imagingStepInput(filter: GmosNorthFilter): String =
    s"""
          {
            instrumentConfig: {
              exposure: {
                seconds: 20
              }
              readout: {
                xBin: ONE
                yBin: ONE
                ampCount: TWELVE
                ampGain: LOW
                ampReadMode: SLOW
              }
              dtax: ZERO
              roi: FULL_FRAME
              filter: ${filter.tag.toScreamingSnakeCase}
            }
            stepConfig: {
              science: true
            }
            observeClass: SCIENCE
          }
    """

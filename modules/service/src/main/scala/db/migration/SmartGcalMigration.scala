// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package db.migration

/**
 * Performs a smart gcal migration which consists of reading the named
 * `definitionFiles`, parsing them and updating the corresponding tables.
 * Most of this work is delegated to a `SmartGcalLoader` which could be used
 * in other contexts.
 *
 * @param instrumentName identifies the migration in `getDescription`
  */
abstract class SmartGcalMigration(instrumentName: String) extends RepeatableMigration(s"${instrumentName} smart gcal loader")
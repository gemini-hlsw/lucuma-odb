#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 <observation-id>

Triggers an obscalc recalculation marking the obscalc row as pending,
avoiding going through explore.
Obscalc will pick it up, recalculate, and on transition to 'ready' fire 
a ch_obscalc_update notification.
Use it to e.g. test the calibration loop

Example:
  $0 o-123
EOF
  exit 1
}

[[ $# -lt 1 ]] && usage

OBS_ID="$1"

PG_HOST=localhost
PG_USER=jimmy
PG_DATABASE=lucuma-odb
export PGPASSWORD=banana

psql -h "$PG_HOST" -U "$PG_USER" -d "$PG_DATABASE" -v ON_ERROR_STOP=1 <<SQL
UPDATE t_obscalc
SET c_obscalc_state = 'pending',
    c_last_invalidation = now()
WHERE c_observation_id = '${OBS_ID}';
SQL

echo "Updated obs time for ${OBS_ID}"

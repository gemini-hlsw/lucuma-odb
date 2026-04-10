#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 <observation-id>

Triggers an obscalc recalculation by touching the observation time and setting it to now
avoiding going through explore.
Changing the time fires an DB trigger that invalidates obscalc, which in turn
sends the ch_obscalc_update notification.
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
UPDATE t_observation
SET c_observation_time = now()
WHERE c_observation_id = '${OBS_ID}';
SQL

echo "Updated obs time for ${OBS_ID}"

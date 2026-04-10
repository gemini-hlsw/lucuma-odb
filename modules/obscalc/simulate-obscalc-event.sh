#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF
Usage: $0 [psql-options] <observation-id>

Triggers an obscalc recalculation by touching the observation time.
This fires the DB trigger that invalidates obscalc, which in turn
sends the ch_obscalc_update notification.

Arguments:
  observation-id   e.g. o-123

Any flags before the positional arg are passed to psql, e.g.:
  $0 -h localhost -p 5432 -U jimmy -d lucuma-odb o-123

Defaults (from docker-compose):
  host=localhost port=5432 user=jimmy db=lucuma-odb
EOF
  exit 1
}

PSQL_ARGS=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|-p|-U|-d)
      PSQL_ARGS+=("$1" "$2"); shift 2 ;;
    -*)
      PSQL_ARGS+=("$1"); shift ;;
    *)
      break ;;
  esac
done

[[ $# -lt 1 ]] && usage

OBS_ID="$1"

if [[ ${#PSQL_ARGS[@]} -eq 0 ]]; then
  PSQL_ARGS=(-h localhost -p 5432 -U jimmy -d lucuma-odb)
fi

PGPASSWORD="${PGPASSWORD:-banana}" psql "${PSQL_ARGS[@]}" -v ON_ERROR_STOP=1 <<SQL
UPDATE t_observation
SET c_observation_time = now()
WHERE c_observation_id = '${OBS_ID}';
SQL

echo "Updated observation time for ${OBS_ID} — obscalc invalidated"

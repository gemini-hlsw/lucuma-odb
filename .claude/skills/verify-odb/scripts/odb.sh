#!/usr/bin/env bash
# Drive an isolated, locally built ODB for verification. See ../SKILL.md.
set -euo pipefail

REPO="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
SCRIPTS="$(cd "$(dirname "$0")" && pwd)"
DB_PORT="${VERIFY_ODB_DB_PORT:-15432}"
PORT="${VERIFY_ODB_PORT:-18082}"
NAME="verify-odb-${PORT}"
BASE="$REPO/.claude/verify-odb"
RUN="$BASE/run-$PORT"
EVIDENCE_ROOT="$BASE/evidence"
STAGE_LIB="$REPO/modules/service/target/universal/stage/lib"
URL="http://localhost:$PORT"

die() { echo "odb.sh: $*" >&2; exit 1; }

port_pid() { lsof -nP -t -iTCP:"$1" -sTCP:LISTEN 2>/dev/null | head -1 || true; }

cmd_build() {
  # sbt comes from the nix dev shell, which only direnv puts on PATH.
  if command -v sbt >/dev/null; then (cd "$REPO" && sbt -batch -J-Xmx6g -J-Xss4M service/stage)
  else (cd "$REPO" && direnv exec "$REPO" sbt -batch -J-Xmx6g -J-Xss4M service/stage); fi
}

cmd_up() {
  compgen -G "$STAGE_LIB/edu.gemini.lucuma-odb-service-*.jar" >/dev/null || die "no staged ODB in $STAGE_LIB; run: $0 build"
  docker info >/dev/null 2>&1 || die "docker is not running (open -a Docker)"
  [[ -z "$(port_pid "$DB_PORT")" ]] || die "port $DB_PORT is taken; set VERIFY_ODB_DB_PORT"
  [[ -z "$(port_pid "$PORT")" ]] || die "port $PORT is taken; set VERIFY_ODB_PORT"
  mkdir -p "$RUN"

  chmod 0600 "$REPO"/test-cert/* || true
  docker run -d --name "$NAME-db" --label verify-odb="$PORT" \
    -e POSTGRES_USER=jimmy -e POSTGRES_PASSWORD=banana -e POSTGRES_DB=lucuma-odb \
    -v "$REPO/test-cert/server.crt:/var/lib/postgresql/server.crt:ro" \
    -v "$REPO/test-cert/server.key:/var/lib/postgresql/server.key:ro" \
    -p "$DB_PORT:5432" postgres:15 \
    -c ssl=on -c ssl_cert_file=/var/lib/postgresql/server.crt \
    -c ssl_key_file=/var/lib/postgresql/server.key -c jit=off >/dev/null

  # pg_isready passes during the image's temporary init server, so wait for the final restart.
  for _ in $(seq 1 60); do
    if docker logs "$NAME-db" 2>&1 | grep -q 'PostgreSQL init process complete' \
       && docker exec "$NAME-db" pg_isready -U jimmy -d lucuma-odb -h localhost >/dev/null 2>&1; then
      break
    fi
    sleep 1
  done
  docker exec "$NAME-db" pg_isready -U jimmy -d lucuma-odb -h localhost >/dev/null || die "postgres never became ready"

  [[ -f "$RUN/sso-key.pem" ]] || "$SCRIPTS/mint_token.py" keygen "$RUN"

  (
    unset ODB_OTEL_ENDPOINT ODB_OTEL_KEY
    export PORT
    export DATABASE_URL="postgres://jimmy:banana@localhost:$DB_PORT/lucuma-odb"
    export ODB_SSO_PUBLIC_KEY="$(cat "$RUN/sso-key.asc")"
    export ODB_SSO_ROOT="${VERIFY_ODB_SSO_ROOT:-http://localhost:1}"
    export ODB_ITC_ROOT="${VERIFY_ODB_ITC_ROOT:-https://itc-dev.gpp.gemini.edu/itc}"
    export ODB_SERVICE_JWT="$("$SCRIPTS/mint_token.py" token "$RUN" service)"
    export ODB_DOMAIN=localhost CORS_OVER_HTTPS=false
    export CLOUDCUBE_ACCESS_KEY_ID=verify CLOUDCUBE_SECRET_ACCESS_KEY=verify
    export CLOUDCUBE_URL=https://cube.example.com/verify-odb FILE_UPLOAD_MAX_MB=20
    export MAILGUN_API_KEY=verify MAILGUN_DOMAIN=mail.example.com MAILGUN_WEBHOOK_SIGNING_KEY=verify
    export INVITATION_SENDER_EMAIL=verify@example.com EXPLORE_URL=https://explore.example.com
    export PROPOSAL_EMAIL_DEFAULT=verify@example.com
    # The staged bin script is the Heroku launcher (512 MB heap, /opt/docker agent), so run java directly.
    nohup java -Xmx2g -Xss4M -Duser.language=en -Duser.country=US \
      -cp "$STAGE_LIB/*" lucuma.odb.Main serve > "$RUN/odb.log" 2>&1 &
    echo $! > "$RUN/odb.pid"
  )

  for _ in $(seq 1 180); do
    curl -sf "$URL/health" >/dev/null 2>&1 && break
    kill -0 "$(cat "$RUN/odb.pid")" 2>/dev/null || { tail -40 "$RUN/odb.log"; die "ODB exited during startup"; }
    sleep 1
  done
  curl -sf "$URL/health" >/dev/null || { tail -40 "$RUN/odb.log"; die "ODB never answered /health"; }
  cmd_doctor
}

cmd_doctor() {
  local ok=1 pid listener
  [[ -f "$RUN/odb.pid" ]] || die "no run state in $RUN; nothing was started on port $PORT"
  pid="$(cat "$RUN/odb.pid")"
  if kill -0 "$pid" 2>/dev/null; then echo "process   ok   pid $pid"; else echo "process   FAIL pid $pid not running"; ok=0; fi
  listener="$(port_pid "$PORT")"
  if [[ -n "$listener" ]] && { [[ "$listener" == "$pid" ]] || [[ "$(ps -o ppid= -p "$listener" | tr -d ' ')" == "$pid" ]]; }; then
    echo "port      ok   $PORT owned by our process tree"
  else
    echo "port      FAIL $PORT listener is '${listener:-none}', not ours"; ok=0
  fi
  if [[ "$(docker inspect -f '{{.State.Running}} {{index .Config.Labels "verify-odb"}}' "$NAME-db" 2>/dev/null)" == "true $PORT" ]]; then
    echo "database  ok   container $NAME-db"
  else
    echo "database  FAIL container $NAME-db missing or not ours"; ok=0
  fi
  echo "build     $(grep -m1 -o 'Commit[^|]*' "$RUN/odb.log" 2>/dev/null || echo unknown) (HEAD $(git -C "$REPO" rev-parse --short HEAD))"
  local resp
  resp="$(EVIDENCE='' cmd_gql pi '{ __typename }' 2>/dev/null || true)"
  if [[ "$resp" == *'"Query"'* ]]; then echo "auth      ok   minted PI token accepted"; else echo "auth      FAIL $resp"; ok=0; fi
  echo "ready     $(curl -s "$URL/health/ready" | tr -d '\n' | cut -c1-200) (sso probe fails by design)"
  [[ $ok == 1 ]] || die "doctor found problems; see $RUN/odb.log"
}

cmd_token() {
  [[ -f "$RUN/sso-key.pem" ]] || die "no key in $RUN; run: $0 up"
  "$SCRIPTS/mint_token.py" token "$RUN" "$@"
}

# gql <who> <query-or-@file> [variables-json]
cmd_gql() {
  local who="$1" query="$2" vars="${3:-{\}}" tok body
  [[ "$query" == @* ]] && query="$(cat "${query#@}")"
  tok="$(cmd_token "$who")"
  body="$(jq -nc --arg q "$query" --argjson v "$vars" '{query: $q, variables: $v}')"
  local resp
  resp="$(curl -s -X POST "$URL/odb" -H "Authorization: Bearer $tok" -H 'Content-Type: application/json' -d "$body")"
  if [[ -n "${EVIDENCE:-}" ]]; then
    mkdir -p "$EVIDENCE"
    local n; n="$(printf '%03d' "$(find "$EVIDENCE" -name '*-request.json' | wc -l | tr -d ' ')")"
    jq -n --arg who "$who" --argjson b "$body" '{as: $who, body: $b}' > "$EVIDENCE/$n-request.json"
    echo "$resp" | jq . > "$EVIDENCE/$n-response.json" 2>/dev/null || echo "$resp" > "$EVIDENCE/$n-response.json"
  fi
  echo "$resp"
}

# psql: read-only peek at DB state for side-effect proof.
cmd_psql() {
  docker exec -i "$NAME-db" psql -U jimmy -d lucuma-odb -v ON_ERROR_STOP=1 -P pager=off "$@"
}

cmd_evidence() {
  local d="$EVIDENCE_ROOT/$(date +%Y%m%d-%H%M%S)-${1:-run}"
  mkdir -p "$d"
  echo "$d"
}

cmd_down() {
  if [[ -f "$RUN/odb.pid" ]]; then
    local pid; pid="$(cat "$RUN/odb.pid")"
    if kill -0 "$pid" 2>/dev/null; then
      kill "$pid"
      for _ in $(seq 1 20); do kill -0 "$pid" 2>/dev/null || break; sleep 1; done
      kill -9 "$pid" 2>/dev/null || true
    fi
  fi
  if [[ "$(docker inspect -f '{{index .Config.Labels "verify-odb"}}' "$NAME-db" 2>/dev/null)" == "$PORT" ]]; then
    docker rm -f "$NAME-db" >/dev/null
  fi
  rm -rf "$RUN"
  echo "down: port $PORT cleaned; evidence kept in $EVIDENCE_ROOT"
}

case "${1:-}" in
  build|up|doctor|token|gql|psql|evidence|down) c="$1"; shift; "cmd_$c" "$@" ;;
  *) echo "usage: $0 build|up|doctor|token <who>|gql <who> <query|@file> [vars]|psql [args]|evidence [label]|down" >&2; exit 2 ;;
esac

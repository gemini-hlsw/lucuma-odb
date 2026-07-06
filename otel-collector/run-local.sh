#!/usr/bin/env bash
#
# Build (if needed) and run the OTel collector locally.
#
# Grafana endpoint and token are read from your environment:
#
#   export GRAFANA_OTLP_ENDPOINT=https://otlp-gateway-...grafana.net/otlp
#   export GRAFANA_OTLP_TOKEN=<base64 instanceID:token>
#   ./run-local.sh
#
# Optional overrides (defaults shown):
#   PORT=8888 OTLP_USER=otlp OTLP_PASSWORD=banana ./run-local.sh
#
# Then point ODB at it:
#   export ODB_OTEL_ENDPOINT=http://localhost:8888
#   export ODB_OTEL_KEY=$(echo -n "${OTLP_USER}:${OTLP_PASSWORD}" | base64)

set -euo pipefail

cd "$(dirname "$0")"

IMAGE="${IMAGE:-otel-collector-local}"
CONTAINER="${CONTAINER:-otel-local}"
PORT="${PORT:-8888}"
OTLP_USER="${OTLP_USER:-otlp}"
OTLP_PASSWORD="${OTLP_PASSWORD:-banana}"

require() {
  local name="$1"
  if [[ -z "${!name:-}" ]]; then
    echo "error: \$$name is not set" >&2
    exit 1
  fi
}

require GRAFANA_OTLP_ENDPOINT
require GRAFANA_OTLP_TOKEN

# Build the image if it doesn't exist.
if ! docker image inspect "$IMAGE" >/dev/null 2>&1; then
  echo "==> Building image $IMAGE"
  docker build -t "$IMAGE" .
else
  echo "==> Image $IMAGE already present"
fi

# Generate the htpasswd entry for $OTLP_USER / $OTLP_PASSWORD.
echo "==> Generating htpasswd entry for user '$OTLP_USER'"
HTPASSWD="$(docker run --rm httpd:2.4-alpine htpasswd -nbB "$OTLP_USER" "$OTLP_PASSWORD")"

# Replace any existing container with the same name.
if docker ps -a --format '{{.Names}}' | grep -qx "$CONTAINER"; then
  echo "==> Removing existing container '$CONTAINER'"
  docker rm -f "$CONTAINER" >/dev/null
fi

echo "==> Starting container '$CONTAINER' on port $PORT"
docker run --rm -d --name "$CONTAINER" \
  -p "${PORT}:${PORT}" \
  -e PORT="$PORT" \
  -e OTLP_HTPASSWD="$HTPASSWD" \
  -e GRAFANA_OTLP_ENDPOINT="$GRAFANA_OTLP_ENDPOINT" \
  -e GRAFANA_OTLP_TOKEN="$GRAFANA_OTLP_TOKEN" \
  "$IMAGE" >/dev/null

cat <<EOF

==> Collector running.

ODB env vars:
  export ODB_OTEL_ENDPOINT=http://localhost:${PORT}
  export ODB_OTEL_KEY=$(echo -n "${OTLP_USER}:${OTLP_PASSWORD}" | base64)

Logs:   docker logs -f ${CONTAINER}
Stop:   docker stop ${CONTAINER}
EOF

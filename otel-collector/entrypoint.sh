#!/usr/bin/env bash
#
# nginx + otelcol-contrib in one Heroku dyno.
# nginx terminates HTTP on $PORT and proxies
# authenticated or allow-listed browser traffic
# to the collector on

set -euo pipefail

trap 'kill 0' EXIT

# htpasswd entry as an env var
printf '%s\n' "${OTLP_HTPASSWD}" > /etc/nginx/.htpasswd
chmod 0644 /etc/nginx/.htpasswd

# Update variables on the nginx conf
envsubst '${PORT}' \
  < /etc/nginx/nginx.conf.template \
  > /etc/nginx/nginx.conf

# Launch both nginx and the collector
/usr/local/bin/otelcol-contrib --config=/etc/otel-collector-config.yaml &
nginx -g 'daemon off;' &

# Exit as soon as either child exits; the EXIT trap kills the survivor.
wait -n
exit $?

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

# Comma-separated browser origins that skip auth and get CORS headers.
# Allowlisted origins skip auth entirely, so only include hosts we control.
auth_map=""
cors_map=""
IFS=',' read -ra origins <<< "${OTLP_ALLOWED_ORIGINS:-}"
for origin in "${origins[@]}"; do
  origin="${origin//[[:space:]]/}"
  [[ -z "$origin" ]] && continue
  auth_map+="    \"${origin}\" off;"$'\n'
  cors_map+="    \"${origin}\" \$http_origin;"$'\n'
done
if [[ -z "$auth_map" ]]; then
  echo "warning: OTLP_ALLOWED_ORIGINS is empty, every request requires auth" >&2
fi

cat > /etc/nginx/origins.conf <<ORIGINS
map \$http_origin \$auth_realm {
    default "Restricted";
${auth_map}}

map \$http_origin \$cors_allow_origin {
    default "";
${cors_map}}
ORIGINS

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

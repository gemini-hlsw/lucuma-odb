# Builds a single Heroku container that runs nginx + otelcol-contrib
# side-by-side.

# otel collector image
FROM otel/opentelemetry-collector-contrib:0.110.0 AS collector

# alpine with nginx
FROM alpine:3.20
RUN apk add --no-cache nginx gettext bash \
 && mkdir -p /run/nginx /var/lib/nginx/tmp /var/log/nginx

COPY --from=collector /otelcol-contrib /usr/local/bin/otelcol-contrib
COPY otel-collector-config.yaml /etc/otel-collector-config.yaml
COPY nginx.conf.template        /etc/nginx/nginx.conf.template
COPY entrypoint.sh              /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

CMD ["/usr/local/bin/entrypoint.sh"]

#!/bin/sh
# Renders the sample summary payload in every style inside the built pdf-summary image.
# Usage: pdf-render-check.sh [image] [itc-url]
set -eu

image=${1:-noirlab/pdf-summary-service}
itc=${2:-https://itc-dev.lucuma.xyz/itc}
payload=modules/service/src/test/resources/lucuma/odb/summary/payload-v1.json
styles="gemini-standard gemini-darp gemini-no-investigators gemini-investigators-at-end noirlab-darp chile"

# The payload's attachment URLs point at an unresolvable host; serve stand-ins from inside the container.
docker run --rm -i -v "$PWD/$payload:/check/payload.json:ro" \
  -e ITC="$itc" -e STYLES="$styles" --entrypoint /bin/sh "$image" <<'EOF'
set -eu
py=/opt/pyexplore/bin/python
work=$(mktemp -d)
cd "$work"
$py -c 'from pypdf import PdfWriter
for n in ("science", "team"):
    w = PdfWriter(); w.add_blank_page(612, 792); w.write(n + ".pdf")'
python3 -m http.server 8765 --bind 127.0.0.1 >/dev/null 2>&1 &
sed 's#https://presigned.example.invalid#http://127.0.0.1:8765#' /check/payload.json > payload.json
sleep 1
failed=0
for style in $STYLES; do
  if $py -m pyexplore.pdf.render --payload payload.json --style "$style" \
       --output "out-$style.pdf" --itc-url "$ITC" > "log-$style.txt" 2>&1 \
     && [ "$(head -c 4 "out-$style.pdf")" = "%PDF" ]; then
    echo "ok   $style ($(wc -c < "out-$style.pdf") bytes)"
  else
    echo "FAIL $style"
    tail -20 "log-$style.txt"
    failed=1
  fi
done
exit $failed
EOF

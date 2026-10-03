#!/bin/sh
# Writes changed=true to $GITHUB_OUTPUT when pdf-summary or the pyexplore pin changed since $1.
set -eu

base=$1
changed=false
if [ -z "$base" ] || ! git cat-file -e "$base^{commit}" 2>/dev/null; then
  changed=true
elif git diff --name-only "$base" HEAD | grep -q '^modules/pdf-summary/' \
  || git diff "$base" HEAD -- build.sbt | grep -q '^[-+]lazy val pyexploreRef'; then
  changed=true
fi
echo "changed=$changed" >> "${GITHUB_OUTPUT:-/dev/stdout}"

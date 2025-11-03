#!/bin/bash

set -e

if [ -z $1 ]; then
  echo "🔥 Usage: `basename $0` <path to itc distribution bundle directory>"
  exit 1
fi

if [ -d $1 ]; then
  BUNDLE=$(readlink -f "$1")
  LIB="$(dirname "$(readlink -f "$0")")/modules/service/ocslib"
  mkdir -p "$LIB"
  echo "🔸 Reading from $BUNDLE"
  echo "🔸 Writing to   $LIB"
else
  echo "🔸 Directory not found: $1"
  exit 1
fi

echo "🔸 Capturing OCS build information"
OCS_REPO_PATH="$1"
cd "$OCS_REPO_PATH"

# Capture git information
OCS_GIT_HASH=$(git rev-parse HEAD)
OCS_GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
OCS_GIT_DESCRIBE=$(git describe --always --dirty 2>/dev/null || echo "no-tags")

# Check if working directory is dirty
if ! git diff --quiet; then
  echo "🔥 Error: Working directory has uncommitted changes"
  exit 1
fi

# Check if current commit is local-only
# We want to know if it is on the repo. should we fail if not?
OCS_LOCAL="true"
if git cat-file -e "$OCS_GIT_HASH" 2>/dev/null && git branch -r --contains "$OCS_GIT_HASH" 2>/dev/null | grep -q "origin/"; then
  OCS_LOCAL="false"
fi

cd - > /dev/null

echo "🔸 Removing old library bundles."
touch "$LIB"/dummy.jar
rm "$LIB"/*.jar

echo "🔸 Copying bundles."
cp "$BUNDLE"/argonaut_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-itc-shared_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-itc-web_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-itc_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-json_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-pot_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-shared-skyobject_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-shared-util_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-spmodel-core_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-spmodel-pio_2.11*.jar "$LIB"
cp "$BUNDLE"/edu-gemini-util-skycalc_2.11*.jar "$LIB"
cp "$BUNDLE"/org-jfree_*.jar "$LIB"
cp "$BUNDLE"/scalaz-core_2.11*.jar "$LIB"
cp "$BUNDLE"/squants_2.11*.jar "$LIB"
cp "$BUNDLE"/scala-library-2.11.12.jar "$LIB"

BUILD_INFO_FILE="$LIB/build-info.json"
cat > "$BUILD_INFO_FILE" << EOF
{
  "ocs_git_hash": "$OCS_GIT_HASH",
  "ocs_git_branch": "$OCS_GIT_BRANCH",
  "ocs_git_describe": "$OCS_GIT_DESCRIBE",
  "local": $OCS_LOCAL,
  "build_timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

echo "🔸 Build info saved to $BUILD_INFO_FILE"

# Status reporting
echo "🔸 OCS libraries built from: $OCS_GIT_BRANCH (commit: $OCS_GIT_HASH)"

if [ "$OCS_LOCAL" = "true" ]; then
  echo "⚠️  Local-only changes (not pushed to origin)"
else
  echo "✅ Commit exists on origin remote"
fi

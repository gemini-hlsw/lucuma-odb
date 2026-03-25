#!/bin/bash

set -e

SKIP_GIT_CHECKS=false
while getopts "f" opt; do
  case $opt in
    f) SKIP_GIT_CHECKS=true ;;
    *) echo "🔥 Usage: `basename $0` [-f] <path to itc distribution bundle directory>"
       echo "   -f  Skip git status checks on the OCS directory"
       echo "   Example: `basename $0` /path/to/ocs/app/itc/target/itc/2026A-test.1.1.1/Test/itc/bundle"
       exit 1 ;;
  esac
done
shift $((OPTIND - 1))

if [ -z "$1" ]; then
  echo "🔥 Usage: `basename $0` [-f] <path to itc distribution bundle directory>"
  echo "   -f  Skip git status checks on the OCS directory"
  echo "   Example: `basename $0` /path/to/ocs/app/itc/target/itc/2026A-test.1.1.1/Test/itc/bundle"
  exit 1
fi

if [ ! -d "$1" ]; then
  echo "🔸 Directory not found: $1"
  exit 1
fi

BUNDLE=$(cd "$1" && pwd)
LIB="$(dirname "$(readlink -f "$0")")/service/ocslib"
mkdir -p "$LIB"
echo "🔸 Reading from $BUNDLE"
echo "🔸 Writing to   $LIB"

# Find the git repository root from the bundle directory
echo "🔸 Capturing OCS build information"
cd "$BUNDLE"
OCS_REPO_PATH=$(git rev-parse --show-toplevel 2>/dev/null || echo "")

if [ -z "$OCS_REPO_PATH" ]; then
  echo "🔥 Error: Could not find git repository root from $BUNDLE"
  exit 1
fi

echo "🔸 Found OCS repository at $OCS_REPO_PATH"
cd "$OCS_REPO_PATH"

# Capture git information
OCS_GIT_HASH=$(git rev-parse HEAD)
OCS_GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
OCS_GIT_DESCRIBE=$(git describe --always --dirty 2>/dev/null || echo "no-tags")

OCS_LOCAL="true"
if [ "$SKIP_GIT_CHECKS" = false ]; then
  # Check if working directory is dirty
  if ! git diff --quiet; then
    echo "🔥 Error: Working directory has uncommitted changes"
    exit 1
  fi

  # Check if current commit is local-only
  if git cat-file -e "$OCS_GIT_HASH" 2>/dev/null && git branch -r --contains "$OCS_GIT_HASH" 2>/dev/null | grep -q "origin/"; then
    OCS_LOCAL="false"
  fi
else
  echo "🔸 Skipping git status checks (-f flag)"
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

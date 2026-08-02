#!/usr/bin/env bash
# Publiziert TypeFighter.Wasm und kopiert das _framework-Bundle in die Site.
# Vorher publish loeschen: fingerprinted Altstaende akkumulieren sonst ueber
# die Builds hinweg (Lektion aus pxl-software/build-studio-wasm.sh).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$SCRIPT_DIR/src/TypeFighter.Wasm"
PUBLISH_ROOT="$PROJECT_DIR/bin/Release/net10.0/publish/wwwroot"
TARGET="$SCRIPT_DIR/site/public/wasm"

rm -rf "$PROJECT_DIR/bin/Release/net10.0/publish"
dotnet publish "$PROJECT_DIR" -c Release

rm -rf "$TARGET/_framework"
mkdir -p "$TARGET"
cp -R "$PUBLISH_ROOT/_framework" "$TARGET/_framework"

GIT_SHA="$(git -C "$SCRIPT_DIR" rev-parse --short HEAD 2>/dev/null || echo unknown)"
SIZE_KB="$(du -sk "$TARGET/_framework" | cut -f1)"
BR_KB="$(find "$TARGET/_framework" -name '*.br' -exec du -ck {} + | tail -1 | cut -f1)"
cat > "$TARGET/build-info.json" <<EOF
{ "builtAt": "$(date -u +%Y-%m-%dT%H:%M:%SZ)", "gitSha": "$GIT_SHA", "sizeKb": $SIZE_KB, "brotliKb": $BR_KB }
EOF

echo "Bundle: $((SIZE_KB / 1024)) MB gesamt, davon brotli $((BR_KB / 1024)) MB -> $TARGET/_framework"

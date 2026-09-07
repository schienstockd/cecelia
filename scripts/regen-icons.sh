#!/bin/sh
# Regenerate the desktop-launcher icon rasters from the SVG source of truth.
# See docs/todo/DESKTOP_ICON_PLAN.md → Source-asset generation.
#
# Outputs (committed): frontend/public/icons/cecelia-{16,32,48,64,128,256}.png,
# cecelia.ico, cecelia.icns. Vite copies frontend/public/** into frontend/dist/**
# verbatim, so the release tarball picks them up via the existing frontend/dist
# entry in .github/workflows/release.yml — no allow-list edit needed.
#
# Deps: ImageMagick's `convert` (SVG rasterisation) + Python Pillow >=10 (writes
# multi-res .ico and .icns natively; no `iconutil`, no `png2icns`, no macOS host).

set -eu
cd "$(dirname "$0")/.."

in=frontend/public/feijoa.svg
out=frontend/public/icons
mkdir -p "$out"

# Rasterise the SVG at the largest size once, then downsample. Density 1024 gives
# enough detail for the 256-px PNG and both multi-res containers.
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
convert -background none -density 1024 -resize 1024x1024 "$in" "$tmp/cecelia-1024.png"

for s in 16 32 48 64 128 256; do
  convert "$tmp/cecelia-1024.png" -resize ${s}x${s} "$out/cecelia-${s}.png"
done

python3 - "$tmp/cecelia-1024.png" "$out/cecelia.ico" "$out/cecelia.icns" <<'PY'
import sys
from PIL import Image
src, ico, icns = sys.argv[1:]
img = Image.open(src)
img.save(ico, sizes=[(16, 16), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)])
img.save(icns)
PY

echo "Wrote:"
ls -la "$out"

#!/usr/bin/env bash
# Cold vs warm cost of /api/viewer/slab on one real store — is the brick path disk, decode or pipe bound?
#
#   slab_cache_bench.sh <run-label> <concurrency> [out-dir]
#
# Requests a FIXED list of 64 viewer-shaped bricks (128x128 x all z x all channels, level 0 — the shape
# `brickVolumeRenderer.ts` asks for on a thin-Z store: BRICK_XY=128, brickZ=min(128, nZ), cTo=nC-1),
# one per timepoint so every request touches chunks no other request in the list touches. Talks HTTP/2
# over TLS like the browser does (the server is TLS-only when a cert exists; plain http gets a reset).
#
# Writes <out-dir>/<label>.tsv: label, conc, t, http_code, wall_ms, bytes, server_read_ms (the
# route's own X-Server-Read-Ms — zarr chunk read + blosc/zstd decode + slice, measured inside Julia).
# Wall minus server_read is the pipe: TLS, HTTP/2 framing, socket, curl.
#
# Cold run: drop the page cache FIRST (needs sudo, so it is not done here):
#   sync; echo 3 | sudo tee /proc/sys/vm/drop_caches
set -euo pipefail

label=${1:?run label}; conc=${2:?concurrency}
out=${3:-$(dirname "$0")/slab_cache_results}
mkdir -p "$out"

BASE=${CECELIA_URL:-https://127.0.0.1:8080}
PROJ=${PROJ:-e1Mn6X}; IMG=${IMG:-8eapy6}
NT=181; NZ=31; NC=4; NXB=8; NYB=8; B=128   # 8eapy6: (t,c,z,y,x) = (181,4,31,1024,1024) u16

cfg=$(mktemp); trap 'rm -f "$cfg"' EXIT
for i in $(seq 0 63); do
  t=$(( (i * (NT - 1) + 31) / 63 ))        # 64 timepoints spread evenly over 0..180
  bx=$(( (i * 3) % NXB )); by=$(( (i * 5 + i / NXB) % NYB ))   # brick position varies too
  x0=$((bx * B)); y0=$((by * B))
  printf 'url = "%s/api/viewer/slab?projectUid=%s&imageUid=%s&t=%d&c=0&cTo=%d&z=0&zTo=%d&x=%d&xTo=%d&y=%d&yTo=%d&level=0"\noutput = "/dev/null"\n' \
    "$BASE" "$PROJ" "$IMG" "$t" $((NC - 1)) $((NZ - 1)) "$x0" $((x0 + B - 1)) "$y0" $((y0 + B - 1)) >> "$cfg"
done

fmt="${label}\t${conc}\t%{url}\t%{http_code}\t%{time_total}\t%{size_download}\t%header{x-server-read-ms}\n"
start=$(date +%s.%N)
curl -sk --http2 -Z --parallel-max "$conc" --parallel-immediate -K "$cfg" -w "$fmt" \
  | awk -F'\t' -v OFS='\t' '{ match($3, /[?&]t=[0-9]+/); t = substr($3, RSTART + 3, RLENGTH - 3);
                              print $1, $2, t, $4, $5 * 1000, $6, $7 }' > "$out/$label.tsv"
end=$(date +%s.%N)
echo "$label: $(wc -l < "$out/$label.tsv") requests, run wall $(echo "$end - $start" | bc) s -> $out/$label.tsv"

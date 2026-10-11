#!/usr/bin/env bash
# Drive slab_response_bench.jl: one run of a response mode, prints run wall + median + GC delta.
#
#   slab_response_bench.sh <port> <mode> <concurrency>
#
# Brick modes (view|chunked|vec|pool) request the bench's MODE=scrub list: all 64 bricks of 4
# timepoints. Flat modes (flatview|flatpool) request 8 timepoints x 4 channels of whole volumes.
# HTTP/1.1, like the dev server without a cert. Run each mode once to warm up (JIT + cache), then
# interleave the modes you compare.
set -euo pipefail
port=${1:?port}; m=${2:?mode}; conc=${3:?concurrency}
base="http://127.0.0.1:$port/s"
cfg=$(mktemp); trap 'rm -f "$cfg" "$cfg.t"' EXIT
case $m in
  flat*) for t in 5 25 45 65 85 105 125 145; do for c in 0 1 2 3; do
           printf 'url = "%s?t=%d&bx=%d&m=%s"\noutput = "/dev/null"\n' "$base" $t $c "$m" >> "$cfg"; done; done ;;
  *)     for t in 11 61 111 161; do for by in $(seq 0 7); do for bx in $(seq 0 7); do
           printf 'url = "%s?t=%d&bx=%d&by=%d&m=%s"\noutput = "/dev/null"\n' "$base" $t $bx $by "$m" >> "$cfg"; done; done; done ;;
esac
g0=$(curl -s "$base?gc=1" | sed 's/.*gc_ms=\([0-9.]*\).*/\1/')
s=$(date +%s.%N)
curl -s --http1.1 -Z --parallel-max "$conc" --parallel-immediate -K "$cfg" -w '%{time_total}\n' | sort -n > "$cfg.t"
e=$(date +%s.%N)
g1=$(curl -s "$base?gc=1" | sed 's/.*gc_ms=\([0-9.]*\).*/\1/')
printf '%-9s conc=%-3s wall=%.2fs median=%.0fms gc=%.0fms\n' "$m" "$conc" "$(echo "$e - $s" | bc)" \
  "$(awk '{a[NR]=$1} END {print a[int(NR/2)] * 1000}' "$cfg.t")" "$(echo "$g1 - $g0" | bc)"

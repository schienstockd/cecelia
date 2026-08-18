# Generates the celltrackR golden values asserted by the "diagnostics golden — …" testsets in
# app/test/suite.jl (see app/src/tracking/track_diagnostics.jl for the port).
#
# NOT run by the suite or by CI: it needs an R installation with celltrackR, which neither has. It is
# committed so the numbers stay RE-DERIVABLE — a golden value whose generator is lost is a magic
# constant, and the whole point of these is that they came from the reference implementation.
#
# Run it (celltrackR 1.2.2 lives in the old R version's renv cache on the dev machine):
#   L=~/.cache/R/renv/library/cecelia-*/linux-*/R-4.5/x86_64-pc-linux-gnu \
#     Rscript app/test/golden/celltrackr_golden.R
# or with celltrackR on the default library path:
#   Rscript app/test/golden/celltrackr_golden.R
#
# What each block pins, beyond the arithmetic — these are the CONVENTIONS the port had to match, and
# every one of them was a guess until this ran:
#   • MSD      — `subtrack.length = L` averages over every OVERLAPPING L-step subtrack, and L is a
#                frame lag.
#   • NORMDOT  — `overallNormDot` dots the FIRST and LAST step of an L-step subtrack, so celltrackR's
#                L is our lag L-1; its L=1 is the trivial 1.0.
#   • PLANE    — the distance is measured at the step's START point, and the angle is to the plane
#                (not to its normal).
#   • HOTELLING— `step.spacing` passes `overlap = -step.spacing` to `subtracks`, i.e. a stride of
#                spacing+1; and `dim = c("x","y")` means drift is an XY test by default. The two
#                p-values printed here are the same data with and without decorrelation.
#   • CELLPAIRS— `distanceCells` is the MINIMUM distance over shared timepoints (not the distance
#                between starting points), and `angleCells` is between overall displacement vectors.
if (nzchar(Sys.getenv("L"))) .libPaths(c(Sys.getenv("L"), .libPaths()))
suppressPackageStartupMessages(library(celltrackR))
cat("# celltrackR", as.character(packageVersion("celltrackR")), "\n")

set.seed(42)
mk <- function(n) cbind(t = 0:(n - 1),
                        x = round(cumsum(c(0, rnorm(n - 1, 1, 1))), 1),
                        y = round(cumsum(c(0, rnorm(n - 1, 0, 1))), 1),
                        z = round(cumsum(c(0, rnorm(n - 1, 0, 0.5))) + 10, 1))
tr <- list("1" = mk(8), "2" = mk(8), "3" = mk(8))
X <- as.tracks(tr)

cat("### COORDS (track_id,t,x,y,z)\n")
for (id in names(tr)) { m <- tr[[id]]
  for (i in seq_len(nrow(m)))
    cat(sprintf("%s,%g,%g,%g,%g\n", id, m[i, "t"], m[i, "x"], m[i, "y"], m[i, "z"])) }

cat("### MSD (subtrack.length -> mean squareDisplacement)\n")
msd <- aggregate(X, squareDisplacement, subtrack.length = 1:5)
for (i in seq_len(nrow(msd))) cat(sprintf("%g,%.10f\n", msd[i, 1], msd[i, 2]))

cat("### NORMDOT (subtrack.length -> mean overallNormDot)\n")
nd <- aggregate(X, overallNormDot, subtrack.length = 1:5)
for (i in seq_len(nrow(nd))) cat(sprintf("%g,%.10f\n", nd[i, 1], nd[i, 2]))

cat("### PLANE (per single step: distance,angle to the lower-z plane)\n")
minz <- boundingBox(X)["min", "z"]
steps <- subtracks(X, 1)
ang <- sapply(steps, angleToPlane, p1 = c(0, 0, minz), p2 = c(1, 0, minz), p3 = c(0, 1, minz))
dst <- sapply(steps, distanceToPlane, p1 = c(0, 0, minz), p2 = c(1, 0, minz), p3 = c(0, 1, minz))
cat(sprintf("minz=%.10f\n", minz))
for (i in seq_along(ang)) cat(sprintf("%.10f,%.10f\n", dst[i], ang[i]))

cat("### HOTELLING (xy, the dim default)\n")
cat(sprintf("all_steps_p=%.10g\n", hotellingsTest(X)$p.value))
cat(sprintf("spacing3_p=%.10g\n", hotellingsTest(X, step.spacing = 3)$p.value))

cat("### CELLPAIRS (cell1,cell2,angle,distance)\n")
cp <- analyzeCellPairs(X)
for (i in seq_len(nrow(cp)))
  cat(sprintf("%s,%s,%.10f,%.10f\n", cp$cell1[i], cp$cell2[i], cp$angle[i], cp$dist[i]))

# ── shader_constants.jl — the numbers the viewer, its shaders and the movie renderer share ─────────
#
# Read from `frontend/src/lib/webgpu/shaders/constants.json`, the file the browser (`shaderSource.ts`)
# and the Python host (`wgsl_utils.shader_constants`) read too, so a default here cannot drift from
# the viewer's.

using JSON3

const SHADER_CONSTANTS_PATH = normpath(joinpath(@__DIR__, "..", "..", "frontend", "src", "lib",
                                                "webgpu", "shaders", "constants.json"))
const SHADER_CONSTANTS = JSON3.read(read(SHADER_CONSTANTS_PATH, String))
#: Planes either side of a 2D frame's whose points / tail ends it shows — the viewer's default.
const OVERLAY_Z_TOL = Int(SHADER_CONSTANTS.OVERLAY_Z_TOL)
#: Default fill opacity of a filled (contour 0) mask — the viewer's `LABEL_OPACITY`. A look read off
#: the viewer carries its own value (`maskOpacity`).
const MASK_FILL_OPACITY = Float32(SHADER_CONSTANTS.LABEL_OPACITY)

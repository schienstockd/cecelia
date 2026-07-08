# Exercised during sysimage build so the compiled Makie plot methods get baked in (this is what
# kills the time-to-first-plot tax). Mirrors the AoG+CairoMakie calls real notebooks make.
using CairoMakie, AlgebraOfGraphics, DataFrames
df = DataFrame(x = randn(500), g = rand(["a", "b"], 500))
save(tempname() * ".png", draw(data(df) * mapping(:x) * AlgebraOfGraphics.histogram(bins=30)))
save(tempname() * ".png", draw(data(df) * mapping(:x, color=:g) * AlgebraOfGraphics.density()))

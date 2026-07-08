# Precompile workload for the FULL (release) sysimage: exercises the whole notebook path so Cecelia,
# CeceliaNb and the Makie plot methods all get baked in. Uses a synthetic DataFrame — NO real project
# is assumed (this runs at build/CI time), so it loads + renders rather than reading a .h5ad.
using Cecelia, CeceliaNb, DataFrames, AlgebraOfGraphics, CairoMakie
Cecelia.init_cecelia!()   # bake the config/model load path
df = DataFrame(x = randn(400), g = rand(["a", "b", "c"], 400))
save(tempname() * ".png", nb_hist(df, :x; bins = 30))
save(tempname() * ".png", nb_box(df, :g, :x))
save(tempname() * ".png", nb_scatter(df, :x, :x; color = :g))
nb_count(rename(df, :g => :value_name))

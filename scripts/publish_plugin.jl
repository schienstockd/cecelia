#!/usr/bin/env julia
#
# Publish an example plugin from THIS repo to its own GitHub repo.
#
#     julia scripts/publish_plugin.jl ccia-importTracks [--dry-run]
#     julia scripts/publish_plugin.jl --all
#
# ── Why this script exists ────────────────────────────────────────────────────────────────────────
#
# The plugin lives in two places and one of them has to be the source:
#
#   docs/examples/plugins/<name>/     in this repo — CI loads and RUNS it, so it cannot rot, and a
#                                     framework change lands together with the example that uses it
#                                     (`showIf` and the spec that declares it were one commit)
#   github.com/schienstockd/<name>    what a user installs from Settings → Plugins
#
# The in-repo copy is the source. It has to be: an example that CI does not execute is an example
# that silently stops working, and splitting a framework change across two repos means a window where
# `main` and the published plugin disagree.
#
# The first publications were done BY HAND — `cp -r` into a scratch dir, `git init`, push — which is a
# snapshot with no link back. `ccia-importTracks` was 70 lines behind within days, so the version a
# user could actually install was missing every fix made to the version CI was testing, including the
# ones reported from the screen that morning. Nothing detected it, because nothing was looking.
#
# ── What it does ──────────────────────────────────────────────────────────────────────────────────
#
# Clones the published repo, replaces its tree with the in-repo directory (deleting files that are no
# longer there — a stale template left behind is the same class of bug), commits and pushes. No
# rewriting: `plugin.json`'s `homepage` and the README are committed in-repo and shipped verbatim, so
# there is no generated content to reconcile.
#
# Requires `gh` (auth) and `git`.

const REPO_ROOT  = normpath(joinpath(@__DIR__, ".."))
const EXAMPLES   = joinpath(REPO_ROOT, "docs", "examples", "plugins")
const OWNER      = "schienstockd"

usage() = println("""
usage: julia scripts/publish_plugin.jl <plugin-name> [--dry-run]
       julia scripts/publish_plugin.jl --all [--dry-run]

plugins: $(join(sort([d for d in readdir(EXAMPLES) if isdir(joinpath(EXAMPLES, d))]), ", "))""")

"""Copy `src` over `dst`, removing anything in `dst` that `src` no longer has. `.git` is preserved."""
function mirror!(src::String, dst::String)
    for entry in readdir(dst)
        entry == ".git" && continue
        rm(joinpath(dst, entry); recursive = true, force = true)
    end
    for entry in readdir(src)
        cp(joinpath(src, entry), joinpath(dst, entry))
    end
end

function publish(name::String; dry_run::Bool = false)
    src = joinpath(EXAMPLES, name)
    isdir(src) || (@error "No such example plugin" name src; return false)
    isfile(joinpath(src, "plugin.json")) ||
        (@error "Not a plugin: no plugin.json" src; return false)
    # A published plugin with no README is a repo whose landing page explains nothing. Cheap to
    # require, and the in-repo copy is the only place it can be edited.
    isfile(joinpath(src, "README.md")) ||
        (@error "Refusing to publish without a README.md" src; return false)

    url = "https://github.com/$OWNER/$name"
    tmp = mktempdir()
    clone = joinpath(tmp, name)
    # `gh repo clone`, not `git clone <https url>`: the https remote has no credentials in a fresh
    # temp dir, so the push failed with a bare exit 128 AFTER the tree had been rewritten. gh applies
    # its own auth and picks the protocol the user is set up for.
    try
        run(pipeline(`gh repo clone $OWNER/$name $clone -- --quiet`; stdout = devnull, stderr = devnull))
    catch
        @error "Could not clone — does the repo exist, and is `gh` authenticated?" url
        return false
    end

    mirror!(src, clone)
    # `git status --porcelain` is empty when the published tree already matches, which is the common
    # case and must be a no-op rather than an empty commit.
    changed = strip(read(Cmd(`git status --porcelain`; dir = clone), String))
    if isempty(changed)
        println("  $name: already up to date")
        return true
    end
    println("  $name: changes to publish\n" * join("    " .* split(changed, "\n"), "\n"))
    if dry_run
        println("  (dry run — nothing pushed)")
        return true
    end

    sha = strip(read(Cmd(`git rev-parse --short HEAD`; dir = REPO_ROOT), String))
    msg = """
    Sync from cecelia@$sha

    Published from docs/examples/plugins/$name in schienstockd/cecelia, which is
    the source: CI loads and runs it there, so a framework change and the example
    that uses it land together.
    """
    run(Cmd(`git add -A`; dir = clone))
    run(pipeline(Cmd(`git commit -q -m $msg`; dir = clone); stdout = devnull))
    run(pipeline(Cmd(`git push -q`; dir = clone); stdout = devnull, stderr = devnull))
    println("  $name: pushed → $url")
    true
end

args = copy(ARGS)
dry  = "--dry-run" in args
filter!(a -> a != "--dry-run", args)

if isempty(args) || "--help" in args || "-h" in args
    usage()
elseif "--all" in args
    names = sort([d for d in readdir(EXAMPLES) if isdir(joinpath(EXAMPLES, d))])
    all(publish(n; dry_run = dry) for n in names) || exit(1)
else
    publish(args[1]; dry_run = dry) || exit(1)
end

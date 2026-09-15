# ── Self-signed dev TLS cert ─────────────────────────────────────────────────────
#
# HTTP/2 in the browser requires TLS — Chromium refuses cleartext h2 (h2c) by policy. This
# gets a working https:// dev server going by shelling out to the system `openssl` binary.
# (OpenSSL_jll — stdlib on Julia 1.12 — only exposes `libssl`/`libcrypto`, not the CLI.
# System openssl is present by default on Linux and macOS and via git-for-windows on
# Windows; when it isn't, `ensure_dev_cert` returns nothing and the server falls back to
# HTTP/1.1 cleanly.)
#
# One-time-per-install cert: `<config_dir>/tls/{cert.pem,key.pem}`. Regenerated only if the
# key file is missing (rotate by deleting both). Users click through Chrome's "Not secure"
# once and the browser remembers the exception. mkcert would give a warning-free experience
# at the cost of bundling a third-party binary and writing the OS trust store — not worth
# the packaging surface for a research tool; revisit if the click-through actually bites.
# See `docs/todo/WEBGPU_UPLOAD_PATH_PLAN.md` → U4.

const TLS_SUBDIR = "tls"
const TLS_CERT_NAME = "cert.pem"
const TLS_KEY_NAME  = "key.pem"

"""
    tls_paths([dev_dir]) -> (cert::String, key::String)

Where the dev cert and key live inside `config_dir`. Pure path computation — does not
create anything. Use `ensure_dev_cert` before opening either.
"""
function tls_paths(dev_dir::Union{String,Nothing} = nothing)
    dir = joinpath(config_dir(dev_dir), TLS_SUBDIR)
    (joinpath(dir, TLS_CERT_NAME), joinpath(dir, TLS_KEY_NAME))
end

"""
    ensure_dev_cert([dev_dir]) -> (cert::String, key::String) | nothing

Return the cert/key pair, generating a fresh self-signed cert on first run. Returns
`nothing` on failure — the caller then falls back to HTTP/1.1 rather than refusing to
start. Never throws: a broken openssl binary on some fresh platform must not stop the
server from serving anything at all.

Certificate is a `localhost` cert with SANs for `localhost`, `127.0.0.1`, and `::1` —
covers the three loopback names any local browser resolves. 365 days validity: short
enough that a stolen key ages out on its own, long enough that regeneration is not a
constant chore. 2048-bit RSA: matches every browser's baseline without needing an ECDSA
compatibility check.
"""
function ensure_dev_cert(dev_dir::Union{String,Nothing} = nothing)::Union{Tuple{String,String},Nothing}
    cert_path, key_path = tls_paths(dev_dir)
    # Idempotent: if both files exist, hand them back. A partial state (cert but no key,
    # or the other way round) forces a full regenerate — we can't recover the private key
    # from a public cert and we can't recover the cert from a key alone.
    if isfile(cert_path) && isfile(key_path)
        return (cert_path, key_path)
    end
    openssl_bin = Sys.which("openssl")
    if openssl_bin === nothing
        @warn "TLS: system `openssl` not found on PATH — HTTPS will not start, falling back to HTTP/1.1"
        return nothing
    end
    tls_dir = dirname(cert_path)
    try
        mkpath(tls_dir)
    catch e
        @warn "TLS cert dir create failed" dir=tls_dir exception=e
        return nothing
    end
    # openssl req flags:
    #  -x509                self-signed (not a CSR)
    #  -newkey rsa:2048     generate a fresh 2048-bit RSA key
    #  -nodes               no passphrase on the key (Julia/Reseau can't prompt for one)
    #  -days 365            validity window
    #  -subj /CN=localhost  no interactive prompts
    #  -addext subjectAltName=…  SANs so `https://localhost`, `https://127.0.0.1`,
    #                            `https://[::1]` all validate against the same cert
    cmd = `$(openssl_bin) req -x509 -newkey rsa:2048 -nodes -days 365
           -subj /CN=localhost
           -addext subjectAltName=DNS:localhost,IP:127.0.0.1,IP:::1
           -keyout $(key_path) -out $(cert_path)`
    try
        # Redirect stdout+stderr to devnull — openssl is chatty on success and users don't
        # need the "-----BEGIN CERTIFICATE REQUEST-----" chatter in the server log.
        run(pipeline(cmd, stdout=devnull, stderr=devnull))
    catch e
        @warn "TLS cert generation failed — HTTPS will not start, falling back to HTTP/1.1" exception=e
        # Clean up any half-written file so the next attempt sees consistent state.
        try; isfile(cert_path) && rm(cert_path); catch; end
        try; isfile(key_path)  && rm(key_path);  catch; end
        return nothing
    end
    if !(isfile(cert_path) && isfile(key_path))
        @warn "TLS cert generation returned OK but files missing" cert=cert_path key=key_path
        return nothing
    end
    @info "TLS: generated self-signed dev cert" cert=cert_path key=key_path days=365
    (cert_path, key_path)
end

const UID_LENGTH = 6
const UID_CHARS  = collect("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

gen_uid(n::Int=UID_LENGTH) = String(rand(UID_CHARS, n))

function _dir_bytes(path::String)::Int
    if Sys.isunix()
        try; parse(Int, split(readchomp(`du -sb $path`))[1]); catch; 0; end
    else
        try
            total = 0
            for (root, _, files) in walkdir(path)
                for f in files
                    try; total += filesize(joinpath(root, f)); catch; end
                end
            end
            total
        catch; 0; end
    end
end

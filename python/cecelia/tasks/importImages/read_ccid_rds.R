#!/usr/bin/env Rscript
# Read one or more legacy cecelia `ccid.rds` files and emit uid-prefixed, tab-delimited records on
# stdout for the Python migrator/scanner. Base R only — NO package deps (works with system R or the
# old cecelia renv R). Passing many paths in one call keeps a whole-project scan to a single process.
#
# ccid.rds is a gzipped R-serialized *list* (not an R6 object): UID, CciaClass, CciaName, CciaType,
# CciaAttr (named list), CciaMeta (named list of versioned app fields). Versioned fields carry the
# active variant as an R attribute: attr(field, "default") = "<value_name>".
#
# Output (one record per line; list items get their own row → no in-field delimiters):
#   <uid><TAB>META<TAB>class|name|type|orifilepath<TAB><value>
#   <uid><TAB>ATTR<TAB><key><TAB><value>
#   <uid><TAB>ACTIVE<TAB>channels|filepath|labels|labelprops<TAB><value_name>
#   <uid><TAB>CHANNEL<TAB><value_name><TAB><idx><TAB><name>
#   <uid><TAB>FILEPATH<TAB><value_name><TAB><filename>
#   <uid><TAB>LABELS<TAB><value_name><TAB><filename>
#   <uid><TAB>LABELPROPS<TAB><value_name><TAB><filename>
# Usage: Rscript read_ccid_rds.R <ccid1.rds> [<ccid2.rds> ...]

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) quit(save = "no", status = 0)   # nothing to read → emit nothing, exit clean

read_one <- function(path) {
  x <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(x)) return(invisible())
  uid <- as.character(x$UID)
  m <- x$CciaMeta
  emit <- function(...) cat(paste(c(uid, ...), collapse = "\t"), "\n", sep = "")

  emit("META", "class", as.character(x$CciaClass))
  emit("META", "name",  as.character(x$CciaName))
  emit("META", "type",  as.character(x$CciaType))
  if (!is.null(m$oriFilepath)) emit("META", "orifilepath", as.character(m$oriFilepath)[[1]])

  if (!is.null(x$CciaAttr)) for (k in names(x$CciaAttr)) {
    v <- x$CciaAttr[[k]]
    emit("ATTR", k, if (length(v)) as.character(v)[[1]] else "")
  }

  versioned <- function(field, tag, row_fn) {
    if (is.null(field)) return(invisible())
    act <- attr(field, "default")
    if (!is.null(act)) emit("ACTIVE", tag, as.character(act))
    for (vn in names(field)) row_fn(vn, field[[vn]])
  }
  versioned(m$imChannelNames, "channels", function(vn, v) {
    nm <- unname(as.character(v)); for (i in seq_along(nm)) emit("CHANNEL", vn, i - 1L, nm[[i]])
  })
  versioned(m$imFilepath,           "filepath",   function(vn, v) emit("FILEPATH",   vn, as.character(v)[[1]]))
  versioned(m$imLabelPropsFilepath, "labelprops", function(vn, v) emit("LABELPROPS", vn, as.character(v)[[1]]))
  versioned(m$imLabelsFilepath,     "labels",     function(vn, v) for (fn in as.character(v)) emit("LABELS", vn, fn))
}

for (p in args) read_one(p)

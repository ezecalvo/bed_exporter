write_bed <- function(x, path, col_names = FALSE) {
  if (missing(x) || missing(path)) {
    stop("Both `x` (the data) and `path` (the output file) are required.")
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (!ncol(x) %in% c(3L, 6L)) {
    stop(sprintf("BED must have 3 (BED3) or 6 (BED6) columns; got %d.", ncol(x)))
  }
  if (nrow(x) == 0L) {
    stop("Input has 0 rows — nothing to write.")
  }
  
  start <- x[[2]]
  end   <- x[[3]]
  for (info in list(list(idx = 2L, name = "start", v = start),
                    list(idx = 3L, name = "end",   v = end))) {
    v <- info$v
    if (!is.numeric(v)) {
      stop(sprintf("Column %d (%s) must be numeric; got class '%s'.",
                   info$idx, info$name, class(v)[1]))
    }
    if (anyNA(v)) {
      stop(sprintf("Column %d (%s) contains NA values.", info$idx, info$name))
    }
    if (any(v != floor(v))) {
      bad <- which(v != floor(v))
      stop(sprintf("Column %d (%s) must be whole integers; row(s) %s are not.",
                   info$idx, info$name,
                   paste(utils::head(bad, 5), collapse = ", ")))
    }
    if (any(v < 0)) {
      stop(sprintf("Column %d (%s) has negative coordinate(s) — BED is 0-based non-negative.",
                   info$idx, info$name))
    }
  }
  
  ##Check start<end
  bad <- which(!(start < end))
  if (length(bad) > 0L) {
    stop(sprintf("start must be < end. Violated on %d row(s): %s%s",
                 length(bad),
                 paste(utils::head(bad, 5), collapse = ", "),
                 if (length(bad) > 5) ", ..." else ""))
  }
  if (ncol(x) == 6L) {
    strand <- as.character(x[[6]])
    if (!all(strand %in% c("+", "-", ".", NA))) {
      warning("Column 6 (strand) has values outside {+, -, .}; writing as-is.")
    }
  }
  out_dir <- dirname(path)
  if (!dir.exists(out_dir)) {
    stop(sprintf("Output directory does not exist: %s", out_dir))
  }
  if (!grepl("\\.bed(\\.gz)?$", path, ignore.case = TRUE)) {
    warning("Path does not end in '.bed'; writing anyway.")
  }
  x[[2]] <- format(as.integer(round(start)), scientific = FALSE, trim = TRUE)
  x[[3]] <- format(as.integer(round(end)),   scientific = FALSE, trim = TRUE)
  utils::write.table(
    x, file = path,
    sep = "\t", quote = FALSE,
    row.names = FALSE, col.names = col_names
  )
  
  message(sprintf("Wrote %d features to %s", nrow(x), path))
  invisible(path)
}
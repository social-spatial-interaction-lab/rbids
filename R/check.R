#' @keywords Internal
.single_character_check <- function(value, name) {
  if (!is.character(value) || length(value) != 1) {
    abort(paste(name, "must be a single character string"))
  }
}

#' @keywords Internal
.bool_check <- function(value, name) {
  if (!is.logical(value) || length(value) != 1) {
    abort(paste(name, "must be a bool"))
  }
}

#' @keywords Internal
.is_participants_exists <- function(bids) {
  file_path <- file.path(bids$root, "participants.tsv")
  if (!file.exists(file_path)) {
    abort("`participants.tsv` does not exist.")
  } else {
    file_path
  }
}

#' @keywords Internal
.check_root_dir <- function(root, readonly) {
  if (!dir.exists(root)) {
    if (readonly) {
      stop(sprintf("Root directory `%s` does not exist.", root))
    } else {
      warning(
        sprintf("Directory `%s` does not exist. Creating directory...", root)
      )
      dir.create(root, recursive = TRUE)
    }
  }
}

#' @keywords Internal
.check_empty_motion_files <- function(tsv_files) {
  empty_files <- character(0)
  if (nrow(tsv_files) > 0) {
    message("\nChecking motion files...")
    pb <- txtProgressBar(min = 0, max = nrow(tsv_files), style = 3)
    for (i in seq_len(nrow(tsv_files))) {
      file <- tsv_files$file_path[i]
      file_size <- file.size(file)
      if (file_size == 0) {
        empty_files <- c(empty_files, file)
        next
      }
      content <- readr::read_tsv(file, show_col_types = FALSE)
      if (nrow(content) == 0) {
        empty_files <- c(empty_files, file)
      }
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  if (length(empty_files) > 0) {
    warning(
      sprintf(
        "Found %d empty motion data files.\n",
        length(empty_files)
      ),
      paste("  -", empty_files, collapse = "\n"),
      call. = FALSE
    )
  }
  empty_files
}

#' @keywords Internal
.check_valid_tsv_files <- function(tsv_files) {
  if (nrow(tsv_files) == 0) {
    stop("No valid TSV files found in dataset.")
  }
}

library(dplyr)

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

#' This function checks if a *motion.tsv file has
#' corresponding *motion.json, *channels.tsv and *channels.json files.
#' @keywords Internal
.check_missing_meta_files <- function(index_data) {
  motion_tsv_files <- index_data %>%
    dplyr::filter(.data$datatype == "motion", .data$suffix == "tsv")

  missing_files <- list(
    motion_json = character(0),
    channels_json = character(0),
    channels_tsv = character(0)
  )

  for (i in seq_len(nrow(motion_tsv_files))) {
    current_row <- motion_tsv_files[i, ]
    base_conditions <- rlang::quo(
      (is.na(.data$subject) & is.na(current_row$subject) |
        .data$subject == current_row$subject) &
        (is.na(.data$session) & is.na(current_row$session) |
          .data$session == current_row$session) &
        (is.na(.data$task) & is.na(current_row$task) |
          .data$task == current_row$task) &
        (is.na(.data$tracksys) & is.na(current_row$tracksys) |
          .data$tracksys == current_row$tracksys) &
        (is.na(.data$acq) & is.na(current_row$acq) |
          .data$acq == current_row$acq) &
        (is.na(.data$run) & is.na(current_row$run) |
          .data$run == current_row$run)
    )

    checks <- list(
      motion_json = list(datatype = "motion", suffix = "json"),
      channels_json = list(datatype = "channels", suffix = "json"),
      channels_tsv = list(datatype = "channels", suffix = "tsv")
    )

    for (check_type in names(checks)) {
      matching_files <- index_data %>%
        dplyr::filter(
          !!base_conditions,
          .data$datatype == checks[[check_type]]$datatype,
          .data$suffix == checks[[check_type]]$suffix
        )

      if (nrow(matching_files) != 1) {
        missing_files[[check_type]] <- c(
          missing_files[[check_type]],
          current_row$file_path
        )
      }
    }
  }
  file_type_names <- c(
    motion_json = "*motion.json",
    channels_json = "*channels.json",
    channels_tsv = "*channels.tsv"
  )

  for (type in names(missing_files)) {
    if (length(missing_files[[type]]) > 0) {
      warning(
        sprintf(
          "Found %d motion TSV files without corresponding %s.\n",
          length(missing_files[[type]]),
          file_type_names[[type]]
        ),
        paste("  -", missing_files[[type]], collapse = "\n"),
        call. = FALSE
      )
    }
  }

  missing_files
}

#' @keywords Internal
.check_and_filter_empty_files <- function(files_subset) {
  empty_files <- files_subset %>%
    dplyr::filter(.data$is_empty == TRUE)

  if (nrow(empty_files) > 0) {
    message(sprintf(
      "Found %d empty files, automatically filtering them out.",
      nrow(empty_files)
    ))

    return(files_subset %>% dplyr::filter(.data$is_empty == FALSE))
  }

  files_subset
}

#' Check if merge attributes are valid
#' @param merge_attr A character vector specifying which attributes to merge.
#' @return A character vector containing only valid merge attributes.
#' @keywords Internal
.check_merge_attributes <- function(merge_attr) {
  valid_attrs <- c("subject", "session", "task", "tracksys", "acq", "run")

  invalid_attrs <- setdiff(merge_attr, valid_attrs)
  if (length(invalid_attrs) > 0) {
    warning(
      sprintf(
        "Invalid merge attributes: %s. These will be ignored. You can only merge the
        following attributes: subject, session, task, tracksys, acq, run",
        paste(invalid_attrs, collapse = ", ")
      ),
      call. = FALSE
    )
    return(intersect(merge_attr, valid_attrs))
  }

  return(merge_attr)
}

#' @keywords Internal
.check_type_attributes <- function(type_check) {
  if (!type_check %in% c("auto", "strict")) {
    stop("type_check must be either 'auto' or 'strict'", call. = FALSE)
  }
}

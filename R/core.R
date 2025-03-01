#' BIDS Dataset Handler
#'
#' A class for managing and accessing BIDS(Brain Imaging Data Structure) motions data.
#' See [BIDS Motion](https://bids-specification.readthedocs.io/en/stable/modality-specific-files/motion.html) for more details.
#' @param root A character string. The root directory of the BIDS dataset.
#' @param readonly Logical. Default is TRUE.
#'
#' @field root A character string. The root directory of the BIDS dataset.
#' @field index A tibble containing the BIDS dataset index.
#' @field readonly Logical. Default is TRUE.
#' @export
Bids <- R6Class( # nolint: object_name_linter, cyclocomp_linter.
  "Bids",
  public = list(
    root = NULL,
    index = NULL,
    readonly = NULL,
    #' @description
    #' Initialize the Bids object.
    #' @param root A character string. The root directory of the BIDS dataset.
    #' @param readonly Logical. Default is TRUE.
    initialize = function(root, readonly = TRUE) {
      .single_character_check(root, "root")
      .bool_check(readonly, "readonly")
      .check_root_dir(root, readonly)

      self$root <- fs::path_abs(root)
      self$readonly <- readonly
      private$.build_index()
      self$print()
    },
    #' @description
    #' Load the participants.tsv file.
    #' @return A tibble containing the participants.tsv file.
    load_participant = function() {
      file_path <- .is_participants_exists(self)
      readr::read_tsv(file_path, show_col_types = FALSE)
    },
    #' @description
    #' Load the motion files. E.g. *motion.tsv.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @param empty_check Logical. If TRUE (default), automatically filters out empty.
    #' @param merge_attr A character vector specifying which attributes to merge.
    #' Default: c("subject").
    #' @param type_check Character. Determines how to handle type conflicts across
    #' files. Use "strict" (default) to abort on conflicts, or "auto" to automatically
    #' resolve conflicts.
    #' @return A tibble containing the motion files with subject and other index
    #' columns. e.g. session, task, tracksys, acq, run.
    load_motion = function(..., empty_check = TRUE, merge_attr = "subject",
                           type_check = "strict") {
      .bool_check(empty_check, "empty_check")
      motion_files <- self$index %>%
        dplyr::filter(datatype == "motion", suffix == "tsv", ...)
      if (empty_check) {
        motion_files <- .check_and_filter_empty_files(motion_files)
      }
      private$.merge_with_index(motion_files,
        merge_attr = merge_attr,
        type_check = type_check
      )
    },
    #' @description
    #' Load the logs files. E.g. *events.tsv.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @param merge_attr A character vector specifying which attributes to merge.
    #' Default: c("subject", "task").
    #' @param type_check Character. Determines how to handle type conflicts across
    #' files. Use "strict" (default) to abort on conflicts, or "auto" to automatically
    #' resolve conflicts.
    #' @return A tibble containing the logs files with subject and other index
    #' columns. e.g. session, task, tracksys, acq, run.
    load_logs = function(..., merge_attr = c("subject", "task"),
                         type_check = "strict") {
      logs_files <- self$index %>%
        dplyr::filter(datatype == "events", suffix == "tsv", ...)
      private$.merge_with_index(
        logs_files,
        merge_attr = merge_attr,
        type_check = type_check
      )
    },
    #' @description
    #' Load the session files. E.g. *sessions.tsv.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @param merge_attr A character vector specifying which attributes to merge.
    #' Default: c("subject", "session").
    #' @param type_check Character. Determines how to handle type conflicts across
    #' files. Use "strict" (default) to abort on conflicts, or "auto" to automatically
    #' resolve conflicts.
    load_session = function(..., merge_attr = c("subject", "session"),
                            type_check = "strict") {
      session_files <- self$index %>%
        dplyr::filter(datatype == "session", suffix == "tsv", ...)
      private$.merge_with_index(
        session_files,
        merge_attr = merge_attr,
        type_check = type_check
      )
    },
    #' @description
    #' Load files from a vector of file paths.
    #' @param file_paths A character vector containing file paths to load.
    #' @param empty_check Logical. If TRUE (default), automatically filters out empty.
    #' @param merge_attr A character vector specifying which attributes to merge.
    #' Default: c("subject").
    #' @param type_check Character. Determines how to handle type conflicts across
    #' files. Use "strict" (default) to abort on conflicts, or "auto" to automatically
    #' resolve conflicts.
    #' @return A tibble containing the files with subject.
    load_files = function(
        file_paths,
        empty_check = TRUE, merge_attr = "subject",
        type_check = "strict") {
      if (length(file_paths) == 0) {
        warning("No files provided.", call. = FALSE)
        return(NULL)
      }
      .bool_check(empty_check, "empty_check")
      file_subset <- self$index %>%
        dplyr::filter(file_path %in% file_paths)
      if (nrow(file_subset) == 0) {
        warning("None of the provided file paths match the index. Are you sure the file
        paths are correct?",
          call. = FALSE
        )
        return(NULL)
      }
      if (empty_check) {
        file_subset <- .check_and_filter_empty_files(file_subset)
      }
      private$.merge_with_index(
        file_subset,
        merge_attr = merge_attr, type_check = type_check
      )
    },
    #' @description
    #' List the motion files.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @return A character vector containing the motion files.
    list_motion = function(...) {
      result <- self$index %>%
        dplyr::filter(datatype == "motion", suffix == "tsv", ...) %>%
        dplyr::pull(file_path)
      return(invisible(result))
    },
    #' @description
    #' List the logs files.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @return A character vector containing the logs files.
    list_logs = function(...) {
      result <- self$index %>%
        dplyr::filter(datatype == "events", suffix == "tsv", ...) %>%
        dplyr::pull(file_path)
      return(invisible(result))
    },
    #' @description
    #' List the session files.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @return A character vector containing the session files.
    list_session = function(...) {
      result <- self$index %>%
        dplyr::filter(datatype == "sessions", suffix == "tsv", ...) %>%
        dplyr::pull(file_path)
      return(invisible(result))
    },
    #' @description
    #' Print the BIDS dataset summary.
    print = function() {
      cat("\nBIDS Dataset Summary\n\n")

      cat(sprintf("%-20s %s\n", "Root:", self$root))
      cat(sprintf("%-20s %d\n", "Files:", nrow(self$index)))

      subject_count <- length(unique(self$index$subject))
      subjects <- paste(
        paste(head(sort(unique(self$index$subject)), 5), collapse = ", "),
        "..."
      )
      sessions <- private$.format_attribute_values(self$index$session)
      tasks <- private$.format_attribute_values(self$index$task)
      datatypes <- private$.format_attribute_values(self$index$datatype)
      tracksys <- private$.format_attribute_values(self$index$tracksys)
      acq <- private$.format_attribute_values(self$index$acq)
      run <- private$.format_attribute_values(self$index$run)

      cat(sprintf("%-20s %d\n", "Total Subjects:", subject_count))
      cat(sprintf("%-20s %s\n", "Subjects:", subjects))
      cat(sprintf("%-20s %s\n", "Sessions:", sessions))
      cat(sprintf("%-20s %s\n", "Tasks:", tasks))
      cat(sprintf("%-20s %s\n", "Datatypes:", datatypes))
      cat(sprintf("%-20s %s\n", "Tracksys:", tracksys))
      cat(sprintf("%-20s %s\n", "Acquisition:", acq))
      cat(sprintf("%-20s %s\n\n", "Run:", run))
    }
  ),
  private = list(
    .merge_with_index = function(file_subset, merge_attr, type_check = "strict") {
      if (nrow(file_subset) == 0) {
        warning("No files found.", call. = FALSE)
        return(NULL)
      }
      merge_attr <- .check_merge_attributes(merge_attr)

      # Validate type_check parameter
      if (!type_check %in% c("auto", "strict")) {
        stop("type_check must be either 'auto' or 'strict'", call. = FALSE)
      }

      message(sprintf("Filtered %d files, loading...\n", nrow(file_subset)))
      flush.console()

      # Step 1: Pre-scan files to detect column types
      message("Checking file schemas...")
      flush.console()
      pb_check <- txtProgressBar(min = 0, max = nrow(file_subset), style = 3)
      file_schemas <- vector("list", nrow(file_subset))

      for (i in seq_len(nrow(file_subset))) {
        file_path <- file_subset$file_path[i]
        # Read just one row to get column types
        df_sample <- readr::read_tsv(file_path, n_max = 5, show_col_types = FALSE)
        file_schemas[[i]] <- list(
          path = file_path,
          columns = colnames(df_sample),
          types = sapply(df_sample, class)
        )
        setTxtProgressBar(pb_check, i)
      }
      close(pb_check)

      # Step 2: Detect type conflicts
      conflicts <- private$.detect_type_conflicts(file_schemas)

      # Step 3: Handle conflicts based on type_check mode
      if (length(conflicts) > 0) {
        if (type_check == "strict") {
          private$.report_type_conflicts(conflicts, file_schemas, mode = "strict")
          stop("Type conflicts detected. Aborting merge operation.", call. = FALSE)
        } else {
          # Auto mode - resolve conflicts
          resolved_types <- private$.resolve_type_conflicts(conflicts, file_schemas)
          private$.report_type_conflicts(conflicts, file_schemas,
            mode = "auto",
            resolved_types = resolved_types
          )
        }
      } else {
        message("No type conflicts detected.")
      }

      # Step 4: Load and merge data with appropriate type conversions
      message(sprintf("Loading and merging %d files...\n", nrow(file_subset)))
      flush.console()

      pb <- txtProgressBar(min = 0, max = nrow(file_subset), style = 3)
      data_list <- vector("list", nrow(file_subset))

      resolved_types <- if (exists("resolved_types")) resolved_types else list()

      for (i in seq_len(nrow(file_subset))) {
        file_path <- file_subset$file_path[i]
        df <- readr::read_tsv(file_path, show_col_types = FALSE)

        # Apply type conversions if in auto mode and conflicts were resolved
        if (type_check == "auto" && length(resolved_types) > 0) {
          df <- private$.convert_column_types(df, resolved_types)
        }

        for (attr in merge_attr) {
          df[[attr]] <- file_subset[[attr]][i]
        }

        data_list[[i]] <- df
        setTxtProgressBar(pb, i)
      }
      close(pb)

      dplyr::bind_rows(data_list)
    },
    .detect_type_conflicts = function(file_schemas) {
      all_columns <- unique(unlist(lapply(file_schemas, function(x) x$columns)))

      message("Detecting type conflicts...")
      flush.console()
      pb <- txtProgressBar(min = 0, max = length(all_columns), style = 3)

      conflicts <- list()
      conflict_count <- 0

      for (i in seq_along(all_columns)) {
        col <- all_columns[i]
        col_types <- list()

        # Collect all types for this column across files
        for (j in seq_along(file_schemas)) {
          schema <- file_schemas[[j]]
          col_idx <- match(col, schema$columns)

          if (!is.na(col_idx)) {
            col_type <- schema$types[col_idx]
            file_path <- schema$path

            if (is.null(col_types[[col_type]])) {
              col_types[[col_type]] <- c(file_path)
            } else {
              col_types[[col_type]] <- c(col_types[[col_type]], file_path)
            }
          }
        }

        # If more than one type exists for this column, it's a conflict
        if (length(col_types) > 1) {
          conflict_count <- conflict_count + 1
          conflicts[[conflict_count]] <- list(
            column = col,
            types = col_types
          )
        }

        setTxtProgressBar(pb, i)
      }
      close(pb)

      return(conflicts)
    },

    # Resolve type conflicts in auto mode
    .resolve_type_conflicts = function(conflicts, file_schemas) {
      message("Resolving type conflicts...")
      flush.console()
      pb <- txtProgressBar(min = 0, max = length(conflicts), style = 3)

      resolved_types <- list()

      for (i in seq_along(conflicts)) {
        conflict <- conflicts[[i]]
        col <- conflict$column
        types <- names(conflict$types)

        # Decision logic for type resolution
        # Try to convert to numeric if possible, otherwise character
        can_be_numeric <- TRUE

        # Check if all types can be converted to numeric
        for (type in types) {
          if (!(type %in% c("numeric", "integer", "double"))) {
            # Check sample values from files with this type
            for (file_path in conflict$types[[type]]) {
              df_sample <- readr::read_tsv(
                file_path,
                n_max = 10, show_col_types = FALSE
              )
              values <- df_sample[[col]]

              # Try to convert to numeric
              suppressWarnings({
                converted <- as.numeric(values)
              })

              # If any conversion resulted in NA that wasn't NA before, can't convert
              if (any(is.na(converted) & !is.na(values))) {
                can_be_numeric <- FALSE
                break
              }
            }
          }

          if (!can_be_numeric) break
        }

        # Determine the target type
        target_type <- if (can_be_numeric) "numeric" else "character"

        resolved_types[[col]] <- list(
          target_type = target_type,
          original_types = types
        )

        setTxtProgressBar(pb, i)
      }
      close(pb)

      return(resolved_types)
    },

    # Report type conflicts
    .report_type_conflicts = function(conflicts,
                                      file_schemas,
                                      mode,
                                      resolved_types = NULL) {
      if (mode == "strict") {
        message("\n", rep("=", 80), "\n")
        message("  TYPE CONFLICTS DETECTED (STRICT MODE)\n", rep("-", 80))

        for (i in seq_along(conflicts)) {
          conflict <- conflicts[[i]]
          message("\n  CONFLICT #", i, ": Column '", conflict$column, "'")
          message("  ", rep("-", 60))

          for (type_name in names(conflict$types)) {
            file_count <- length(conflict$types[[type_name]])
            file_examples <- paste(basename(head(conflict$types[[type_name]], 3)),
              collapse = ", "
            )

            if (file_count > 3) {
              file_examples <- paste0(
                file_examples,
                ", ... (", file_count - 3, " more)"
              )
            }

            message("    Type: '", type_name, "'")
            message("    Files (", file_count, "): ", file_examples)
            message("")
          }
        }
        message(rep("=", 80), "\n")
      } else if (mode == "auto" && !is.null(resolved_types)) {
        message("\n", rep("=", 80), "\n")
        message("  TYPE CONFLICTS AUTOMATICALLY RESOLVED\n", rep("-", 80))

        for (i in seq_along(names(resolved_types))) {
          col_name <- names(resolved_types)[i]
          resolution <- resolved_types[[col_name]]

          message("\n  RESOLUTION #", i, ": Column '", col_name, "'")
          message("  ", rep("-", 60))
          message(
            "    Original types: [",
            paste(resolution$original_types, collapse = ", "), "]"
          )
          message("    Converted to:   '", resolution$target_type, "'")
          message("")
        }
        message(rep("=", 80), "\n")
      }
    },

    # Convert column types according to resolved_types
    .convert_column_types = function(df, resolved_types) {
      for (col_name in names(resolved_types)) {
        if (col_name %in% colnames(df)) {
          target_type <- resolved_types[[col_name]]$target_type

          if (target_type == "numeric") {
            df[[col_name]] <- as.numeric(df[[col_name]])
          } else if (target_type == "character") {
            df[[col_name]] <- as.character(df[[col_name]])
          }
        }
      }
      return(df)
    },
    .build_index = function() {
      all_files <- list.files(
        path = self$root,
        recursive = TRUE,
        full.names = TRUE
      )
      file_names <- basename(all_files)
      message("Indexing BIDS dataset...")

      pattern <- paste0(
        "^",
        "(?:sub-(?<subject>[[:alnum:]]+))?",
        "(?:_ses-(?<session>[[:alnum:]]+))?",
        "(?:_task-(?<task>[[:alnum:]]+))?",
        "(?:_tracksys-(?<tracksys>[[:alnum:]]+))?",
        "(?:_acq-(?<acq>[[:alnum:]]+))?",
        "(?:_run-(?<run>[0-9]+))?",
        "_?",
        "(?<datatype>[[:alnum:]]+)",
        "\\.(?<suffix>[[:alnum:]]+)$"
      )

      extracted_data <- stringr::str_match(file_names, pattern)

      bids_index <- tibble::tibble(
        file_path = all_files,
        subject = ifelse(is.na(extracted_data[, "subject"]),
          NA_character_,
          paste0("sub-", extracted_data[, "subject"])
        ),
        session = extracted_data[, "session"],
        task = extracted_data[, "task"],
        tracksys = extracted_data[, "tracksys"],
        acq = extracted_data[, "acq"],
        run = extracted_data[, "run"],
        datatype = extracted_data[, "datatype"],
        suffix = extracted_data[, "suffix"]
      )

      self$index <- bids_index %>%
        dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, NA_character_)))

      tsv_files <- self$index %>%
        dplyr::filter(suffix == "tsv" & datatype == "motion", !is.na(subject))

      empty_files <- .check_empty_motion_files(tsv_files)
      .check_valid_tsv_files(tsv_files)
      missing_json_files <- .check_missing_meta_files(self$index)

      self$index <- self$index %>%
        dplyr::mutate(
          is_empty = file_path %in% empty_files,
          missing_json = file_path %in% missing_json_files
        )
    },
    .format_attribute_values = function(values) {
      values <- values[!is.na(values)]
      if (length(values) > 0) {
        paste(sort(unique(values)), collapse = ", ")
      } else {
        "None"
      }
    }
  )
)

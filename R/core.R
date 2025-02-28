#' BIDS Dataset Handler
#'
#' A class for managing and accessing BIDS(Brain Imaging Data Structure) motions data.
#' See [BIDS Motion](https://bids-specification.readthedocs.io/en/stable/modality-specific-files/motion.html)
#' @param root A character string. The root directory of the BIDS dataset.
#' @param readonly Logical. Default is TRUE.
#'
#' @field root A character string. The root directory of the BIDS dataset.
#' @field index A tibble containing the BIDS dataset index.
#' @field readonly Logical. Default is TRUE.
#' @export
Bids <- R6Class( # nolint: object_name_linter.
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
    #' Load the motion files.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @param empty_check Logical. If TRUE (default), automatically filters out empty.
    #' @return A tibble containing the motion files with participant_id.
    load_motion = function(..., empty_check = TRUE) {
      motion_files <- self$index %>%
        dplyr::filter(datatype == "motion", ...)
      if (empty_check) {
        motion_files <- .check_and_filter_empty_files(motion_files)
      }
      private$.load_files_with_progress(motion_files)
    },
    #' @description
    #' Load the logs files.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @return A tibble containing the logs files with participant_id.
    load_logs = function(...) {
      logs_files <- self$index %>%
        dplyr::filter(datatype == "events", ...)
      private$.load_files_with_progress(logs_files)
    },
    #' @description
    #' Print the BIDS dataset summary.
    #' @return NULL
    print = function() {
      cat("\nBIDS Dataset Summary\n\n")

      cat(sprintf("%-20s %s\n", "Root:", self$root))
      cat(sprintf("%-20s %d\n", "Files:", nrow(self$index)))

      subject_count <- length(unique(self$index$subject))
      subjects <- paste(
        paste(head(sort(unique(self$index$subject)), 5), collapse = ", "),
        "..."
      )

      session_values <- self$index$session
      session_values <- session_values[!is.na(session_values)]
      sessions <- if (length(session_values) > 0) {
        paste(sort(unique(session_values)), collapse = ", ")
      } else {
        "None"
      }

      task_values <- self$index$task
      task_values <- task_values[!is.na(task_values)]
      tasks <- if (length(task_values) > 0) {
        paste(sort(unique(task_values)), collapse = ", ")
      } else {
        "None"
      }

      datatype_values <- self$index$datatype
      datatype_values <- datatype_values[!is.na(datatype_values)]
      datatypes <- if (length(datatype_values) > 0) {
        paste(sort(unique(datatype_values)), collapse = ", ")
      } else {
        "None"
      }

      tracksys_values <- self$index$tracksys
      tracksys_values <- tracksys_values[!is.na(tracksys_values)]
      tracksys <- if (length(tracksys_values) > 0) {
        paste(sort(unique(tracksys_values)), collapse = ", ")
      } else {
        "None"
      }

      acq_values <- self$index$acq
      acq_values <- acq_values[!is.na(acq_values)]
      acq <- if (length(acq_values) > 0) {
        paste(sort(unique(acq_values)), collapse = ", ")
      } else {
        "None"
      }

      run_values <- self$index$run
      run_values <- run_values[!is.na(run_values)]
      run <- if (length(run_values) > 0) {
        paste(sort(unique(run_values)), collapse = ", ")
      } else {
        "None"
      }

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
    .load_files_with_progress = function(file_subset) {
      if (nrow(file_subset) == 0) {
        warning("No files found.", call. = FALSE)
        return(NULL)
      }
      message(sprintf("Filtered %d files, loading...\n", nrow(file_subset)))
      flush.console()

      pb <- txtProgressBar(min = 0, max = nrow(file_subset), style = 3)
      data_list <- vector("list", nrow(file_subset))
      problematic_files <- character(0)

      for (i in seq_len(nrow(file_subset))) {
        file_path <- file_subset$file_path[i]
        df <- readr::read_tsv(file_path, show_col_types = FALSE)

        df$participant_id <- file_subset$subject[i]
        data_list[[i]] <- df
        setTxtProgressBar(pb, i)
      }
      close(pb)

      if (length(problematic_files) > 0) {
        warning("The following files have issues, data type conflict:", call. = FALSE)
        print(problematic_files)
        stop("Data merge failed. Please check the problematic files.")
      }

      dplyr::bind_rows(data_list)
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
    }
  )
)

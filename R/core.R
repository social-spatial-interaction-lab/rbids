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
    #' Load the motion files. E.g. *motion.tsv.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @param empty_check Logical. If TRUE (default), automatically filters out empty.
    #' @return A tibble containing the motion files with participant_id and other index
    #' columns. e.g. session, task, tracksys, acq, run.
    load_motion = function(..., empty_check = TRUE) {
      motion_files <- self$index %>%
        dplyr::filter(datatype == "motion", suffix == "tsv", ...)
      if (empty_check) {
        motion_files <- .check_and_filter_empty_files(motion_files)
      }
      private$.merge_with_index(motion_files)
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
    #' Load the logs files. E.g. *events.tsv.
    #' @param ... A list of filter conditions. Use session, task, tracksys, acq, run,
    #' datatype to filter.
    #' @return A tibble containing the logs files with participant_id and other index
    #' columns. e.g. session, task, tracksys, acq, run.
    load_logs = function(...) {
      logs_files <- self$index %>%
        dplyr::filter(datatype == "events", suffix == "tsv", ...)
      private$.merge_with_index(logs_files)
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
    #' Load files from a vector of file paths.
    #' @param file_paths A character vector containing file paths to load.
    #' @return A tibble containing the files with subject.
    load_files = function(file_paths) {
      if (length(file_paths) == 0) {
        warning("No files provided.", call. = FALSE)
        return(NULL)
      }
      file_subset <- self$index %>%
        dplyr::filter(file_path %in% file_paths)
      if (nrow(file_subset) == 0) {
        warning("None of the provided file paths match the index. Are you sure the file
        paths are correct?",
          call. = FALSE
        )
        return(NULL)
      }
      private$.merge_with_index(file_subset)
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
    # Merge the files with subject. The motion data files doesn't have subject info
    # inside, the file's name describes the subject, so we need to merged to help for
    # analysis.
    # param: file_subset A tibble containing the files to merge.
    # return: A tibble containing the files with subject, session, task, tracksys, acq,
    # run columns.
    .merge_with_index = function(file_subset) {
      if (nrow(file_subset) == 0) {
        warning("No files found.", call. = FALSE)
        return(NULL)
      }
      message(sprintf("Filtered %d files, loading...\n", nrow(file_subset)))
      flush.console()

      pb <- txtProgressBar(min = 0, max = nrow(file_subset), style = 3)
      data_list <- vector("list", nrow(file_subset))

      for (i in seq_len(nrow(file_subset))) {
        file_path <- file_subset$file_path[i]
        df <- readr::read_tsv(file_path, show_col_types = FALSE)

        df$participant_id <- file_subset$subject[i]
        df$session <- file_subset$session[i]
        df$task <- file_subset$task[i]
        df$tracksys <- file_subset$tracksys[i]
        df$acq <- file_subset$acq[i]
        df$run <- file_subset$run[i]
        data_list[[i]] <- df
        setTxtProgressBar(pb, i)
      }
      close(pb)

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

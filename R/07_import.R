# Bulk import and convenience methods for active-experiment treatment work.
#
# Requires:
#   * 02_animal.R
#   * 05_drug.R
#   * 05_drug_2ndpart.R
#   * 06_treatment.R
#
# This file must load after 06_treatment.R.
#
# Add `.tm_import_public_methods` to public = c(...) in:
# R/90_tumormodel_project.R


.tm_import_public_methods <- list(

  #' Read tumor measurements and animal assignments from a study Excel workbook
  #'
  #' Reads a wide-format Excel worksheet containing animal identifiers, cage
  #' identifiers, repeated tumor-measurement blocks, and optional treatment or
  #' endpoint assignment columns. The function converts the tumor measurements to
  #' long format and returns standardized measurement and assignment tables ready
  #' for review and subsequent import.
  #'
  #' Each tumor-measurement block must contain four adjacent columns in this order:
  #'
  #' \itemize{
  #'   \item Length (or L), in mm
  #'   \item Width (or W), in mm
  #'   \item Depth (or D), in mm
  #'   \item Tumor volume (or TV), in mm3
  #' }
  #'
  #' The Excel-entered tumor-volume value is retained as
  #' `spreadsheet_volume_mm3` for reconciliation only. The returned
  #' `source_volume_mm3` is recalculated using the study-standard formula:
  #'
  #' \deqn{volume = length * width^2 / 2}
  #'
  #' The worksheet must contain exactly one animal-ID column, recognized as
  #' `ID`, `Animal ID`, or `Mouse ID`, and exactly one cage column, recognized as
  #' `Cage`, `Cage ID`, `Cage No`, or `Cage Number`.
  #'
  #' Measurement dates should ordinarily be supplied through
  #' `measurement_dates`. The legacy `measurement_days` argument calculates dates
  #' from the active study's inoculation date, where measurement day 1 is the
  #' inoculation date.
  #'
  #' Optional assignment columns are recognized using common header variants for
  #' depletion, treatment, treatment start date, sacrifice/euthanasia date,
  #' sacrifice/euthanasia day, and endpoint reason. Missing optional columns are
  #' returned as `NA`.
  #'
  #' @param file Character scalar. Path to an existing `.xlsx` or `.xls` file.
  #'
  #' @param sheet Worksheet name or numeric worksheet index. Defaults to `1`.
  #'
  #' @param table_range Character scalar giving the Excel range to read, including
  #'   the header row; for example, `"E12:AF57"`.
  #'
  #' @param measurement_dates Preferred. A character vector of unique dates in
  #'   `YYYY-MM-DD` format, with one date for each tumor-measurement block in the
  #'   spreadsheet, from left to right.
  #'
  #' @param measurement_days Legacy alternative to `measurement_dates`. A vector
  #'   of unique positive integer study days, with one day for each
  #'   tumor-measurement block. Dates are calculated relative to the saved
  #'   inoculation date. Supply exactly one of `measurement_dates` and
  #'   `measurement_days`.
  #'
  #' @param tumor_label Character scalar identifying the tumor represented by the
  #'   measurement blocks. Defaults to `"RF"`.
  #'
  #' @return A named list with two data frames:
  #'
  #' \describe{
  #'   \item{`tumor_measurements`}{Long-format tumor measurements with columns
  #'   `cage_id`, `animal_id`, `tumor_label`, `measurement_day`,
  #'   `observation_date`, `length_mm`, `width_mm`, `depth_mm`,
  #'   `source_volume_mm3`, `spreadsheet_volume_mm3`, and `notes`. Rows without
  #'   both length and width measurements are excluded.}
  #'
  #'   \item{`animal_assignments`}{One row per animal with columns `cage_id`,
  #'   `animal_id`, `depletion`, `treatment`, `treatment_start_date`,
  #'   `sacrifice_date`, `sacrifice_day`, and `reason`.}
  #' }
  #'
  #' @details This method requires a valid model setup with an inoculation date.
  #' Run `study$set_model_setup()` before using this reader.
  #'
  #' The returned measurement data are intended for use with
  #' `study$load_tumor_measurements()`. That loader resolves the readable
  #' `animal_id` and `tumor_label` within the active experiment; users do not need
  #' to supply internal database primary keys.
  #'
  #' @examples
  #' \dontrun{
  #' measurement_dates <- c(
  #'   "2026-10-22",
  #'   "2026-10-26",
  #'   "2026-10-29",
  #'   "2026-11-02",
  #'   "2026-11-05"
  #' )
  #'
  #' import_data <- study$read_tumor_measurements_and_assignments_excel(
  #'   file = "mouse_spreadsheet_20261022.xlsx",
  #'   sheet = 3,
  #'   table_range = "E12:AF57",
  #'   measurement_dates = measurement_dates,
  #'   tumor_label = "RF"
  #' )
  #'
  #' tumor_measurements <- import_data$tumor_measurements
  #' animal_assignments <- import_data$animal_assignments
  #'
  #' study$load_tumor_measurements(
  #'   data = tumor_measurements
  #' )
  #' }
  #'
  #' @seealso [load_tumor_measurements()]
  #'
  #' @keywords internal
  read_tumor_measurements_and_assignments_excel = function(
      file,
      sheet = 1,
      table_range = "E12:AF57",
      measurement_dates = NULL,
      measurement_days = NULL,
      tumor_label = "RF",
      user_id = .tm_default_user_id()
    ) {
      # ---------------------------------------------------------------------------
      # Validate basic inputs.
      # ---------------------------------------------------------------------------
    
      if (
        length(file) != 1L ||
          is.na(file) ||
          !is.character(file) ||
          !nzchar(trimws(file)) ||
          !file.exists(file)
      ) {
        stop(
          "`file` must be the path to an existing Excel file.",
          call. = FALSE
        )
      }
    
      if (!tolower(tools::file_ext(file)) %in% c("xlsx", "xls")) {
        stop(
          "`file` must be an Excel (.xlsx or .xls) file.",
          call. = FALSE
        )
      }
    
      if (length(sheet) != 1L || is.na(sheet)) {
        stop(
          "`sheet` must identify exactly one worksheet.",
          call. = FALSE
        )
      }
    
      if (
        length(table_range) != 1L ||
          is.na(table_range) ||
          !is.character(table_range) ||
          !nzchar(trimws(table_range))
      ) {
        stop(
          "`table_range` must be a valid Excel range, such as `E12:AF57`.",
          call. = FALSE
        )
      }
    
      if (
        length(tumor_label) != 1L ||
          is.na(tumor_label) ||
          !is.character(tumor_label) ||
          !nzchar(trimws(tumor_label))
      ) {
        stop(
          "`tumor_label` must be one non-empty character value.",
          call. = FALSE
        )
      }
    
      tumor_label <- trimws(tumor_label)
    
      # ---------------------------------------------------------------------------
      # Exactly one date specification method must be used.
      # ---------------------------------------------------------------------------
    
      if (!is.null(measurement_dates) && !is.null(measurement_days)) {
        stop(
          paste0(
            "Supply either `measurement_dates` or `measurement_days`, ",
            "not both. `measurement_dates` is preferred."
          ),
          call. = FALSE
        )
      }
    
      if (is.null(measurement_dates) && is.null(measurement_days)) {
        stop(
          paste0(
            "Supply `measurement_dates` (preferred), such as ",
            "`c('2026-10-22', '2026-10-26')`, or legacy `measurement_days`."
          ),
          call. = FALSE
        )
      }
    
      # ---------------------------------------------------------------------------
      # Helper functions.
      # ---------------------------------------------------------------------------
    
      blank_to_na <- function(x) {
        x <- trimws(as.character(x))
        x[x == ""] <- NA_character_
        x
      }
    
      normalize_header <- function(x) {
        x <- trimws(tolower(as.character(x)))
        x <- gsub("³", "3", x, fixed = TRUE)
        gsub("[^a-z0-9]+", "", x)
      }
    
      to_numeric <- function(x) {
        suppressWarnings(
          as.numeric(
            gsub(",", "", as.character(x), fixed = TRUE)
          )
        )
      }
    
      parse_iso_date <- function(x, argument_name) {
        x <- trimws(as.character(x))
    
        parsed <- suppressWarnings(
          as.Date(x, format = "%Y-%m-%d")
        )
    
        invalid <- is.na(parsed) |
          format(parsed, "%Y-%m-%d") != x
    
        if (any(invalid)) {
          stop(
            paste0(
              "`", argument_name, "` must contain dates in YYYY-MM-DD format. ",
              "Invalid value(s): ",
              paste(unique(x[invalid]), collapse = ", "),
              "."
            ),
            call. = FALSE
          )
        }
    
        parsed
      }
    
     # ---------------------------------------------------------------------------
     # Obtain the inoculation date from the active experiment.
     #
     # In v2, setup belongs to each experiment, not to the overall project.
     # Convention:
     #   measurement_day 1 = inoculation_date
     # ---------------------------------------------------------------------------
   
     con <- private$connect()
     on.exit(DBI::dbDisconnect(con), add = TRUE)
   
     active_experiment_id <- .tm_get_active_experiment_id(
       con = con,
       user_id = user_id,
       minimum_role = "editor"
     )
   
     experiment_setup <- DBI::dbGetQuery(
       con,
       paste(
         "SELECT",
         "  experiment_id,",
         "  experiment_name,",
         "  study_start_date,",
         "  inoculation_date",
         "FROM experiments",
         "WHERE experiment_id = ?"
       ),
       params = list(active_experiment_id)
     )
   
     if (nrow(experiment_setup) != 1L) {
       stop(
         paste0(
           "Could not retrieve the active experiment `",
           active_experiment_id,
           "`."
         ),
         call. = FALSE
       )
     }
   
     inoculation_date_text <- trimws(
       as.character(experiment_setup$inoculation_date[[1L]])
     )
   
     if (is.na(inoculation_date_text) || !nzchar(inoculation_date_text)) {
       stop(
         paste0(
           "The active experiment `",
           active_experiment_id,
           "` does not have an `inoculation_date`. ",
           "Create or update the experiment before importing measurements."
         ),
         call. = FALSE
       )
     }
   
     inoculation_date <- parse_iso_date(
       inoculation_date_text,
       "active experiment inoculation_date"
     ) 
      # ---------------------------------------------------------------------------
      # Read Excel data as text so that IDs remain IDs rather than numbers.
      # ---------------------------------------------------------------------------
    
      raw_data <- tryCatch(
        {
          suppressMessages(
            as.data.frame(
              readxl::read_excel(
                path = file,
                sheet = sheet,
                range = table_range,
                col_names = FALSE,
                col_types = "text",
                na = c("", "NA", "N/A"),
                trim_ws = TRUE,
                .name_repair = "minimal"
              ),
              stringsAsFactors = FALSE
            )
          )
        },
        error = function(e) {
          stop(
            paste0(
              "Could not read Excel range `",
              table_range,
              "` from sheet `",
              sheet,
              "`: ",
              conditionMessage(e)
            ),
            call. = FALSE
          )
        }
      )
    
      if (nrow(raw_data) < 2L) {
        stop(
          "`table_range` must include one header row and at least one data row.",
          call. = FALSE
        )
      }
    
      raw_data[] <- lapply(raw_data, blank_to_na)
    
      field_headers <- unlist(
        raw_data[1, , drop = FALSE],
        use.names = FALSE
      )
    
      header_keys <- normalize_header(field_headers)
    
      # ---------------------------------------------------------------------------
      # Identify required Cage and ID columns.
      # ---------------------------------------------------------------------------
    
      cage_columns <- which(
        header_keys %in% c(
          "cage",
          "cageid",
          "cageno",
          "cagenumber"
        )
      )
    
      if (length(cage_columns) != 1L) {
        stop(
          paste0(
            "Could not identify exactly one Cage column. Found ",
            length(cage_columns),
            "."
          ),
          call. = FALSE
        )
      }
    
      id_columns <- which(
        header_keys %in% c(
          "id",
          "animalid",
          "mouseid"
        )
      )
    
      if (length(id_columns) != 1L) {
        stop(
          paste0(
            "Could not identify exactly one animal ID column. Found ",
            length(id_columns),
            "."
          ),
          call. = FALSE
        )
      }
    
      cage_column <- cage_columns[[1L]]
      id_column <- id_columns[[1L]]
    
      # ---------------------------------------------------------------------------
      # Standardize measurement-header variants.
      # ---------------------------------------------------------------------------
    
      measurement_fields <- header_keys
    
      measurement_fields[measurement_fields %in% c(
        "l",
        "lmm",
        "length",
        "lengthmm"
      )] <- "length_mm"
    
      measurement_fields[measurement_fields %in% c(
        "w",
        "wmm",
        "width",
        "widthmm"
      )] <- "width_mm"
    
      measurement_fields[measurement_fields %in% c(
        "d",
        "dmm",
        "depth",
        "depthmm"
      )] <- "depth_mm"
    
      measurement_fields[measurement_fields %in% c(
        "tv",
        "tvmm3",
        "tumorvol",
        "tumorvolmm3",
        "tumorvolume",
        "tumorvolumemm3",
        "volume",
        "volumemm3"
      )] <- "spreadsheet_volume_mm3"
    
      # ---------------------------------------------------------------------------
      # Identify tumor-measurement blocks.
      #
      # Required order in each block:
      #   Length/L | Width/W | Depth/D | Tumor volume/TV
      # ---------------------------------------------------------------------------
    
      length_columns <- which(
        measurement_fields == "length_mm"
      )
    
      if (length(length_columns) == 0L) {
        stop(
          paste0(
            "No `Length (mm)` or `L (mm)` columns were found in ",
            "the selected Excel range."
          ),
          call. = FALSE
        )
      }
    
      number_of_blocks <- length(length_columns)
    
      block_columns <- lapply(
        seq_len(number_of_blocks),
        function(block_index) {
          first_column <- length_columns[[block_index]]
          columns <- first_column + 0:3
    
          if (max(columns) > ncol(raw_data)) {
            stop(
              "Tumor-measurement block ",
              block_index,
              " is incomplete.",
              call. = FALSE
            )
          }
    
          expected_fields <- c(
            "length_mm",
            "width_mm",
            "depth_mm",
            "spreadsheet_volume_mm3"
          )
    
          actual_fields <- measurement_fields[columns]
    
          if (!identical(actual_fields, expected_fields)) {
            stop(
              paste0(
                "Tumor-measurement block ",
                block_index,
                " has an unexpected order. Expected: ",
                "Length/L, Width/W, Depth/D, Tumor volume/TV. Found: ",
                paste(field_headers[columns], collapse = " | "),
                "."
              ),
              call. = FALSE
            )
          }
    
          columns
        }
      )
    
      # ---------------------------------------------------------------------------
      # Establish observation dates and measurement days.
      # ---------------------------------------------------------------------------
    
      if (!is.null(measurement_dates)) {
        observation_dates <- parse_iso_date(
          measurement_dates,
          "measurement_dates"
        )
    
        if (length(observation_dates) != number_of_blocks) {
          stop(
            paste0(
              "`measurement_dates` contains ",
              length(observation_dates),
              " value(s), but the worksheet contains ",
              number_of_blocks,
              " tumor-measurement block(s)."
            ),
            call. = FALSE
          )
        }
    
        if (anyDuplicated(observation_dates)) {
          duplicated_dates <- unique(
            observation_dates[duplicated(observation_dates)]
          )
    
          stop(
            paste0(
              "`measurement_dates` cannot contain duplicates. Duplicated date(s): ",
              paste(format(duplicated_dates, "%Y-%m-%d"), collapse = ", "),
              "."
            ),
            call. = FALSE
          )
        }
    
        measurement_day_values <- as.integer(
          observation_dates - inoculation_date + 1L
        )
    
        if (any(measurement_day_values < 1L)) {
          stop(
            paste0(
              "One or more `measurement_dates` occur before the inoculation date ",
              format(inoculation_date, "%Y-%m-%d"),
              "."
            ),
            call. = FALSE
          )
        }
      } else {
        measurement_day_text <- trimws(
          as.character(measurement_days)
        )
    
        measurement_day_values <- suppressWarnings(
          as.integer(measurement_day_text)
        )
    
        invalid_days <- is.na(measurement_day_values) |
          measurement_day_values < 1L |
          measurement_day_text != as.character(measurement_day_values)
    
        if (any(invalid_days)) {
          stop(
            paste0(
              "`measurement_days` must contain whole positive numbers. ",
              "Invalid value(s): ",
              paste(
                unique(measurement_day_text[invalid_days]),
                collapse = ", "
              ),
              "."
            ),
            call. = FALSE
          )
        }
    
        if (length(measurement_day_values) != number_of_blocks) {
          stop(
            paste0(
              "`measurement_days` contains ",
              length(measurement_day_values),
              " value(s), but the worksheet contains ",
              number_of_blocks,
              " tumor-measurement block(s)."
            ),
            call. = FALSE
          )
        }
    
        if (anyDuplicated(measurement_day_values)) {
          duplicated_days <- unique(
            measurement_day_values[
              duplicated(measurement_day_values)
            ]
          )
    
          stop(
            paste0(
              "`measurement_days` cannot contain duplicates. Duplicated day(s): ",
              paste(duplicated_days, collapse = ", "),
              "."
            ),
            call. = FALSE
          )
        }
    
        observation_dates <- inoculation_date +
          measurement_day_values -
          1L
      }
    
      # ---------------------------------------------------------------------------
      # Extract animal-level spreadsheet data.
      # ---------------------------------------------------------------------------
    
      animal_data <- raw_data[-1, , drop = FALSE]
    
      animal_ids <- blank_to_na(animal_data[[id_column]])
      cage_ids <- blank_to_na(animal_data[[cage_column]])
    
      # Rows with no animal ID are not animals and are excluded.
      keep_rows <- !is.na(animal_ids)
    
      animal_data <- animal_data[keep_rows, , drop = FALSE]
      animal_ids <- animal_ids[keep_rows]
      cage_ids <- cage_ids[keep_rows]
    
      if (length(animal_ids) == 0L) {
        stop(
          "No animal IDs were found below the ID header.",
          call. = FALSE
        )
      }
    
      if (anyDuplicated(animal_ids)) {
        duplicated_ids <- unique(
          animal_ids[duplicated(animal_ids)]
        )
    
        stop(
          paste0(
            "Animal IDs must be unique in the selected spreadsheet range. ",
            "Duplicate ID(s): ",
            paste(duplicated_ids, collapse = ", "),
            "."
          ),
          call. = FALSE
        )
      }
    
      # ---------------------------------------------------------------------------
      # Create long-format tumor measurements.
      #
      # The Excel-entered TV is preserved only for audit/reconciliation.
      # The system volume is always recalculated:
      #
      #   volume = length × width² / 2
      # ---------------------------------------------------------------------------
    
      measurement_blocks <- lapply(
        seq_len(number_of_blocks),
        function(block_index) {
          columns <- block_columns[[block_index]]
    
          length_mm <- to_numeric(
            animal_data[[columns[[1L]]]]
          )
    
          width_mm <- to_numeric(
            animal_data[[columns[[2L]]]]
          )
    
          depth_mm <- to_numeric(
            animal_data[[columns[[3L]]]]
          )
    
          spreadsheet_volume_mm3 <- to_numeric(
            animal_data[[columns[[4L]]]]
          )
    
          source_volume_mm3 <- length_mm *
            (width_mm ^ 2) /
            2
    
          data.frame(
            cage_id = cage_ids,
            animal_id = animal_ids,
            tumor_label = rep(
              tumor_label,
              length(animal_ids)
            ),
            measurement_day = rep(
              measurement_day_values[[block_index]],
              length(animal_ids)
            ),
            observation_date = rep(
              format(
                observation_dates[[block_index]],
                "%Y-%m-%d"
              ),
              length(animal_ids)
            ),
            length_mm = length_mm,
            width_mm = width_mm,
            depth_mm = depth_mm,
            source_volume_mm3 = source_volume_mm3,
            spreadsheet_volume_mm3 = spreadsheet_volume_mm3,
            notes = NA_character_,
            stringsAsFactors = FALSE
          )
        }
      )
    
      tumor_measurements <- do.call(
        rbind,
        measurement_blocks
      )
    
      # Keep only actual measurements. A spreadsheet TV of 0 alone is not enough
      # to create a tumor measurement record; length and width are required.
      tumor_measurements <- tumor_measurements[
        !is.na(tumor_measurements$length_mm) &
          !is.na(tumor_measurements$width_mm),
        ,
        drop = FALSE
      ]
    
      if (nrow(tumor_measurements) == 0L) {
        stop(
          "No valid tumor measurements with both length and width were found.",
          call. = FALSE
        )
      }
    
      tumor_measurements <- tumor_measurements[
        order(
          tumor_measurements$animal_id,
          tumor_measurements$observation_date
        ),
        ,
        drop = FALSE
      ]
    
      rownames(tumor_measurements) <- NULL
    
      # ---------------------------------------------------------------------------
      # Extract optional assignment columns.
      #
      # The function recognizes common header variants. Missing optional columns
      # are returned as NA rather than generating an error.
      # ---------------------------------------------------------------------------
    
      find_single_optional_column <- function(valid_names) {
        matched <- which(header_keys %in% valid_names)
    
        if (length(matched) > 1L) {
          stop(
            paste0(
              "More than one column matches assignment field `",
              valid_names[[1L]],
              "`: ",
              paste(field_headers[matched], collapse = " | "),
              "."
            ),
            call. = FALSE
          )
        }
    
        if (length(matched) == 0L) {
          return(NA_integer_)
        }
    
        matched[[1L]]
      }
    
      depletion_column <- find_single_optional_column(
        c(
          "depletion",
          "depletiongroup",
          "depletiontreatment"
        )
      )
    
      treatment_column <- find_single_optional_column(
        c(
          "tx",
          "treatment",
          "treatmentgroup",
          "group"
        )
      )
    
      treatment_start_column <- find_single_optional_column(
        c(
          "txstart",
          "treatmentstart",
          "txstartdate",
          "treatmentstartdate"
        )
      )
    
      sacrifice_date_column <- find_single_optional_column(
        c(
          "sacdate",
          "sacrificedate",
          "euthanasiadate",
          "killeddate"
        )
      )
    
      sacrifice_day_column <- find_single_optional_column(
        c(
          "sacday",
          "sacrificeday",
          "euthanasiaday",
          "killedday"
        )
      )
    
      reason_column <- find_single_optional_column(
        c(
          "reason",
          "sacreason",
          "sacrificereason",
          "euthanasiareason",
          "endpointreason"
        )
      )
    
      optional_text_column <- function(column_number) {
        if (is.na(column_number)) {
          return(rep(NA_character_, length(animal_ids)))
        }
    
        blank_to_na(animal_data[[column_number]])
      }
    
      animal_assignments <- data.frame(
        cage_id = cage_ids,
        animal_id = animal_ids,
        depletion = optional_text_column(depletion_column),
        treatment = optional_text_column(treatment_column),
        treatment_start_date = optional_text_column(
          treatment_start_column
        ),
        sacrifice_date = optional_text_column(
          sacrifice_date_column
        ),
        sacrifice_day = optional_text_column(
          sacrifice_day_column
        ),
        reason = optional_text_column(reason_column),
        stringsAsFactors = FALSE
      )
    
      animal_assignments <- animal_assignments[
        order(animal_assignments$animal_id),
        ,
        drop = FALSE
      ]
    
      rownames(animal_assignments) <- NULL
    
      # ---------------------------------------------------------------------------
      # Return both standardized import tables.
      # ---------------------------------------------------------------------------
    
      list(
        tumor_measurements = tumor_measurements,
        animal_assignments = animal_assignments
      )
    },

  #' Import Treatment-Group Assignments into the Active Experiment
  #'
  #' @param csv_file Optional CSV allocation file.
  #' @param allocation Optional in-memory allocation data frame.
  #' @param assignment_date Assignment date in YYYY-MM-DD format.
  #' @param treatment_start_date Treatment start date in YYYY-MM-DD format.
  #' @param validate_cages Whether imported cage IDs should be checked against
  #'   active-experiment animal records.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return Invisibly, the imported allocation data frame.
  import_treatment_allocation = function(
    csv_file = NULL,
    allocation = NULL,
    assignment_date,
    treatment_start_date,
    validate_cages = TRUE,
    user_id = .tm_default_user_id()
  ) {
    using_csv_file <- !is.null(csv_file)
    using_allocation_data <- !is.null(allocation)

    if (using_csv_file && using_allocation_data) {
      stop(
        paste(
          "Supply either `csv_file` or `allocation`, not both.",
          "For Excel-derived data, use `allocation = imported_data`."
        ),
        call. = FALSE
      )
    }

    if (!using_csv_file && !using_allocation_data) {
      stop(
        "Supply either `csv_file` or an in-memory `allocation` data frame.",
        call. = FALSE
      )
    }

    assignment_date <- .tm_treatment_date(
      assignment_date,
      "assignment_date"
    )

    treatment_start_date <- .tm_treatment_date(
      treatment_start_date,
      "treatment_start_date"
    )

    if (using_csv_file) {
      if (
        length(csv_file) != 1L ||
          is.na(csv_file) ||
          !is.character(csv_file) ||
          !nzchar(trimws(csv_file))
      ) {
        stop(
          "`csv_file` must be one non-empty file path.",
          call. = FALSE
        )
      }

      if (!file.exists(csv_file)) {
        stop(
          "File does not exist: ",
          csv_file,
          call. = FALSE
        )
      }

      allocation <- tryCatch(
        {
          utils::read.csv(
            csv_file,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            na.strings = c("", "NA", "N/A")
          )
        },
        error = function(e) {
          stop(
            "Could not read allocation CSV: ",
            conditionMessage(e),
            call. = FALSE
          )
        }
      )

      allocation_source <- paste0(
        "CSV file ",
        normalizePath(
          csv_file,
          winslash = "/",
          mustWork = TRUE
        )
      )
    } else {
      if (!is.data.frame(allocation)) {
        stop(
          "`allocation` must be a data.frame.",
          call. = FALSE
        )
      }

      if (nrow(allocation) == 0L) {
        stop(
          "`allocation` contains no rows.",
          call. = FALSE
        )
      }

      allocation <- as.data.frame(
        allocation,
        stringsAsFactors = FALSE
      )

      allocation_source <- "in-memory allocation data"
    }

    normalize_column_name <- function(x) {
      x <- trimws(tolower(as.character(x)))
      gsub("[^a-z0-9]+", "", x)
    }

    column_keys <- normalize_column_name(names(allocation))

    find_one_column <- function(
      accepted_names,
      display_name,
      required = TRUE
    ) {
      matches <- which(column_keys %in% accepted_names)

      if (length(matches) == 0L && isTRUE(required)) {
        stop(
          "Could not identify a `",
          display_name,
          "` column in the allocation data. Available columns: ",
          paste(names(allocation), collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      if (length(matches) > 1L) {
        stop(
          "Found more than one possible `",
          display_name,
          "` column in the allocation data: ",
          paste(names(allocation)[matches], collapse = ", "),
          ". Keep only one.",
          call. = FALSE
        )
      }

      if (length(matches) == 0L) {
        return(NA_integer_)
      }

      matches[[1L]]
    }

    cage_column <- find_one_column(
      accepted_names = c(
        "cage",
        "cageid",
        "cageno",
        "cagenumber"
      ),
      display_name = "cage ID",
      required = FALSE
    )

    animal_id_column <- find_one_column(
      accepted_names = c(
        "id",
        "animalid",
        "mouseid"
      ),
      display_name = "animal ID",
      required = TRUE
    )

    treatment_group_column <- find_one_column(
      accepted_names = c(
        "tx",
        "treatmentgroup",
        "group",
        "groupname",
        "treatmentarm"
      ),
      display_name = "treatment group",
      required = FALSE
    )

    treatment_description_column <- find_one_column(
      accepted_names = c(
        "treatment",
        "treatmentdescription",
        "treatmentlabel"
      ),
      display_name = "treatment description",
      required = FALSE
    )

    if (
      !is.na(treatment_group_column) &&
        !is.na(treatment_description_column)
    ) {
      stop(
        paste(
          "Allocation data contains both a direct treatment-group column",
          "and a treatment-description column. Supply only one."
        ),
        call. = FALSE
      )
    }

    if (
      is.na(treatment_group_column) &&
        is.na(treatment_description_column)
    ) {
      stop(
        paste(
          "Could not identify a treatment-assignment column.",
          "Use `treatment_group` / `Tx` for group names or",
          "`treatment` for treatment-group descriptions."
        ),
        call. = FALSE
      )
    }

    standardized_allocation <- data.frame(
      cage_id = if (!is.na(cage_column)) {
        allocation[[cage_column]]
      } else {
        NA_character_
      },
      animal_id = allocation[[animal_id_column]],
      stringsAsFactors = FALSE
    )

    if (!is.na(treatment_group_column)) {
      standardized_allocation$treatment_group <- allocation[[treatment_group_column]]

      standardized_allocation$treatment_description <- NA_character_

      allocation_mode <- "direct treatment-group assignment"
    } else {
      standardized_allocation$treatment_group <- NA_character_

      standardized_allocation$treatment_description <- allocation[[treatment_description_column]]

      allocation_mode <- "treatment-description assignment"
    }

    allocation <- standardized_allocation

    clean_text <- function(x) {
      x <- trimws(as.character(x))
      x[x %in% c("", "NA", "N/A")] <- NA_character_
      x
    }

    allocation$cage_id <- clean_text(allocation$cage_id)
    allocation$animal_id <- clean_text(allocation$animal_id)
    allocation$treatment_group <- clean_text(
      allocation$treatment_group
    )
    allocation$treatment_description <- clean_text(
      allocation$treatment_description
    )

    if (any(is.na(allocation$animal_id))) {
      stop(
        "The allocation data contains a blank animal ID.",
        call. = FALSE
      )
    }

    duplicate_ids <- unique(
      allocation$animal_id[
        duplicated(allocation$animal_id)
      ]
    )

    if (length(duplicate_ids) > 0L) {
      stop(
        "Duplicate animal IDs in allocation data: ",
        paste(duplicate_ids, collapse = ", "),
        call. = FALSE
      )
    }

    if (
      allocation_mode == "direct treatment-group assignment" &&
        any(is.na(allocation$treatment_group))
    ) {
      stop(
        "The allocation data contains a blank treatment-group value for: ",
        paste(
          allocation$animal_id[
            is.na(allocation$treatment_group)
          ],
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    if (
      allocation_mode == "treatment-description assignment" &&
        any(is.na(allocation$treatment_description))
    ) {
      stop(
        "The allocation data contains a blank treatment description for: ",
        paste(
          allocation$animal_id[
            is.na(allocation$treatment_description)
          ],
          collapse = ", "
        ),
        call. = FALSE
      )
    }

    # `animals()` is active-experiment scoped.
    animals <- as.data.frame(
      self$animals(
        include_inactive = TRUE,
        user_id = user_id
      ),
      stringsAsFactors = FALSE
    )

    if (!all(c("animal_id", "animal_pk") %in% names(animals))) {
      stop(
        "`self$animals()` must return `animal_id` and `animal_pk` columns.",
        call. = FALSE
      )
    }

    animals$animal_id <- clean_text(animals$animal_id)

    missing_animals <- setdiff(
      allocation$animal_id,
      animals$animal_id
    )

    if (length(missing_animals) > 0L) {
      stop(
        "Animal IDs are not enrolled in the active experiment: ",
        paste(missing_animals, collapse = ", "),
        call. = FALSE
      )
    }

    if (
      isTRUE(validate_cages) &&
        "cage_id" %in% names(animals) &&
        any(!is.na(allocation$cage_id))
    ) {
      project_cage_id <- clean_text(
        animals$cage_id[
          match(
            allocation$animal_id,
            animals$animal_id
          )
        ]
      )

      cage_mismatch <- (
        !is.na(allocation$cage_id) &
          !is.na(project_cage_id) &
          allocation$cage_id != project_cage_id
      )

      if (any(cage_mismatch)) {
        mismatches <- data.frame(
          animal_id = allocation$animal_id[cage_mismatch],
          imported_cage_id = allocation$cage_id[cage_mismatch],
          project_cage_id = project_cage_id[cage_mismatch],
          stringsAsFactors = FALSE
        )

        message("\nCage-ID mismatches:\n")
        print(mismatches, row.names = FALSE)

        stop(
          "Cage IDs in allocation data do not match active-experiment ",
          "animal records.",
          call. = FALSE
        )
      }
    }

    # `treatment_groups()` is active-experiment scoped.
    groups <- as.data.frame(
      self$treatment_groups(user_id = user_id),
      stringsAsFactors = FALSE
    )

    if (!all(c("group_name", "description") %in% names(groups))) {
      stop(
        "`self$treatment_groups()` must return `group_name` and ",
        "`description` columns.",
        call. = FALSE
      )
    }

    groups$group_name <- clean_text(groups$group_name)
    groups$description <- clean_text(groups$description)

    if (nrow(groups) == 0L) {
      stop(
        "The active experiment has no treatment groups.",
        call. = FALSE
      )
    }

    if (allocation_mode == "direct treatment-group assignment") {
      unknown_groups <- setdiff(
        unique(allocation$treatment_group),
        groups$group_name
      )

      if (length(unknown_groups) > 0L) {
        stop(
          "Unknown active-experiment treatment-group value(s): ",
          paste(unknown_groups, collapse = ", "),
          ". Valid group names are: ",
          paste(groups$group_name, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      allocation$treatment_description <- groups$description[
        match(
          allocation$treatment_group,
          groups$group_name
        )
      ]
    } else {
      if (any(is.na(groups$description))) {
        stop(
          paste(
            "Every active-experiment treatment group must have a",
            "nonblank `description` when importing by treatment description."
          ),
          call. = FALSE
        )
      }

      duplicate_descriptions <- unique(
        groups$description[
          duplicated(groups$description)
        ]
      )

      if (length(duplicate_descriptions) > 0L) {
        stop(
          "Treatment-group descriptions are not unique in the active ",
          "experiment: ",
          paste(duplicate_descriptions, collapse = ", "),
          call. = FALSE
        )
      }

      group_index <- match(
        allocation$treatment_description,
        groups$description
      )

      if (any(is.na(group_index))) {
        stop(
          "Unknown treatment description(s): ",
          paste(
            unique(
              allocation$treatment_description[
                is.na(group_index)
              ]
            ),
            collapse = ", "
          ),
          ".",
          call. = FALSE
        )
      }

      allocation$treatment_group <- groups$group_name[group_index]
    }

    allocation <- allocation[
      ,
      c(
        "cage_id",
        "animal_id",
        "treatment_description",
        "treatment_group"
      ),
      drop = FALSE
    ]

    message(
      "\nTreatment assignments to import from ",
      allocation_source,
      " (",
      allocation_mode,
      "):\n"
    )

    print(allocation, row.names = FALSE)

    for (i in seq_len(nrow(allocation))) {
      self$assign_animal_to_treatment_group(
        animal_id = allocation$animal_id[[i]],
        treatment_group = allocation$treatment_group[[i]],
        assignment_date = assignment_date,
        treatment_start_date = treatment_start_date,
        user_id = user_id
      )
    }

    allocation$assignment_date <- as.Date(assignment_date)

    allocation$treatment_start_date <- as.Date(
      treatment_start_date
    )

    message(
      "\nImported ",
      nrow(allocation),
      " treatment assignment(s) into the active experiment."
    )

    invisible(allocation)
  },


  #' Prepare Scheduled Dose Records for Review
  #'
  #' This method prepares records but does not write them. Use
  #' `record_prepared_doses(..., confirm = TRUE)` after review.
  #'
  #' @param dates One or more administration dates in YYYY-MM-DD format.
  #' @param days Optional whole-number offsets from model treatment start.
  #' @param lot_numbers Optional named character vector of lot numbers,
  #'   indexed by drug identifier.
  #' @param route Optional route that overrides regimen routes.
  #' @param injection_volume_ul Optional volume that overrides regimen volume.
  #' @param notes Optional notes to apply to each prepared record.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return Invisibly, a data frame of proposed dose records.
  prepare_scheduled_doses = function(
    dates = NULL,
    days = NULL,
    lot_numbers = NULL,
    route = NULL,
    injection_volume_ul = NULL,
    notes = NULL,
    user_id = .tm_default_user_id()
  ) {
    if (!is.null(dates) && !is.null(days)) {
      stop(
        "Supply either `dates` or `days`, not both.",
        call. = FALSE
      )
    }

    if (is.null(dates) && is.null(days)) {
      stop(
        paste(
          "Supply one or more administration `dates` or",
          "treatment-relative `days`."
        ),
        call. = FALSE
      )
    }

    if (!is.null(days)) {
      if (
        !is.numeric(days) ||
          any(is.na(days)) ||
          any(days != as.integer(days))
      ) {
        stop(
          "`days` must contain whole-number offsets from treatment start.",
          call. = FALSE
        )
      }

      setup <- self$model_setup()

      treatment_start_date <- if (is.data.frame(setup)) {
        setup$treatment_start_date[[1L]]
      } else {
        setup[["treatment_start_date"]]
      }

      treatment_start_date <- .tm_treatment_date(
        treatment_start_date,
        "model_setup()$treatment_start_date"
      )

      dates <- format(
        as.Date(treatment_start_date) + as.integer(days),
        "%Y-%m-%d"
      )
    }

    dates <- sort(unique(vapply(
      dates,
      .tm_treatment_date,
      character(1),
      argument_name = "dates"
    )))

    if (!is.null(lot_numbers)) {
      if (
        is.null(names(lot_numbers)) ||
          any(is.na(names(lot_numbers))) ||
          any(!nzchar(trimws(names(lot_numbers))))
      ) {
        stop(
          paste(
            "`lot_numbers` must be a named vector, for example:",
            'c(aIL4 = "908324N1").'
          ),
          call. = FALSE
        )
      }

      lot_numbers <- as.character(lot_numbers)
      names(lot_numbers) <- trimws(names(lot_numbers))
    }

    route <- .tm_text(route, "route")

    injection_volume_ul <- .tm_optional_number(
      injection_volume_ul,
      "injection_volume_ul",
      positive = TRUE
    )

    notes <- .tm_text(notes, "notes")

    regimens <- self$treatment_group_regimens(
      user_id = user_id
    )

    required_regimen_columns <- c(
      "treatment_group",
      "treatment_name",
      "drug",
      "company_name",
      "catalog_number",
      "treatment_dose",
      "dose_unit",
      "route",
      "component_volume_ul",
      "final_injection_volume_ul"
    )

    missing_columns <- setdiff(
      required_regimen_columns,
      names(regimens)
    )

    if (length(missing_columns) > 0L) {
      stop(
        "`self$treatment_group_regimens()` is missing: ",
        paste(missing_columns, collapse = ", "),
        call. = FALSE
      )
    }

    if (nrow(regimens) == 0L) {
      stop(
        "The active experiment has no treatment-group regimens.",
        call. = FALSE
      )
    }

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    all_records <- list()

    for (administration_date in dates) {
      assignments <- self$treatment_assignments(
        active_on_date = administration_date,
        alive_only = TRUE,
        user_id = user_id
      )

      if (nrow(assignments) == 0L) {
        message(
          "No active, alive treatment assignments on ",
          administration_date,
          "."
        )

        next
      }

      records <- merge(
        assignments[
          ,
          c(
            "animal_id",
            "cage_id",
            "treatment_group"
          ),
          drop = FALSE
        ],
        regimens,
        by = "treatment_group",
        all.x = TRUE,
        sort = FALSE
      )

      missing_regimen <- is.na(records$treatment_name)

      if (any(missing_regimen)) {
        stop(
          "No regimen is registered for treatment group(s): ",
          paste(
            unique(records$treatment_group[missing_regimen]),
            collapse = ", "
          ),
          call. = FALSE
        )
      }

      records$treatment_dose <- suppressWarnings(
        as.numeric(records$treatment_dose)
      )

      invalid_dose <- (
        is.na(records$treatment_dose) |
          !is.finite(records$treatment_dose) |
          records$treatment_dose <= 0
      )

      if (any(invalid_dose)) {
        print(
          unique(
            records[
              invalid_dose,
              c(
                "treatment_group",
                "treatment_name",
                "drug",
                "treatment_dose",
                "dose_unit"
              ),
              drop = FALSE
            ]
          ),
          row.names = FALSE
        )

        stop(
          paste(
            "Every active treatment-group regimen must have a",
            "positive numeric `treatment_dose`."
          ),
          call. = FALSE
        )
      }

      invalid_unit <- (
        is.na(records$dose_unit) |
          !nzchar(trimws(as.character(records$dose_unit)))
      )

      if (any(invalid_unit)) {
        stop(
          "Every active treatment-group regimen must have a nonblank ",
          "`dose_unit`.",
          call. = FALSE
        )
      }

      records$lot_number <- vapply(
        seq_len(nrow(records)),
        function(i) {
          .tm_resolve_lot_number(
            con = con,
            drug = records$drug[[i]],
            company_name = records$company_name[[i]],
            catalog_number = records$catalog_number[[i]],
            lot_numbers = lot_numbers
          )
        },
        character(1)
      )

      records$administration_date <- administration_date
      records$dose <- records$treatment_dose

      records$actual_route <- if (!is.na(route)) {
        route
      } else {
        records$route
      }

      records$actual_volume_ul <- if (!is.na(injection_volume_ul)) {
        injection_volume_ul
      } else {
        records$final_injection_volume_ul
      }

      records$actual_notes <- notes

      all_records[[administration_date]] <- records
    }

    if (length(all_records) == 0L) {
      message("No dose records were prepared.")
      return(invisible(data.frame()))
    }

    doses_to_record <- do.call(rbind, all_records)
    rownames(doses_to_record) <- NULL

    preview_columns <- c(
      "administration_date",
      "animal_id",
      "cage_id",
      "treatment_group",
      "treatment_name",
      "drug",
      "dose",
      "dose_unit",
      "component_volume_ul",
      "lot_number",
      "actual_route",
      "actual_volume_ul"
    )

    message("\nPrepared dose records (not yet written):\n")

    print(
      doses_to_record[
        ,
        preview_columns,
        drop = FALSE
      ],
      row.names = FALSE
    )

    invisible(doses_to_record)
  },


  #' Record Dose Records Prepared by prepare_scheduled_doses()
  #'
  #' @param doses_to_record Data frame returned by `prepare_scheduled_doses()`.
  #' @param confirm Must be `TRUE` before records are written.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return Invisibly, the supplied dose data frame.
  record_prepared_doses = function(
    doses_to_record,
    confirm = FALSE,
    user_id = .tm_default_user_id()
  ) {
    if (!is.data.frame(doses_to_record)) {
      stop(
        paste(
          "`doses_to_record` must be a data frame returned by",
          "`prepare_scheduled_doses()`."
        ),
        call. = FALSE
      )
    }

    if (nrow(doses_to_record) == 0L) {
      message("No dose records were supplied.")
      return(invisible(doses_to_record))
    }

    required_columns <- c(
      "animal_id",
      "treatment_group",
      "drug",
      "company_name",
      "catalog_number",
      "lot_number",
      "administration_date",
      "dose",
      "dose_unit",
      "actual_route",
      "actual_volume_ul",
      "actual_notes"
    )

    missing_columns <- setdiff(
      required_columns,
      names(doses_to_record)
    )

    if (length(missing_columns) > 0L) {
      stop(
        "`doses_to_record` is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    doses_to_record$administration_date <- vapply(
      doses_to_record$administration_date,
      .tm_treatment_date,
      character(1),
      argument_name = "administration_date"
    )

    doses_to_record$dose <- vapply(
      doses_to_record$dose,
      .tm_optional_number,
      numeric(1),
      argument_name = "dose",
      positive = TRUE
    )

    if (any(is.na(doses_to_record$dose))) {
      stop(
        "Every dose to be recorded must have a positive numeric `dose`.",
        call. = FALSE
      )
    }

    duplicate_rows <- duplicated(
      doses_to_record[
        ,
        c(
          "animal_id",
          "treatment_group",
          "drug",
          "administration_date"
        ),
        drop = FALSE
      ]
    )

    if (any(duplicate_rows)) {
      stop(
        paste(
          "Duplicate animal/group/drug/date combinations occur in",
          "`doses_to_record`."
        ),
        call. = FALSE
      )
    }

    preview_columns <- c(
      "administration_date",
      "animal_id",
      "treatment_group",
      "drug",
      "dose",
      "dose_unit",
      "lot_number",
      "actual_route",
      "actual_volume_ul"
    )

    message("\nThe following dose records will be written:\n")

    print(
      doses_to_record[
        ,
        preview_columns,
        drop = FALSE
      ],
      row.names = FALSE
    )

    if (!isTRUE(confirm)) {
      message(
        "\nNo doses were written. Review the records, then rerun with ",
        "`confirm = TRUE`."
      )

      return(invisible(doses_to_record))
    }

    for (i in seq_len(nrow(doses_to_record))) {
      dose_row <- doses_to_record[i, , drop = FALSE]

      self$record_dose(
        animal_id = dose_row$animal_id[[1L]],
        treatment_group = dose_row$treatment_group[[1L]],
        drug = dose_row$drug[[1L]],
        company_name = dose_row$company_name[[1L]],
        catalog_number = dose_row$catalog_number[[1L]],
        lot_number = dose_row$lot_number[[1L]],
        administration_date = dose_row$administration_date[[1L]],
        dose = dose_row$dose[[1L]],
        dose_unit = dose_row$dose_unit[[1L]],
        route = dose_row$actual_route[[1L]],
        volume_ul = dose_row$actual_volume_ul[[1L]],
        notes = dose_row$actual_notes[[1L]],
        user_id = user_id
      )
    }

    message(
      "\nRecorded ",
      nrow(doses_to_record),
      " treatment-dose record(s)."
    )

    invisible(doses_to_record)
  },


  #' Import Actual Dose Records into the Active Experiment
  #'
  #' @param csv_file CSV file containing actual dose records.
  #' @param confirm Must be `TRUE` before records are written.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return Invisibly, the imported data frame.
  import_doses = function(
    csv_file,
    confirm = FALSE,
    user_id = .tm_default_user_id()
  ) {
    if (
      length(csv_file) != 1L ||
        is.na(csv_file) ||
        !is.character(csv_file) ||
        !nzchar(trimws(csv_file))
    ) {
      stop(
        "`csv_file` must be one non-empty file path.",
        call. = FALSE
      )
    }

    if (!file.exists(csv_file)) {
      stop(
        "File does not exist: ",
        csv_file,
        call. = FALSE
      )
    }

    doses_to_import <- tryCatch(
      {
        utils::read.csv(
          csv_file,
          stringsAsFactors = FALSE,
          check.names = FALSE,
          na.strings = c("", "NA", "N/A")
        )
      },
      error = function(e) {
        stop(
          "Could not read dose-import CSV: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    if (nrow(doses_to_import) == 0L) {
      stop(
        "The dose-import CSV contains no rows.",
        call. = FALSE
      )
    }

    required_columns <- c(
      "animal_id",
      "treatment_group",
      "drug",
      "company_name",
      "catalog_number",
      "lot_number",
      "administration_date",
      "dose",
      "dose_unit"
    )

    missing_columns <- setdiff(
      required_columns,
      names(doses_to_import)
    )

    if (length(missing_columns) > 0L) {
      stop(
        "Dose-import CSV is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    optional_columns <- c(
      "route",
      "volume_ul",
      "notes"
    )

    for (column in optional_columns) {
      if (!column %in% names(doses_to_import)) {
        doses_to_import[[column]] <- NA_character_
      }
    }

    doses_to_import$administration_date <- vapply(
      doses_to_import$administration_date,
      .tm_treatment_date,
      character(1),
      argument_name = "administration_date"
    )

    doses_to_import$dose <- vapply(
      doses_to_import$dose,
      .tm_optional_number,
      numeric(1),
      argument_name = "dose",
      positive = TRUE
    )

    if (any(is.na(doses_to_import$dose))) {
      stop(
        "Every imported dose must have a positive numeric `dose`.",
        call. = FALSE
      )
    }

    required_text_columns <- c(
      "animal_id",
      "treatment_group",
      "drug",
      "company_name",
      "catalog_number",
      "lot_number",
      "dose_unit"
    )

    for (column in required_text_columns) {
      values <- trimws(as.character(doses_to_import[[column]]))

      if (any(is.na(values) | !nzchar(values))) {
        bad_rows <- which(is.na(values) | !nzchar(values))

        stop(
          "Dose-import CSV has blank `",
          column,
          "` value(s) in CSV row(s): ",
          paste(bad_rows + 1L, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      doses_to_import[[column]] <- values
    }

    duplicate_rows <- duplicated(
      doses_to_import[
        ,
        c(
          "animal_id",
          "treatment_group",
          "drug",
          "administration_date"
        ),
        drop = FALSE
      ]
    )

    if (any(duplicate_rows)) {
      stop(
        paste(
          "Duplicate animal/group/drug/date combinations occur in",
          "the dose-import CSV."
        ),
        call. = FALSE
      )
    }

    preview_columns <- c(
      "administration_date",
      "animal_id",
      "treatment_group",
      "drug",
      "company_name",
      "catalog_number",
      "lot_number",
      "dose",
      "dose_unit",
      "route",
      "volume_ul"
    )

    message("\nThe following actual dose records will be imported:\n")

    print(
      doses_to_import[
        ,
        preview_columns,
        drop = FALSE
      ],
      row.names = FALSE
    )

    if (!isTRUE(confirm)) {
      message(
        "\nNo doses were written. Rerun with `confirm = TRUE` after review."
      )

      return(invisible(doses_to_import))
    }

    for (i in seq_len(nrow(doses_to_import))) {
      dose_row <- doses_to_import[i, , drop = FALSE]

      self$record_dose(
        animal_id = dose_row$animal_id[[1L]],
        treatment_group = dose_row$treatment_group[[1L]],
        drug = dose_row$drug[[1L]],
        company_name = dose_row$company_name[[1L]],
        catalog_number = dose_row$catalog_number[[1L]],
        lot_number = dose_row$lot_number[[1L]],
        administration_date = dose_row$administration_date[[1L]],
        dose = dose_row$dose[[1L]],
        dose_unit = dose_row$dose_unit[[1L]],
        route = dose_row$route[[1L]],
        volume_ul = dose_row$volume_ul[[1L]],
        notes = dose_row$notes[[1L]],
        user_id = user_id
      )
    }

    message(
      "\nImported ",
      nrow(doses_to_import),
      " dose record(s) into the active experiment."
    )

    invisible(doses_to_import)
  }
)
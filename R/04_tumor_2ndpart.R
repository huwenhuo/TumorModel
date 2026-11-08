# Additional public TumorModelProject tumor methods.
#
# This file must load after 04_tumor.R. It adds methods to the existing
# `.tm_tumor_public_methods` list rather than replacing it.

.tm_tumor_public_methods <- c(
  .tm_tumor_public_methods,

  list(

    #' Add a Tumor Measurement in the Active Experiment
    #'
    #' Tumor volume is calculated as:
    #' `(longest_dimension_mm * shortest_dimension_mm^2) / 2`.
    #'
    #' @param tumor_pk Database primary key for a tumor in the active experiment.
    #' @param measurement_date Date in YYYY-MM-DD format.
    #' @param length_mm One tumor dimension in millimeters.
    #' @param width_mm Perpendicular tumor dimension in millimeters.
    #' @param notes Optional measurement notes.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return Invisibly, the current TumorModelProject object.
    add_tumor_measurement = function(
      tumor_pk,
      measurement_date,
      length_mm,
      width_mm,
      notes = NULL,
      user_id = .tm_default_user_id()
    ) {
      tumor_pk <- suppressWarnings(as.integer(tumor_pk))
      measurement_date <- private$nullable_text(measurement_date)
      length_mm <- private$nullable_number(length_mm)
      width_mm <- private$nullable_number(width_mm)

      if (is.na(tumor_pk) || tumor_pk < 1L) {
        stop(
          "`tumor_pk` must be a valid positive integer.",
          call. = FALSE
        )
      }

      parsed_date <- suppressWarnings(
        as.Date(
          measurement_date,
          format = "%Y-%m-%d"
        )
      )

      if (
        is.na(measurement_date) ||
          is.na(parsed_date) ||
          format(parsed_date, "%Y-%m-%d") != measurement_date
      ) {
        stop(
          "`measurement_date` must use YYYY-MM-DD format.",
          call. = FALSE
        )
      }

      if (is.na(length_mm) || !is.finite(length_mm) || length_mm <= 0) {
        stop(
          "`length_mm` must be a positive number.",
          call. = FALSE
        )
      }

      if (is.na(width_mm) || !is.finite(width_mm) || width_mm <= 0) {
        stop(
          "`width_mm` must be a positive number.",
          call. = FALSE
        )
      }

      longest_dimension_mm <- max(length_mm, width_mm)
      shortest_dimension_mm <- min(length_mm, width_mm)

      volume_mm3 <- (
        longest_dimension_mm * shortest_dimension_mm^2
      ) / 2

      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      tumor <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT tumor_pk",
          "FROM tumors",
          "WHERE tumor_pk = ?",
          "AND experiment_id = ?"
        ),
        params = list(
          tumor_pk,
          experiment_id
        )
      )

      if (nrow(tumor) != 1L) {
        stop(
          "Tumor `",
          tumor_pk,
          "` does not exist in the active experiment.",
          call. = FALSE
        )
      }

      now <- .tm_now_utc()

      tryCatch(
        {
          DBI::dbExecute(
            con,
            paste(
              "INSERT INTO tumor_measurements (",
              "tumor_pk, measurement_date, length_mm, width_mm, volume_mm3,",
              "notes, created_at_utc, updated_at_utc",
              ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
            ),
            params = list(
              tumor_pk,
              measurement_date,
              length_mm,
              width_mm,
              volume_mm3,
              private$nullable_text(notes),
              now,
              now
            )
          )
        },
        error = function(e) {
          if (grepl(
            "UNIQUE constraint failed: tumor_measurements.tumor_pk, tumor_measurements.measurement_date",
            conditionMessage(e),
            fixed = TRUE
          )) {
            stop(
              "This tumor already has a measurement for ",
              measurement_date,
              ".",
              call. = FALSE
            )
          }

          stop(e)
        }
      )

      private$touch_project()

      invisible(self)
    },

    #' Delete a Tumor Measurement from the Active Experiment
    #'
    #' @param measurement_pk Database primary key for a measurement.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return Invisibly, the current TumorModelProject object.
    delete_tumor_measurement = function(
      measurement_pk,
      user_id = .tm_default_user_id()
    ) {
      measurement_pk <- suppressWarnings(as.integer(measurement_pk))

      if (is.na(measurement_pk) || measurement_pk < 1L) {
        stop(
          "`measurement_pk` must be a valid positive integer.",
          call. = FALSE
        )
      }

      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      rows_deleted <- DBI::dbExecute(
        con,
        paste(
          "DELETE FROM tumor_measurements",
          "WHERE measurement_pk = ?",
          "AND tumor_pk IN (",
          "  SELECT tumor_pk",
          "  FROM tumors",
          "  WHERE experiment_id = ?",
          ")"
        ),
        params = list(
          measurement_pk,
          experiment_id
        )
      )

      if (rows_deleted != 1L) {
        stop(
          "No measurement was deleted. It may not exist in the active experiment.",
          call. = FALSE
        )
      }

      private$touch_project()

      invisible(self)
    },

    #' Update a Tumor Measurement in the Active Experiment
    #'
    #' @param measurement_pk Database primary key for the measurement.
    #' @param measurement_date Optional replacement date in YYYY-MM-DD format.
    #' @param length_mm Optional replacement dimension in millimeters.
    #' @param width_mm Optional replacement perpendicular dimension in millimeters.
    #' @param notes Optional replacement notes. Use `""` to clear notes.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return Invisibly, the current TumorModelProject object.
    update_tumor_measurement = function(
      measurement_pk,
      measurement_date = NULL,
      length_mm = NULL,
      width_mm = NULL,
      notes = NULL,
      user_id = .tm_default_user_id()
    ) {
      measurement_pk <- suppressWarnings(as.integer(measurement_pk))

      if (is.na(measurement_pk) || measurement_pk < 1L) {
        stop(
          "`measurement_pk` must be a valid positive integer.",
          call. = FALSE
        )
      }

      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      existing <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT",
          "  m.measurement_pk,",
          "  m.tumor_pk,",
          "  m.measurement_date,",
          "  m.length_mm,",
          "  m.width_mm,",
          "  m.notes",
          "FROM tumor_measurements AS m",
          "INNER JOIN tumors AS t",
          "  ON t.tumor_pk = m.tumor_pk",
          "WHERE m.measurement_pk = ?",
          "AND t.experiment_id = ?"
        ),
        params = list(
          measurement_pk,
          experiment_id
        )
      )

      if (nrow(existing) != 1L) {
        stop(
          "No measurement exists with `measurement_pk = ",
          measurement_pk,
          "` in the active experiment.",
          call. = FALSE
        )
      }

      if (is.null(measurement_date)) {
        measurement_date <- existing$measurement_date[[1L]]
      } else {
        measurement_date <- private$nullable_text(measurement_date)

        parsed_date <- suppressWarnings(
          as.Date(
            measurement_date,
            format = "%Y-%m-%d"
          )
        )

        if (
          is.na(measurement_date) ||
            is.na(parsed_date) ||
            format(parsed_date, "%Y-%m-%d") != measurement_date
        ) {
          stop(
            "`measurement_date` must use YYYY-MM-DD format.",
            call. = FALSE
          )
        }
      }

      if (is.null(length_mm)) {
        length_mm <- existing$length_mm[[1L]]
      } else {
        length_mm <- private$nullable_number(length_mm)

        if (is.na(length_mm) || !is.finite(length_mm) || length_mm <= 0) {
          stop(
            "`length_mm` must be a positive number.",
            call. = FALSE
          )
        }
      }

      if (is.null(width_mm)) {
        width_mm <- existing$width_mm[[1L]]
      } else {
        width_mm <- private$nullable_number(width_mm)

        if (is.na(width_mm) || !is.finite(width_mm) || width_mm <= 0) {
          stop(
            "`width_mm` must be a positive number.",
            call. = FALSE
          )
        }
      }

      # NULL means retain existing notes. A blank string clears the notes.
      if (is.null(notes)) {
        notes <- existing$notes[[1L]]
      } else {
        notes <- private$nullable_text(notes)
      }

      longest_dimension_mm <- max(length_mm, width_mm)
      shortest_dimension_mm <- min(length_mm, width_mm)

      volume_mm3 <- (
        longest_dimension_mm * shortest_dimension_mm^2
      ) / 2

      now <- .tm_now_utc()

      tryCatch(
        {
          DBI::dbExecute(
            con,
            paste(
              "UPDATE tumor_measurements",
              "SET measurement_date = ?,",
              "    length_mm = ?,",
              "    width_mm = ?,",
              "    volume_mm3 = ?,",
              "    notes = ?,",
              "    updated_at_utc = ?",
              "WHERE measurement_pk = ?"
            ),
            params = list(
              measurement_date,
              length_mm,
              width_mm,
              volume_mm3,
              notes,
              now,
              measurement_pk
            )
          )
        },
        error = function(e) {
          if (grepl(
            "UNIQUE constraint failed: tumor_measurements.tumor_pk, tumor_measurements.measurement_date",
            conditionMessage(e),
            fixed = TRUE
          )) {
            stop(
              "This tumor already has a measurement for ",
              measurement_date,
              ".",
              call. = FALSE
            )
          }

          stop(e)
        }
      )

      private$touch_project()

      invisible(self)
    },

    #' Import Tumor Measurements into the Active Experiment
    #'
    #' @param file Path to a CSV, XLSX, or XLS file.
    #' @param data Optional in-memory data frame. Supply exactly one of
    #'   `file` or `data`.
    #' @param sheet Excel worksheet name or number. Ignored for CSV input.
    #' @param if_replace If `TRUE`, replace existing tumor/date measurements.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return Invisibly, a list describing the import.
    load_tumor_measurements = function(
      file = NULL,
      data = NULL,
      sheet = 1,
      if_replace = FALSE,
      user_id = .tm_default_user_id()
    ) {
      has_file <- !is.null(file)
      has_data <- !is.null(data)

      if (has_file == has_data) {
        stop(
          "Supply exactly one of `file` or `data`.",
          call. = FALSE
        )
      }

      if (
        length(if_replace) != 1L ||
          is.na(if_replace) ||
          !is.logical(if_replace)
      ) {
        stop(
          "`if_replace` must be TRUE or FALSE.",
          call. = FALSE
        )
      }

      if (has_file) {
        if (
          length(file) != 1L ||
            is.na(file) ||
            !is.character(file) ||
            !nzchar(trimws(file)) ||
            !file.exists(file)
        ) {
          stop(
            "`file` must be the path to an existing CSV or Excel file.",
            call. = FALSE
          )
        }

        file_extension <- tolower(tools::file_ext(file))

        if (!file_extension %in% c("csv", "xlsx", "xls")) {
          stop(
            "`file` must be a CSV (.csv) or Excel (.xlsx or .xls) file.",
            call. = FALSE
          )
        }

        imported_data <- tryCatch(
          {
            if (file_extension %in% c("xlsx", "xls")) {
              readxl::read_excel(
                path = file,
                sheet = sheet,
                col_types = "text",
                na = c("", "NA", "N/A")
              )
            } else {
              utils::read.csv(
                file = file,
                header = TRUE,
                stringsAsFactors = FALSE,
                colClasses = "character",
                na.strings = c("", "NA", "N/A"),
                check.names = FALSE
              )
            }
          },
          error = function(e) {
            stop(
              "Could not read the import file: ",
              conditionMessage(e),
              call. = FALSE
            )
          }
        )

        imported_data <- as.data.frame(
          imported_data,
          stringsAsFactors = FALSE
        )

        input_source <- normalizePath(
          file,
          winslash = "/",
          mustWork = TRUE
        )

        source_row_label <- "File row"
        source_row_offset <- 1L
      } else {
        if (!is.data.frame(data)) {
          stop(
            "`data` must be a data.frame or tibble.",
            call. = FALSE
          )
        }

        imported_data <- as.data.frame(
          data,
          stringsAsFactors = FALSE
        )

        imported_data[] <- lapply(
          imported_data,
          as.character
        )

        file_extension <- NULL
        input_source <- "in-memory data"
        source_row_label <- "Input row"
        source_row_offset <- 0L
      }

      names(imported_data) <- trimws(names(imported_data))

      if (nrow(imported_data) == 0L) {
        stop(
          "The import data have no measurement rows.",
          call. = FALSE
        )
      }

      blank_to_na <- function(x) {
        x <- trimws(as.character(x))
        x[x %in% c("", "NA", "N/A")] <- NA_character_
        x
      }

      imported_data[] <- lapply(
        imported_data,
        blank_to_na
      )

      required_columns <- c(
        "animal_id",
        "tumor_label",
        "length_mm",
        "width_mm"
      )

      missing_columns <- setdiff(
        required_columns,
        names(imported_data)
      )

      if (length(missing_columns) > 0L) {
        stop(
          "The import data are missing required column(s): ",
          paste(missing_columns, collapse = ", "),
          ".",
          call. = FALSE
        )
      }

      has_measurement_date <- "measurement_date" %in% names(imported_data)
      has_observation_date <- "observation_date" %in% names(imported_data)

      if (!has_measurement_date && !has_observation_date) {
        stop(
          paste(
            "The import data must contain `measurement_date` or",
            "legacy `observation_date`, using YYYY-MM-DD format."
          ),
          call. = FALSE
        )
      }

      supplied_measurement_date <- if (has_measurement_date) {
        blank_to_na(imported_data$measurement_date)
      } else {
        rep(NA_character_, nrow(imported_data))
      }

      supplied_observation_date <- if (has_observation_date) {
        blank_to_na(imported_data$observation_date)
      } else {
        rep(NA_character_, nrow(imported_data))
      }

      canonical_date <- if (has_measurement_date) {
        supplied_measurement_date
      } else {
        supplied_observation_date
      }

      if (!"depth_mm" %in% names(imported_data)) {
        imported_data$depth_mm <- NA_character_
      }

      if (!"notes" %in% names(imported_data)) {
        imported_data$notes <- NA_character_
      }

      imported_data <- data.frame(
        animal_id = blank_to_na(imported_data$animal_id),
        tumor_label = blank_to_na(imported_data$tumor_label),
        measurement_date = canonical_date,
        length_mm = blank_to_na(imported_data$length_mm),
        width_mm = blank_to_na(imported_data$width_mm),
        depth_mm = blank_to_na(imported_data$depth_mm),
        notes = blank_to_na(imported_data$notes),
        stringsAsFactors = FALSE
      )

      imported_data$source_row <- seq_len(
        nrow(imported_data)
      ) + source_row_offset

      errors <- character(0)

      add_error <- function(rows, message) {
        if (length(rows) > 0L) {
          errors <<- c(
            errors,
            paste0(
              source_row_label,
              " ",
              imported_data$source_row[rows],
              ": ",
              message
            )
          )
        }
      }

      if (has_measurement_date && has_observation_date) {
        dates_disagree <- which(
          !is.na(supplied_measurement_date) &
            !is.na(supplied_observation_date) &
            supplied_measurement_date != supplied_observation_date
        )

        add_error(
          dates_disagree,
          "`measurement_date` and `observation_date` disagree."
        )
      }

      add_error(
        which(is.na(imported_data$animal_id)),
        "`animal_id` is required."
      )

      add_error(
        which(is.na(imported_data$tumor_label)),
        "`tumor_label` is required."
      )

      add_error(
        which(is.na(imported_data$measurement_date)),
        "`measurement_date` is required."
      )

      add_error(
        which(is.na(imported_data$length_mm)),
        "`length_mm` is required."
      )

      add_error(
        which(is.na(imported_data$width_mm)),
        "`width_mm` is required."
      )

      measurement_dates <- suppressWarnings(
        as.Date(
          imported_data$measurement_date,
          format = "%Y-%m-%d"
        )
      )

      add_error(
        which(
          !is.na(imported_data$measurement_date) &
            (
              is.na(measurement_dates) |
                format(measurement_dates, "%Y-%m-%d") !=
                  imported_data$measurement_date
            )
        ),
        "`measurement_date` must use YYYY-MM-DD format."
      )

      length_values <- suppressWarnings(
        as.numeric(imported_data$length_mm)
      )

      width_values <- suppressWarnings(
        as.numeric(imported_data$width_mm)
      )

      depth_values <- suppressWarnings(
        as.numeric(imported_data$depth_mm)
      )

      add_error(
        which(!is.na(imported_data$length_mm) & is.na(length_values)),
        "`length_mm` must be numeric."
      )

      add_error(
        which(!is.na(imported_data$width_mm) & is.na(width_values)),
        "`width_mm` must be numeric."
      )

      add_error(
        which(!is.na(imported_data$depth_mm) & is.na(depth_values)),
        "`depth_mm` must be numeric when supplied."
      )

      add_error(
        which(!is.na(length_values) & length_values <= 0),
        "`length_mm` must be greater than zero."
      )

      add_error(
        which(!is.na(width_values) & width_values <= 0),
        "`width_mm` must be greater than zero."
      )

      add_error(
        which(!is.na(depth_values) & depth_values <= 0),
        "`depth_mm` must be greater than zero when supplied."
      )

      animals <- self$animals(
        include_inactive = TRUE,
        user_id = user_id
      )

      animal_row <- match(
        imported_data$animal_id,
        trimws(as.character(animals$animal_id))
      )

      add_error(
        which(
          !is.na(imported_data$animal_id) &
            is.na(animal_row)
        ),
        "`animal_id` is not enrolled in the active experiment."
      )

      animal_pks <- animals$animal_pk[animal_row]

      tumors <- self$tumors(
        user_id = user_id
      )

      tumor_key <- paste(
        tumors$animal_pk,
        trimws(as.character(tumors$tumor_label)),
        sep = "\r"
      )

      imported_tumor_key <- paste(
        animal_pks,
        imported_data$tumor_label,
        sep = "\r"
      )

      tumor_row <- match(
        imported_tumor_key,
        tumor_key
      )

      add_error(
        which(
          !is.na(animal_pks) &
            is.na(tumor_row)
        ),
        paste(
          "`tumor_label` does not exist for this active-experiment animal.",
          "Add the tumor before importing measurements."
        )
      )

      tumor_pks <- tumors$tumor_pk[tumor_row]

      import_measurement_key <- paste(
        tumor_pks,
        imported_data$measurement_date,
        sep = "\r"
      )

      duplicate_rows <- which(
        !is.na(tumor_pks) &
          duplicated(import_measurement_key)
      )

      add_error(
        duplicate_rows,
        paste(
          "A measurement for this tumor and measurement date",
          "appears more than once in this import."
        )
      )

      if (length(errors) > 0L) {
        stop(
          paste(
            c(
              "No rows were imported because the data have validation errors:",
              errors
            ),
            collapse = "\n"
          ),
          call. = FALSE
        )
      }

      volume_values <- ifelse(
        is.na(depth_values),
        (length_values * width_values^2) / 2,
        (length_values * width_values * depth_values) / 2
      )

      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      existing_measurements <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT",
          "  m.measurement_pk,",
          "  m.tumor_pk,",
          "  m.measurement_date",
          "FROM tumor_measurements AS m",
          "INNER JOIN tumors AS t",
          "  ON t.tumor_pk = m.tumor_pk",
          "WHERE t.experiment_id = ?"
        ),
        params = list(experiment_id)
      )

      existing_key <- paste(
        existing_measurements$tumor_pk,
        existing_measurements$measurement_date,
        sep = "\r"
      )

      existing_row <- match(
        import_measurement_key,
        existing_key
      )

      already_exists <- !is.na(existing_row)

      if (any(already_exists) && !if_replace) {
        stop(
          paste(
            c(
              "No rows were imported because these measurements already exist:",
              paste0(
                source_row_label,
                " ",
                imported_data$source_row[already_exists],
                ": a measurement for this tumor and date already exists. ",
                "Use `if_replace = TRUE` to replace it."
              )
            ),
            collapse = "\n"
          ),
          call. = FALSE
        )
      }

      measurement_pks <- existing_measurements$measurement_pk[
        existing_row
      ]

      import_time <- .tm_now_utc()

      rows_inserted <- sum(!already_exists)
      rows_replaced <- sum(already_exists)

      DBI::dbWithTransaction(con, {
        for (i in seq_len(nrow(imported_data))) {
          if (already_exists[i]) {
            DBI::dbExecute(
              con,
              paste(
                "UPDATE tumor_measurements",
                "SET length_mm = ?,",
                "    width_mm = ?,",
                "    depth_mm = ?,",
                "    volume_mm3 = ?,",
                "    notes = ?,",
                "    updated_at_utc = ?",
                "WHERE measurement_pk = ?"
              ),
              params = list(
                length_values[i],
                width_values[i],
                depth_values[i],
                volume_values[i],
                imported_data$notes[i],
                import_time,
                measurement_pks[i]
              )
            )
          } else {
            DBI::dbExecute(
              con,
              paste(
                "INSERT INTO tumor_measurements (",
                "tumor_pk,",
                "measurement_date,",
                "length_mm,",
                "width_mm,",
                "depth_mm,",
                "volume_mm3,",
                "notes,",
                "created_at_utc,",
                "updated_at_utc",
                ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
              ),
              params = list(
                tumor_pks[i],
                format(measurement_dates[i], "%Y-%m-%d"),
                length_values[i],
                width_values[i],
                depth_values[i],
                volume_values[i],
                imported_data$notes[i],
                import_time,
                import_time
              )
            )
          }
        }
      })

      private$touch_project()

      invisible(
        list(
          experiment_id = experiment_id,
          rows_imported = nrow(imported_data),
          rows_inserted = rows_inserted,
          rows_replaced = rows_replaced,
          source = input_source,
          file = if (has_file) input_source else NULL,
          sheet = if (
            has_file &&
              !is.null(file_extension) &&
              file_extension %in% c("xlsx", "xls")
          ) {
            sheet
          } else {
            NULL
          }
        )
      )
    },

    #' Return Active-Experiment Animals Whose Tumors Exceed a Threshold
    #'
    #' @param volume_threshold_mm3 Tumor-volume cutoff in mm^3.
    #' @param start_date Optional first measurement date to evaluate.
    #' @param comparison Whether volume must be `">"` or `">="` threshold.
    #' @param latest_per_animal If `TRUE`, return the newest qualifying
    #'   measurement per animal.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return A data frame of qualifying tumor measurements.
    animals_over_tumor_threshold = function(
      volume_threshold_mm3 = 950,
      start_date = NULL,
      comparison = c(">", ">="),
      latest_per_animal = TRUE,
      user_id = .tm_default_user_id()
    ) {
      comparison <- match.arg(comparison)

      if (
        length(volume_threshold_mm3) != 1L ||
          is.na(volume_threshold_mm3) ||
          !is.numeric(volume_threshold_mm3) ||
          volume_threshold_mm3 < 0
      ) {
        stop(
          "`volume_threshold_mm3` must be one non-negative numeric value.",
          call. = FALSE
        )
      }

      if (!is.null(start_date)) {
        start_date <- as.Date(start_date)

        if (length(start_date) != 1L || is.na(start_date)) {
          stop(
            "`start_date` must be one valid date.",
            call. = FALSE
          )
        }
      }

      measurements <- self$tumor_measurements(
        user_id = user_id
      )

      if (nrow(measurements) == 0L) {
        return(measurements)
      }

      measurements$measurement_date <- as.Date(
        measurements$measurement_date
      )

      if (!is.null(start_date)) {
        measurements <- measurements[
          !is.na(measurements$measurement_date) &
            measurements$measurement_date >= start_date,
          ,
          drop = FALSE
        ]
      }

      if (comparison == ">") {
        measurements <- measurements[
          !is.na(measurements$volume_mm3) &
            measurements$volume_mm3 > volume_threshold_mm3,
          ,
          drop = FALSE
        ]
      } else {
        measurements <- measurements[
          !is.na(measurements$volume_mm3) &
            measurements$volume_mm3 >= volume_threshold_mm3,
          ,
          drop = FALSE
        ]
      }

      if (nrow(measurements) == 0L) {
        return(measurements)
      }

      measurements <- measurements[
        order(
          measurements$animal_id,
          -as.numeric(measurements$measurement_date),
          -measurements$volume_mm3
        ),
        ,
        drop = FALSE
      ]

      if (isTRUE(latest_per_animal)) {
        measurements <- measurements[
          !duplicated(measurements$animal_id),
          ,
          drop = FALSE
        ]
      }

      cage_sort <- measurements$cage_id
      cage_sort[is.na(cage_sort)] <- "ZZZZZZ"

      measurements <- measurements[
        order(
          cage_sort,
          -measurements$volume_mm3,
          measurements$animal_id
        ),
        ,
        drop = FALSE
      ]

      rownames(measurements) <- NULL

      measurements[
        ,
        c(
          "experiment_id",
          "cage_id",
          "animal_id",
          "tumor_label",
          "measurement_date",
          "length_mm",
          "width_mm",
          "depth_mm",
          "volume_mm3",
          "notes",
          "tumor_pk",
          "animal_pk",
          "measurement_pk"
        ),
        drop = FALSE
      ]
    }
  )    
)
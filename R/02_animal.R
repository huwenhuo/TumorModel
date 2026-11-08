# Animal helpers and R6 public methods.
#
# Animals are stored in the central `animals` registry.
# Experiment-specific membership, cage location, and status are stored in
# `experiment_animals`.
#
# Therefore:
#
#   animals
#     = central animal registry
#
#   experiment_animals
#     = an animal's membership in a specific experiment
#
# Most public methods below operate on the current user's active experiment.

.tm_active_experiment_for_user <- function(
  con,
  user_id = .tm_default_user_id(),
  minimum_role = c("viewer", "editor", "owner")
) {
  minimum_role <- match.arg(minimum_role)

  .tm_get_active_experiment_id(
    con = con,
    user_id = user_id,
    minimum_role = minimum_role
  )
}

.tm_validate_animal_status <- function(status) {
  allowed_status <- c(
    "Planned",
    "Active",
    "Removed",
    "Killed",
    "Excluded"
  )

  if (is.na(status) || !status %in% allowed_status) {
    stop(
      "`status` must be one of: ",
      paste(allowed_status, collapse = ", "),
      call. = FALSE
    )
  }

  status
}

.tm_animal_public_methods <- list(

  #' Return the Active Experiment's Animal Roster
  #'
  #' @param include_inactive Logical. If `TRUE`, return all animals enrolled
  #'   in the active experiment. If `FALSE`, return only animals whose
  #'   experiment-specific status is `"Active"`.
  #' @param user_id User whose active experiment should be used.
  #'
  #' @return A data frame containing animals enrolled in the active experiment.
  animals = function(
    include_inactive = TRUE,
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )

    sql <- paste(
      "SELECT",
      "ea.experiment_animal_pk,",
      "ea.experiment_id,",
      "a.animal_pk,",
      "a.animal_id,",
      "a.cage_id,",
      "a.sex,",
      "ea.status,",
      "ea.enrollment_date,",
      "ea.removal_date,",
      "ea.removal_reason,",
      "a.killed_date,",
      "COALESCE(ea.notes, a.notes) AS notes,",
      "a.created_at_utc,",
      "a.updated_at_utc",
      "FROM experiment_animals AS ea",
      "INNER JOIN animals AS a",
      "ON a.animal_pk = ea.animal_pk",
      "WHERE ea.experiment_id = ?"
    )

    params <- list(experiment_id)

    if (!isTRUE(include_inactive)) {
      sql <- paste(
        sql,
        "AND ea.status = 'Active'"
      )
    }

    sql <- paste(
      sql,
      "ORDER BY ea.cage_id, a.animal_id"
    )

    DBI::dbGetQuery(
      con,
      sql,
      params = params
    )
  },

  #' Add an Animal and Enroll It in the Active Experiment
  #'
  #' Creates a central animal record and enrolls that animal in the active
  #' experiment. Animal identifiers are globally unique in the current schema.
  #'
  #' @param animal_id Unique user-facing animal identifier.
  #' @param cage_id Optional cage identifier.
  #' @param species Optional species name, e.g. `"Mus musculus"`.
  #' @param age_weeks Optional positive numeric age in weeks at enrollment.
  #' @param sex Optional sex value, typically `"Female"` or `"Male"`.
  #' @param notes Optional free-text notes.
  #' @param user_id User performing the operation.
  #'
  #' @return Invisibly returns the study object.
  add_animal = function(
    animal_id,
    cage_id = NULL,
    species = NULL,
    strain = NULL, 
    age_weeks = NULL,
    sex = NULL,
    notes = NULL,
    user_id = .tm_default_user_id()
  ) {
    # -----------------------------------------------------------------------
    # Validate required animal ID.
    # -----------------------------------------------------------------------
    animal_id <- private$nullable_text(animal_id)
  
    if (is.na(animal_id)) {
      stop(
        "`animal_id` must be one non-empty text value.",
        call. = FALSE
      )
    }
  
    # -----------------------------------------------------------------------
    # Normalize optional text fields.
    # -----------------------------------------------------------------------
    cage_id <- private$nullable_text(cage_id)
    species <- private$nullable_text(species)
    strain <- private$nullable_text(strain)
    sex <- private$nullable_text(sex)
    notes <- private$nullable_text(notes)
  
    # -----------------------------------------------------------------------
    # Validate age. Store age at enrollment/baseline in weeks.
    # -----------------------------------------------------------------------
    if (is.null(age_weeks) || length(age_weeks) == 0L) {
      age_weeks <- NA_real_
    } else {
      if (length(age_weeks) != 1L) {
        stop(
          "`age_weeks` must be NULL or one positive numeric value.",
          call. = FALSE
        )
      }
  
      age_weeks <- suppressWarnings(as.numeric(age_weeks))
  
      if (
        is.na(age_weeks) ||
          !is.finite(age_weeks) ||
          age_weeks <= 0
      ) {
        stop(
          "`age_weeks` must be NULL or one positive numeric value.",
          call. = FALSE
        )
      }
    }
  
    # Optional: normalize common sex representations.
    if (!is.na(sex)) {
      sex_key <- tolower(sex)
  
      sex <- switch(
        sex_key,
        "f" = "Female",
        "female" = "Female",
        "m" = "Male",
        "male" = "Male",
        sex
      )
    }
  
    # -----------------------------------------------------------------------
    # Connect and identify the active experiment before making changes.
    # -----------------------------------------------------------------------
    con <- private$connect()
  
    on.exit(
      DBI::dbDisconnect(con),
      add = TRUE
    )
  
    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )
  
    # -----------------------------------------------------------------------
    # Prevent duplicate central animal identifiers.
    # -----------------------------------------------------------------------
    existing_animal <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT animal_pk, animal_id",
        "FROM animals",
        "WHERE trim(animal_id) = ?"
      ),
      params = list(animal_id)
    )
  
    if (nrow(existing_animal) > 0L) {
      stop(
        paste0(
          "An animal with animal_id `", animal_id,
          "` already exists (animal_pk = ",
          existing_animal$animal_pk[[1L]],
          "). Use the enrollment method if you intend to add that existing ",
          "animal to the active experiment."
        ),
        call. = FALSE
      )
    }
  
    # -----------------------------------------------------------------------
    # Insert central animal record and active-experiment enrollment together.
    # -----------------------------------------------------------------------
    now <- .tm_now_utc()
  
    animal_pk <- DBI::dbWithTransaction(
      con,
      {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO animals (",
            "  cage_id,",
            "  animal_id,",
            "  species,",
            "  strain,",
            "  age_weeks,",
            "  sex,",
            "  status,",
            "  killed_date,",
            "  notes,",
            "  created_at_utc,",
            "  updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            cage_id,
            animal_id,
            species,
            strain, 
            age_weeks,
            sex,
            "Active",
            NA_character_,
            notes,
            now,
            now
          )
        )
  
        animal_pk <- DBI::dbGetQuery(
          con,
          "SELECT last_insert_rowid() AS animal_pk"
        )$animal_pk[[1L]]
  
        enrollment_rows <- DBI::dbExecute(
          con,
          paste(
            "INSERT INTO experiment_animals (",
            "  experiment_id,",
            "  animal_pk,",
            "  status,",
            "  removal_date,",
            "  created_at_utc,",
            "  updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            experiment_id,
            animal_pk,
            "Active",
            NA_character_,
            now,
            now
          )
        )
  
        if (enrollment_rows != 1L) {
          stop(
            "The animal was created but could not be enrolled in the active ",
            "experiment. Transaction rolled back.",
            call. = FALSE
          )
        }
  
        animal_pk
      }
    )
  
    private$touch_project()
  
    message(
      "Added animal `", animal_id,
      "` (animal_pk = ", animal_pk,
      ") and enrolled it in active experiment `",
      experiment_id, "`."
    )
  
    invisible(self)
  },    

  #' Update an Animal Field in the Active Experiment
  #'
  #' `cage_id` and `status` are experiment-specific.
  #'
  #' `animal_id`, `sex`, and `notes` belong to the central animal record.
  update_animal_field = function(
    animal_pk,
    field,
    value,
    user_id = .tm_default_user_id()
  ) {
    animal_pk <- suppressWarnings(as.integer(animal_pk))

    if (is.na(animal_pk) || animal_pk < 1L) {
      stop(
        "`animal_pk` must be a valid positive integer.",
        call. = FALSE
      )
    }

    allowed_fields <- c(
      "cage_id",
      "animal_id",
      "sex",
      "status",
      "notes"
    )

    if (!field %in% allowed_fields) {
      stop(
        "Only these animal fields may be updated: ",
        paste(allowed_fields, collapse = ", "),
        call. = FALSE
      )
    }

    value <- private$nullable_text(value)

    if (field == "animal_id" && is.na(value)) {
      stop(
        "`animal_id` cannot be blank.",
        call. = FALSE
      )
    }

    if (field == "status") {
      .tm_validate_animal_status(value)
    }

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )

    now <- .tm_now_utc()

    tryCatch(
      {
        # These fields belong to the experiment enrollment record.
        if (field %in% c("cage_id", "status")) {
          rows_changed <- DBI::dbExecute(
            con,
            paste0(
              "UPDATE experiment_animals ",
              "SET ", field, " = ?, updated_at_utc = ? ",
              "WHERE experiment_id = ? ",
              "AND animal_pk = ?"
            ),
            params = list(
              value,
              now,
              experiment_id,
              animal_pk
            )
          )
        } else {
          # Confirm the animal is enrolled in the active experiment before
          # updating a central-registry field.
          enrolled <- DBI::dbGetQuery(
            con,
            paste(
              "SELECT experiment_animal_pk",
              "FROM experiment_animals",
              "WHERE experiment_id = ?",
              "AND animal_pk = ?"
            ),
            params = list(
              experiment_id,
              animal_pk
            )
          )

          if (nrow(enrolled) != 1L) {
            stop(
              "Animal `",
              animal_pk,
              "` is not enrolled in the active experiment.",
              call. = FALSE
            )
          }

          rows_changed <- DBI::dbExecute(
            con,
            paste0(
              "UPDATE animals ",
              "SET ", field, " = ?, updated_at_utc = ? ",
              "WHERE animal_pk = ?"
            ),
            params = list(
              value,
              now,
              animal_pk
            )
          )
        }

        if (rows_changed != 1L) {
          stop(
            "No active-experiment animal was found with animal_pk = ",
            animal_pk,
            call. = FALSE
          )
        }
      },
      error = function(e) {
        if (grepl(
          "UNIQUE constraint failed: animals.animal_id",
          conditionMessage(e),
          fixed = TRUE
        )) {
          stop(
            "An animal with animal_id `",
            value,
            "` already exists in the central animal registry.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  },

  #' Remove an Animal from the Active Experiment
  #'
  #' This removes only the experiment enrollment. It does not delete the
  #' animal from the central registry and does not remove data from another
  #' experiment.
  delete_animal = function(
    animal_pk,
    user_id = .tm_default_user_id()
  ) {
    animal_pk <- suppressWarnings(as.integer(animal_pk))

    if (is.na(animal_pk) || animal_pk < 1L) {
      stop(
        "`animal_pk` must be a valid positive integer.",
        call. = FALSE
      )
    }

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )

    rows_deleted <- DBI::dbExecute(
      con,
      paste(
        "DELETE FROM experiment_animals",
        "WHERE experiment_id = ?",
        "AND animal_pk = ?"
      ),
      params = list(
        experiment_id,
        animal_pk
      )
    )

    if (rows_deleted != 1L) {
      stop(
        "No enrolled animal was deleted; the record may not exist ",
        "in the active experiment.",
        call. = FALSE
      )
    }

    private$touch_project()

    invisible(self)
  },

  #' Set an Animal's Killed Date
  #'
  #' A killed date is a central animal record because death is not specific
  #' to an experiment. The active experiment enrollment is also marked
  #' `"Killed"` when a date is supplied.
  set_animal_killed_date = function(
    animal_pk,
    killed_date = NULL,
    user_id = .tm_default_user_id()
  ) {
    animal_pk <- suppressWarnings(as.integer(animal_pk))

    if (is.na(animal_pk) || animal_pk < 1L) {
      stop(
        "`animal_pk` must be a valid positive integer.",
        call. = FALSE
      )
    }

    killed_date <- private$nullable_text(killed_date)

    if (!is.na(killed_date)) {
      parsed_date <- as.Date(
        killed_date,
        format = "%Y-%m-%d"
      )

      if (
        is.na(parsed_date) ||
          format(parsed_date, "%Y-%m-%d") != killed_date
      ) {
        stop(
          "`killed_date` must use YYYY-MM-DD format.",
          call. = FALSE
        )
      }
    }

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )

    now <- .tm_now_utc()

    DBI::dbWithTransaction(con, {
      enrolled <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT experiment_animal_pk",
          "FROM experiment_animals",
          "WHERE experiment_id = ?",
          "AND animal_pk = ?"
        ),
        params = list(
          experiment_id,
          animal_pk
        )
      )

      if (nrow(enrolled) != 1L) {
        stop(
          "Animal `",
          animal_pk,
          "` is not enrolled in the active experiment.",
          call. = FALSE
        )
      }

      rows_updated <- DBI::dbExecute(
        con,
        paste(
          "UPDATE animals",
          "SET killed_date = ?,",
          "updated_at_utc = ?",
          "WHERE animal_pk = ?"
        ),
        params = list(
          killed_date,
          now,
          animal_pk
        )
      )

      if (rows_updated != 1L) {
        stop(
          "No animal was updated; the record may no longer exist.",
          call. = FALSE
        )
      }

      if (!is.na(killed_date)) {
        DBI::dbExecute(
          con,
          paste(
            "UPDATE experiment_animals",
            "SET status = 'Killed',",
            "removal_date = ?,",
            "updated_at_utc = ?",
            "WHERE experiment_id = ?",
            "AND animal_pk = ?"
          ),
          params = list(
            killed_date,
            now,
            experiment_id,
            animal_pk
          )
        )
      }
    })

    private$touch_project()

    invisible(self)
  },

  #' Record Killed Dates for Multiple Animals
  #'
  #' Sets the central killed date for one or more animals and marks each
  #' animal's enrollment in the active experiment as Killed.
  #'
  #' @param animal_id One or more user-facing animal IDs.
  #' @param killed_date Date of euthanasia/killing, in YYYY-MM-DD format.
  #' @param reason Optional text appended to the central animal notes.
  #' @param confirm Must be TRUE before records are changed.
  #' @param user_id User performing the operation.
  #'
  #' @return Invisibly returns the study/project object.
  record_killed_animals = function(
    animal_id,
    killed_date = Sys.Date(),
    reason = NULL,
    confirm = FALSE,
    user_id = .tm_default_user_id()
  ) {
    # -----------------------------------------------------------------------
    # Validate animal IDs.
    # -----------------------------------------------------------------------
    if (is.null(animal_id) || length(animal_id) == 0L) {
      stop(
        "`animal_id` must contain one or more animal IDs.",
        call. = FALSE
      )
    }
  
    animal_ids <- trimws(as.character(animal_id))
  
    if (any(is.na(animal_ids) | !nzchar(animal_ids))) {
      stop(
        "`animal_id` cannot contain missing or blank values.",
        call. = FALSE
      )
    }
  
    animal_ids <- unique(animal_ids)
  
    # -----------------------------------------------------------------------
    # Validate killed date in the same format expected by
    # set_animal_killed_date().
    # -----------------------------------------------------------------------
    killed_date <- as.Date(killed_date)
  
    if (length(killed_date) != 1L || is.na(killed_date)) {
      stop(
        "`killed_date` must be one valid date in YYYY-MM-DD format.",
        call. = FALSE
      )
    }
  
    if (killed_date > Sys.Date()) {
      stop(
        "`killed_date` cannot be in the future.",
        call. = FALSE
      )
    }
  
    killed_date_text <- format(killed_date, "%Y-%m-%d")
  
    # -----------------------------------------------------------------------
    # Validate confirmation and optional reason.
    # -----------------------------------------------------------------------
    if (
      !is.logical(confirm) ||
        length(confirm) != 1L ||
        is.na(confirm)
    ) {
      stop(
        "`confirm` must be either TRUE or FALSE.",
        call. = FALSE
      )
    }
  
    reason <- private$nullable_text(reason)
  
    # -----------------------------------------------------------------------
    # Restrict the animal lookup to the active experiment.
    # -----------------------------------------------------------------------
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  
    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )
  
    placeholders <- paste(
      rep("?", length(animal_ids)),
      collapse = ", "
    )
  
    selected_animals <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT ",
        "  a.animal_pk, ",
        "  a.animal_id, ",
        "  a.cage_id, ",
        "  a.killed_date, ",
        "  ea.experiment_animal_pk, ",
        "  ea.status AS experiment_status, ",
        "  ea.removal_date ",
        "FROM experiment_animals ea ",
        "INNER JOIN animals a ",
        "  ON a.animal_pk = ea.animal_pk ",
        "WHERE ea.experiment_id = ? ",
        "  AND trim(a.animal_id) IN (", placeholders, ") ",
        "ORDER BY a.cage_id, a.animal_id"
      ),
      params = c(
        list(experiment_id),
        as.list(animal_ids)
      )
    )
  
    found_ids <- trimws(as.character(selected_animals$animal_id))
  
    missing_ids <- setdiff(animal_ids, found_ids)
  
    if (length(missing_ids) > 0L) {
      stop(
        paste0(
          "The following animal ID(s) are not enrolled in the active ",
          "experiment: ",
          paste(missing_ids, collapse = ", "),
          ". No records were changed."
        ),
        call. = FALSE
      )
    }
  
    selected_animals$killed_date <- as.character(
      selected_animals$killed_date
    )
  
    selected_animals$removal_date <- as.character(
      selected_animals$removal_date
    )
  
    selected_animals$experiment_status <- trimws(
      as.character(selected_animals$experiment_status)
    )
  
    already_correct <- !is.na(selected_animals$killed_date) &
      selected_animals$killed_date == killed_date_text &
      !is.na(selected_animals$experiment_status) &
      tolower(selected_animals$experiment_status) == "killed" &
      !is.na(selected_animals$removal_date) &
      selected_animals$removal_date == killed_date_text
  
    to_update <- selected_animals[
      !already_correct,
      ,
      drop = FALSE
    ]
  
    unchanged <- selected_animals[
      already_correct,
      ,
      drop = FALSE
    ]
  
    # -----------------------------------------------------------------------
    # Preview.
    # -----------------------------------------------------------------------
    message(
      "Kill-date update preview for active experiment `",
      experiment_id,
      "`:"
    )
  
    print(
      selected_animals[
        c(
          "animal_id",
          "cage_id",
          "killed_date",
          "experiment_status",
          "removal_date"
        )
      ],
      row.names = FALSE
    )
  
    message(
      "  Records needing update: ", nrow(to_update), "\n",
      "  Already correctly recorded: ", nrow(unchanged)
    )
  
    if (!is.na(reason)) {
      message("Reason to append to changed central animal records: ", reason)
    }
  
    if (nrow(to_update) == 0L) {
      message(
        "No changes needed: all selected animals already have killed_date = ",
        killed_date_text,
        " and are marked Killed in this experiment."
      )
  
      return(invisible(self))
    }
  
    if (!isTRUE(confirm)) {
      stop(
        paste0(
          "No records were changed. Review the preview and re-run with ",
          "`confirm = TRUE` to update ",
          nrow(to_update),
          " animal(s)."
        ),
        call. = FALSE
      )
    }
  
    # -----------------------------------------------------------------------
    # Update central death record and active-experiment enrollment together.
    # -----------------------------------------------------------------------
    now <- .tm_now_utc()
  
    DBI::dbWithTransaction(con, {
      for (i in seq_len(nrow(to_update))) {
        animal_pk <- to_update$animal_pk[[i]]
  
        if (is.na(reason)) {
          rows_updated <- DBI::dbExecute(
            con,
            paste(
              "UPDATE animals",
              "SET killed_date = ?,",
              "    updated_at_utc = ?",
              "WHERE animal_pk = ?"
            ),
            params = list(
              killed_date_text,
              now,
              animal_pk
            )
          )
        } else {
          rows_updated <- DBI::dbExecute(
            con,
            paste(
              "UPDATE animals",
              "SET killed_date = ?,",
              "    notes = CASE",
              "      WHEN notes IS NULL OR trim(notes) = '' THEN ?",
              "      ELSE notes || char(10) || ?",
              "    END,",
              "    updated_at_utc = ?",
              "WHERE animal_pk = ?"
            ),
            params = list(
              killed_date_text,
              reason,
              reason,
              now,
              animal_pk
            )
          )
        }
  
        if (rows_updated != 1L) {
          stop(
            "Failed to update central record for animal_pk = ",
            animal_pk,
            ". Transaction rolled back.",
            call. = FALSE
          )
        }
  
        enrollment_updated <- DBI::dbExecute(
          con,
          paste(
            "UPDATE experiment_animals",
            "SET status = 'Killed',",
            "    removal_date = ?,",
            "    updated_at_utc = ?",
            "WHERE experiment_id = ?",
            "  AND animal_pk = ?"
          ),
          params = list(
            killed_date_text,
            now,
            experiment_id,
            animal_pk
          )
        )
  
        if (enrollment_updated != 1L) {
          stop(
            "Failed to update active-experiment enrollment for animal_pk = ",
            animal_pk,
            ". Transaction rolled back.",
            call. = FALSE
          )
        }
      }
    })
  
    private$touch_project()
  
    message(
      "Recorded killed_date = ", killed_date_text,
      " and active-experiment status = Killed for ",
      nrow(to_update),
      " animal(s)."
    )
  
    if (nrow(unchanged) > 0L) {
      message(
        nrow(unchanged),
        " animal(s) were already correctly recorded and were unchanged."
      )
    }
  
    invisible(self)
  },

  #' Import Animal Variables into the Active Experiment
  load_vars = function(
    file,
    sheet = 1,
    user_id = .tm_default_user_id()
  ) {
    if (
      length(file) != 1L ||
        is.na(file) ||
        !nzchar(file) ||
        !file.exists(file)
    ) {
      stop(
        "`file` must be the path to an existing Excel or CSV file.",
        call. = FALSE
      )
    }

    file_extension <- tolower(
      tools::file_ext(file)
    )

    if (!file_extension %in% c("xlsx", "xls", "csv")) {
      stop(
        "`file` must be an Excel (.xlsx or .xls) or CSV (.csv) file.",
        call. = FALSE
      )
    }

    required_columns <- c(
      "animal_id",
      "observation_date",
      "variable_name",
      "value_numeric",
      "value_text",
      "unit",
      "notes"
    )

    imported_data <- tryCatch(
      {
        if (file_extension %in% c("xlsx", "xls")) {
          readxl::read_excel(
            path = file,
            sheet = sheet,
            na = c("", "NA", "N/A")
          )
        } else {
          utils::read.csv(
            file = file,
            header = TRUE,
            stringsAsFactors = FALSE,
            na.strings = c("", "NA", "N/A"),
            check.names = FALSE
          )
        }
      },
      error = function(e) {
        stop(
          "Could not read the input file: ",
          conditionMessage(e),
          call. = FALSE
        )
      }
    )

    imported_data <- as.data.frame(
      imported_data,
      stringsAsFactors = FALSE
    )

    missing_columns <- setdiff(
      required_columns,
      names(imported_data)
    )

    if (length(missing_columns) > 0L) {
      stop(
        "The input file is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    imported_data <- imported_data[
      ,
      required_columns,
      drop = FALSE
    ]

    if (nrow(imported_data) == 0L) {
      stop(
        "The input file has no data rows to import.",
        call. = FALSE
      )
    }

    blank_to_na <- function(x) {
      x <- trimws(as.character(x))
      x[x == ""] <- NA_character_
      x
    }

    imported_data[] <- lapply(
      imported_data,
      blank_to_na
    )

    imported_data$excel_row <- seq_len(
      nrow(imported_data)
    ) + 1L

    errors <- character(0)

    add_error <- function(rows, message) {
      if (length(rows) > 0L) {
        errors <<- c(
          errors,
          paste0(
            "Input row ",
            imported_data$excel_row[rows],
            ": ",
            message
          )
        )
      }
    }

    add_error(
      which(is.na(imported_data$animal_id)),
      "`animal_id` is required."
    )

    add_error(
      which(is.na(imported_data$observation_date)),
      "`observation_date` is required."
    )

    add_error(
      which(is.na(imported_data$variable_name)),
      "`variable_name` is required."
    )

    parsed_dates <- suppressWarnings(
      as.Date(
        imported_data$observation_date,
        format = "%Y-%m-%d"
      )
    )

    add_error(
      which(
        !is.na(imported_data$observation_date) &
          (
            is.na(parsed_dates) |
              format(parsed_dates, "%Y-%m-%d") !=
                imported_data$observation_date
          )
      ),
      "`observation_date` must use YYYY-MM-DD format."
    )

    has_numeric_value <- !is.na(imported_data$value_numeric)
    has_text_value <- !is.na(imported_data$value_text)

    add_error(
      which(!has_numeric_value & !has_text_value),
      "Enter either `value_numeric` or `value_text`."
    )

    add_error(
      which(has_numeric_value & has_text_value),
      "Use only one value field: `value_numeric` or `value_text`."
    )

    numeric_values <- suppressWarnings(
      as.numeric(imported_data$value_numeric)
    )

    add_error(
      which(has_numeric_value & is.na(numeric_values)),
      "`value_numeric` must be a valid number."
    )

    active_animals <- self$animals(
      include_inactive = TRUE,
      user_id = user_id
    )

    matched_animal_row <- match(
      imported_data$animal_id,
      active_animals$animal_id
    )

    add_error(
      which(
        !is.na(imported_data$animal_id) &
          is.na(matched_animal_row)
      ),
      "`animal_id` is not enrolled in the active experiment."
    )

    if (length(errors) > 0L) {
      stop(
        paste(
          c(
            "No rows were imported because the input file has validation errors:",
            errors
          ),
          collapse = "\n"
        ),
        call. = FALSE
      )
    }

    animal_pks <- active_animals$animal_pk[
      matched_animal_row
    ]

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )

    import_time <- .tm_now_utc()

    DBI::dbWithTransaction(con, {
      for (i in seq_len(nrow(imported_data))) {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO animal_vars (",
            "experiment_id,",
            "animal_pk,",
            "observation_date,",
            "variable_name,",
            "value_numeric,",
            "value_text,",
            "unit,",
            "notes,",
            "created_at_utc,",
            "updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            experiment_id,
            animal_pks[i],
            format(parsed_dates[i], "%Y-%m-%d"),
            imported_data$variable_name[i],
            numeric_values[i],
            imported_data$value_text[i],
            imported_data$unit[i],
            imported_data$notes[i],
            import_time,
            import_time
          )
        )
      }
    })

    private$touch_project()

    invisible(
      list(
        experiment_id = experiment_id,
        rows_imported = nrow(imported_data),
        file = normalizePath(
          file,
          winslash = "/",
          mustWork = TRUE
        ),
        sheet = sheet
      )
    )
  },

  #' Return Animal Variables from the Active Experiment
  vars = function(
    animal_id = NULL,
    variable_name = NULL,
    start_date = NULL,
    end_date = NULL,
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )

    query <- paste(
      "SELECT",
      "v.var_pk,",
      "v.experiment_id,",
      "a.animal_pk,",
      "a.animal_id,",
      "v.observation_date,",
      "v.variable_name,",
      "v.value_numeric,",
      "v.value_text,",
      "v.unit,",
      "v.notes,",
      "v.created_at_utc,",
      "v.updated_at_utc",
      "FROM animal_vars AS v",
      "INNER JOIN animals AS a",
      "ON v.animal_pk = a.animal_pk",
      "INNER JOIN experiment_animals AS ea",
      "ON ea.animal_pk = a.animal_pk",
      "AND ea.experiment_id = v.experiment_id"
    )

    where <- c("v.experiment_id = ?")
    params <- list(experiment_id)

    if (!is.null(animal_id)) {
      where <- c(
        where,
        "a.animal_id = ?"
      )
      params <- c(
        params,
        list(as.character(animal_id))
      )
    }

    if (!is.null(variable_name)) {
      where <- c(
        where,
        "v.variable_name = ?"
      )
      params <- c(
        params,
        list(as.character(variable_name))
      )
    }

    if (!is.null(start_date)) {
      where <- c(
        where,
        "v.observation_date >= ?"
      )
      params <- c(
        params,
        list(as.character(start_date))
      )
    }

    if (!is.null(end_date)) {
      where <- c(
        where,
        "v.observation_date <= ?"
      )
      params <- c(
        params,
        list(as.character(end_date))
      )
    }

    query <- paste(
      query,
      "WHERE",
      paste(where, collapse = " AND "),
      "ORDER BY",
      "a.animal_id,",
      "v.observation_date,",
      "v.variable_name,",
      "v.var_pk"
    )

    DBI::dbGetQuery(
      con,
      query,
      params = params
    )
  },

  #' List Active Animals in the Active Experiment
  active_animals = function(
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_active_experiment_for_user(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )

    active_animals <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "ea.experiment_animal_pk,",
        "ea.experiment_id,",
        "a.animal_pk,",
        "a.animal_id,",
        "ea.cage_id,",
        "a.sex,",
        "ea.status,",
        "ea.enrollment_date,",
        "a.killed_date,",
        "COALESCE(ea.notes, a.notes) AS notes,",
        "a.created_at_utc,",
        "a.updated_at_utc",
        "FROM experiment_animals AS ea",
        "INNER JOIN animals AS a",
        "ON a.animal_pk = ea.animal_pk",
        "WHERE ea.experiment_id = ?",
        "AND ea.status = 'Active'",
        "AND (",
        "  a.killed_date IS NULL",
        "  OR trim(a.killed_date) = ''",
        ")",
        "ORDER BY ea.cage_id, a.animal_id"
      ),
      params = list(experiment_id)
    )

    if (nrow(active_animals) == 0L) {
      message(
        "No active animals were found in experiment `",
        experiment_id,
        "`."
      )
    } else {
      message(
        "Found ",
        nrow(active_animals),
        " active animal(s) in experiment `",
        experiment_id,
        "`."
      )
    }

    active_animals
  }
)
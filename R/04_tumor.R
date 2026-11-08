# Public TumorModelProject methods for tumors and tumor measurements.
#
# Tumors belong to an experiment. Tumor measurements belong to a tumor.
#
# All methods in this file use the current user's active experiment.

.tm_tumor_public_methods <- list(

  #' Return Tumors in the Active Experiment
  #'
  #' @param animal_pk Optional database primary key for one animal.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return A data frame of tumors in the active experiment.
  tumors = function(
    animal_pk = NULL,
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_get_active_experiment_id(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )

    query <- paste(
      "SELECT",
      "  t.tumor_pk,",
      "  t.experiment_id,",
      "  t.animal_pk,",
      "  a.animal_id,",
      "  ea.cage_id,",
      "  ea.status AS animal_status,",
      "  t.tumor_label,",
      "  t.site,",
      "  t.status AS tumor_status,",
      "  t.notes,",
      "  t.created_at_utc,",
      "  t.updated_at_utc",
      "FROM tumors AS t",
      "INNER JOIN animals AS a",
      "  ON a.animal_pk = t.animal_pk",
      "INNER JOIN experiment_animals AS ea",
      "  ON ea.experiment_id = t.experiment_id",
      "  AND ea.animal_pk = t.animal_pk",
      "WHERE t.experiment_id = ?"
    )

    params <- list(experiment_id)

    if (!is.null(animal_pk)) {
      animal_pk <- suppressWarnings(as.integer(animal_pk))

      if (is.na(animal_pk) || animal_pk < 1L) {
        stop(
          "`animal_pk` must be a valid positive integer.",
          call. = FALSE
        )
      }

      query <- paste(
        query,
        "AND t.animal_pk = ?"
      )

      params <- c(
        params,
        list(animal_pk)
      )
    }

    query <- paste(
      query,
      "ORDER BY ea.cage_id, a.animal_id, t.tumor_label"
    )

    DBI::dbGetQuery(
      con,
      query,
      params = params
    )
  },

  #' Add a Tumor to an Animal in the Active Experiment
  #'
  #' @param animal_pk Database primary key for an animal enrolled in the
  #'   active experiment.
  #' @param tumor_label User-facing label, for example `"RF"` or `"Left flank"`.
  #' @param site Optional anatomical site.
  #' @param status Tumor status. Defaults to `"Active"`.
  #' @param notes Optional free-text notes.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return Invisibly, the current TumorModelProject object.
  add_tumor = function(
    animal_pk,
    tumor_label,
    site = NULL,
    status = "Active",
    notes = NULL,
    user_id = .tm_default_user_id()
  ) {
    animal_pk <- suppressWarnings(as.integer(animal_pk))
    tumor_label <- private$nullable_text(tumor_label)
    status <- private$nullable_text(status)

    if (is.na(animal_pk) || animal_pk < 1L) {
      stop(
        "`animal_pk` must be a valid positive integer.",
        call. = FALSE
      )
    }

    if (is.na(tumor_label)) {
      stop(
        "`tumor_label` is required.",
        call. = FALSE
      )
    }

    if (is.na(status)) {
      stop(
        "`status` is required.",
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

    enrolled_animal <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT a.animal_id",
        "FROM experiment_animals AS ea",
        "INNER JOIN animals AS a",
        "ON a.animal_pk = ea.animal_pk",
        "WHERE ea.experiment_id = ?",
        "AND ea.animal_pk = ?"
      ),
      params = list(
        experiment_id,
        animal_pk
      )
    )

    if (nrow(enrolled_animal) != 1L) {
      stop(
        "Animal `",
        animal_pk,
        "` is not enrolled in active experiment `",
        experiment_id,
        "`.",
        call. = FALSE
      )
    }

    now <- .tm_now_utc()

    tryCatch(
      {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO tumors (",
            "experiment_id, animal_pk, tumor_label, site, status, notes,",
            "created_at_utc, updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            experiment_id,
            animal_pk,
            tumor_label,
            private$nullable_text(site),
            status,
            private$nullable_text(notes),
            now,
            now
          )
        )
      },
      error = function(e) {
        message_text <- conditionMessage(e)

        if (grepl(
          "UNIQUE constraint failed",
          message_text,
          fixed = TRUE
        )) {
          stop(
            "Animal `",
            enrolled_animal$animal_id[[1L]],
            "` already has a tumor named `",
            tumor_label,
            "` in active experiment `",
            experiment_id,
            "`.",
            call. = FALSE
          )
        }

        if (grepl(
          "FOREIGN KEY constraint failed",
          message_text,
          fixed = TRUE
        )) {
          stop(
            "The tumor could not be saved because its animal or experiment ",
            "record does not exist.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  },

  #' Delete a Tumor from the Active Experiment
  #'
  #' Deleting a tumor also deletes its associated tumor measurements if the
  #' database foreign-key relationship uses `ON DELETE CASCADE`.
  #'
  #' @param tumor_pk Database primary key for the tumor.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return Invisibly, the current TumorModelProject object.
  delete_tumor = function(
    tumor_pk,
    user_id = .tm_default_user_id()
  ) {
    tumor_pk <- suppressWarnings(as.integer(tumor_pk))

    if (is.na(tumor_pk) || tumor_pk < 1L) {
      stop(
        "`tumor_pk` must be a valid positive integer.",
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
        "DELETE FROM tumors",
        "WHERE tumor_pk = ?",
        "AND experiment_id = ?"
      ),
      params = list(
        tumor_pk,
        experiment_id
      )
    )

    if (rows_deleted != 1L) {
      stop(
        "No tumor was deleted. It may not exist in the active experiment.",
        call. = FALSE
      )
    }

    private$touch_project()

    invisible(self)
  },

  #' Return Tumor Measurements from the Active Experiment
  #'
  #' @param tumor_pk Optional database primary key for one tumor.
  #' @param user_id User whose active experiment will be used.
  #'
  #' @return A data frame of tumor measurements.
  tumor_measurements = function(
    tumor_pk = NULL,
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_get_active_experiment_id(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )

    query <- paste(
      "SELECT",
      "  m.measurement_pk,",
      "  t.experiment_id,",
      "  m.tumor_pk,",
      "  t.animal_pk,",
      "  a.animal_id,",
      "  a.cage_id,",
      "  t.tumor_label,",
      "  t.site,",
      "  t.status AS tumor_status,",
      "  m.measurement_date,",
      "  m.length_mm,",
      "  m.width_mm,",
      "  m.depth_mm,",
      "  m.volume_mm3,",
      "  m.notes,",
      "  m.created_at_utc,",
      "  m.updated_at_utc",
      "FROM tumor_measurements AS m",
      "INNER JOIN tumors AS t",
      "  ON t.tumor_pk = m.tumor_pk",
      "INNER JOIN animals AS a",
      "  ON a.animal_pk = t.animal_pk",
      "INNER JOIN experiment_animals AS ea",
      "  ON ea.experiment_id = t.experiment_id",
      "  AND ea.animal_pk = t.animal_pk",
      "WHERE t.experiment_id = ?"
    )

    params <- list(experiment_id)

    if (!is.null(tumor_pk)) {
      tumor_pk <- suppressWarnings(as.integer(tumor_pk))

      if (is.na(tumor_pk) || tumor_pk < 1L) {
        stop(
          "`tumor_pk` must be a valid positive integer.",
          call. = FALSE
        )
      }

      query <- paste(
        query,
        "AND m.tumor_pk = ?"
      )

      params <- c(
        params,
        list(tumor_pk)
      )
    }

    query <- paste(
      query,
      "ORDER BY",
      "m.measurement_date,",
      "ea.cage_id,",
      "a.animal_id,",
      "t.tumor_label"
    )

    DBI::dbGetQuery(
      con,
      query,
      params = params
    )
  }


)
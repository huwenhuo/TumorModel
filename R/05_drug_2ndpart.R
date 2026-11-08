# Active-experiment treatment groups and treatment-group regimen links.
#
# This file must load after 05_drug.R.
#
# It extends `.tm_drug_public_methods`; do not add a second public-methods
# object to the TumorModelProject R6 class.


.tm_group_pk <- function(
  con,
  experiment_id,
  treatment_group
) {
  experiment_id <- .tm_text(
    experiment_id,
    "experiment_id",
    required = TRUE
  )

  treatment_group <- .tm_text(
    treatment_group,
    "treatment_group",
    required = TRUE
  )

  record <- .tm_one_row(
    con = con,
    query = paste(
      "SELECT treatment_group_pk",
      "FROM treatment_groups",
      "WHERE experiment_id = ?",
      "AND group_name = ?"
    ),
    params = list(
      experiment_id,
      treatment_group
    ),
    description = paste0(
      "treatment group `",
      treatment_group,
      "` in experiment `",
      experiment_id,
      "`"
    )
  )

  record$treatment_group_pk[[1L]]
}


.tm_regimen_row <- function(
  con,
  experiment_id,
  treatment_group,
  treatment_name
) {
  group_pk <- .tm_group_pk(
    con = con,
    experiment_id = experiment_id,
    treatment_group = treatment_group
  )

  treatment_pk <- .tm_treatment_pk(
    con = con,
    treatment_name = treatment_name
  )

  .tm_one_row(
    con = con,
    query = paste(
      "SELECT",
      "r.regimen_pk,",
      "r.treatment_group_pk,",
      "r.treatment_pk,",
      "r.display_order,",
      "r.schedule_description,",
      "r.start_day,",
      "r.end_day,",
      "r.notes",
      "FROM treatment_group_regimens AS r",
      "WHERE r.treatment_group_pk = ?",
      "AND r.treatment_pk = ?"
    ),
    params = list(
      group_pk,
      treatment_pk
    ),
    description = paste0(
      "regimen for treatment group `",
      treatment_group,
      "` and treatment `",
      treatment_name,
      "`"
    )
  )
}


.tm_drug_public_methods <- c(
  .tm_drug_public_methods,

  list(

    #' Return Treatment Groups in the Active Experiment
    #'
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return A data frame of treatment groups.
    treatment_groups = function(
      user_id = .tm_default_user_id()
    ) {
      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_ensure_treatment_schema(con)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "viewer"
      )

      DBI::dbGetQuery(
        con,
        paste(
          "SELECT",
          "treatment_group_pk,",
          "experiment_id,",
          "group_name,",
          "description,",
          "notes,",
          "created_at_utc,",
          "updated_at_utc",
          "FROM treatment_groups",
          "WHERE experiment_id = ?",
          "ORDER BY group_name"
        ),
        params = list(experiment_id)
      )
    },


    #' Add a Treatment Group to the Active Experiment
    #'
    #' @param treatment_group Group name, for example `"g1"` or `"Vehicle"`.
    #' @param description Optional descriptive label, for example `"PBS"`.
    #' @param notes Optional notes.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return Invisibly, the current TumorModelProject object.
    add_treatment_group = function(
      treatment_group,
      description = NULL,
      notes = NULL,
      user_id = .tm_default_user_id()
    ) {
      treatment_group <- .tm_text(
        treatment_group,
        "treatment_group",
        required = TRUE
      )

      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_ensure_treatment_schema(con)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      now <- .tm_now_utc()

      tryCatch(
        {
          DBI::dbExecute(
            con,
            paste(
              "INSERT INTO treatment_groups (",
              "experiment_id,",
              "group_name,",
              "description,",
              "notes,",
              "created_at_utc,",
              "updated_at_utc",
              ") VALUES (?, ?, ?, ?, ?, ?)"
            ),
            params = list(
              experiment_id,
              treatment_group,
              .tm_text(description, "description"),
              .tm_text(notes, "notes"),
              now,
              now
            )
          )
        },
        error = function(e) {
          if (grepl(
            "UNIQUE constraint failed",
            conditionMessage(e),
            fixed = TRUE
          )) {
            stop(
              "Treatment group `",
              treatment_group,
              "` already exists in active experiment `",
              experiment_id,
              "`.",
              call. = FALSE
            )
          }

          stop(e)
        }
      )

      private$touch_project()

      invisible(self)
    },


    #' Return Regimens for Treatment Groups in the Active Experiment
    #'
    #' @param treatment_group Optional treatment-group name.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return A data frame of treatment group regimen records.
    treatment_group_regimens = function(
      treatment_group = NULL,
      user_id = .tm_default_user_id()
    ) {
      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_ensure_treatment_schema(con)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "viewer"
      )

      query <- paste(
        "SELECT",
        "r.regimen_pk,",
        "tg.experiment_id,",
        "tg.treatment_group_pk,",
        "tg.group_name AS treatment_group,",
        "tg.description AS treatment_group_description,",
        "t.treatment_pk,",
        "t.treatment_name,",
        "d.drug,",
        "d.drug_name,",
        "dp.company_name,",
        "dp.catalog_number,",
        "dp.product_name,",
        "t.treatment_dose,",
        "t.dose_unit,",
        "t.route,",
        "t.component_volume_ul,",
        "t.vehicle,",
        "t.final_injection_volume_ul,",
        "r.display_order,",
        "r.schedule_description,",
        "r.start_day,",
        "r.end_day,",
        "r.notes,",
        "r.created_at_utc,",
        "r.updated_at_utc",
        "FROM treatment_group_regimens AS r",
        "INNER JOIN treatment_groups AS tg",
        "ON tg.treatment_group_pk = r.treatment_group_pk",
        "INNER JOIN treatments AS t",
        "ON t.treatment_pk = r.treatment_pk",
        "INNER JOIN drug_products AS dp",
        "ON dp.drug_product_pk = t.drug_product_pk",
        "INNER JOIN drugs AS d",
        "ON d.drug_pk = dp.drug_pk",
        "WHERE tg.experiment_id = ?"
      )

      params <- list(experiment_id)

      if (!is.null(treatment_group)) {
        treatment_group <- .tm_text(
          treatment_group,
          "treatment_group",
          required = TRUE
        )

        query <- paste(
          query,
          "AND tg.group_name = ?"
        )

        params <- c(
          params,
          list(treatment_group)
        )
      }

      query <- paste(
        query,
        "ORDER BY",
        "tg.group_name,",
        "r.display_order,",
        "t.treatment_name"
      )

      DBI::dbGetQuery(
        con,
        query,
        params = params
      )
    },


    #' Add a Reusable Treatment to an Active-Experiment Treatment Group
    #'
    #' @param treatment_group Group name in the active experiment.
    #' @param treatment_name Existing global treatment definition.
    #' @param display_order Positive integer controlling display order.
    #' @param schedule_description Optional schedule text.
    #' @param start_day Optional study day at which the regimen begins.
    #' @param end_day Optional study day at which the regimen ends.
    #' @param notes Optional regimen notes.
    #' @param user_id User whose active experiment will be used.
    #'
    #' @return Invisibly, the current TumorModelProject object.
    add_treatment_to_group = function(
      treatment_group,
      treatment_name,
      display_order = 1L,
      schedule_description = NULL,
      start_day = NULL,
      end_day = NULL,
      notes = NULL,
      user_id = .tm_default_user_id()
    ) {
      treatment_group <- .tm_text(
        treatment_group,
        "treatment_group",
        required = TRUE
      )

      treatment_name <- .tm_text(
        treatment_name,
        "treatment_name",
        required = TRUE
      )

      display_order <- .tm_optional_integer(
        display_order,
        "display_order"
      )

      if (is.na(display_order) || display_order < 1L) {
        stop(
          "`display_order` must be a positive integer.",
          call. = FALSE
        )
      }

      start_day <- .tm_optional_integer(
        start_day,
        "start_day"
      )

      end_day <- .tm_optional_integer(
        end_day,
        "end_day"
      )

      if (
        !is.na(start_day) &&
          !is.na(end_day) &&
          end_day < start_day
      ) {
        stop(
          "`end_day` cannot be before `start_day`.",
          call. = FALSE
        )
      }

      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_ensure_treatment_schema(con)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      group_pk <- .tm_group_pk(
        con = con,
        experiment_id = experiment_id,
        treatment_group = treatment_group
      )

      treatment_pk <- .tm_treatment_pk(
        con = con,
        treatment_name = treatment_name
      )

      now <- .tm_now_utc()

      tryCatch(
        {
          DBI::dbExecute(
            con,
            paste(
              "INSERT INTO treatment_group_regimens (",
              "treatment_group_pk,",
              "treatment_pk,",
              "display_order,",
              "schedule_description,",
              "start_day,",
              "end_day,",
              "notes,",
              "created_at_utc,",
              "updated_at_utc",
              ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
            ),
            params = list(
              group_pk,
              treatment_pk,
              display_order,
              .tm_text(
                schedule_description,
                "schedule_description"
              ),
              start_day,
              end_day,
              .tm_text(notes, "notes"),
              now,
              now
            )
          )
        },
        error = function(e) {
          if (grepl(
            "UNIQUE constraint failed",
            conditionMessage(e),
            fixed = TRUE
          )) {
            stop(
              "Treatment `",
              treatment_name,
              "` is already assigned to treatment group `",
              treatment_group,
              "` in the active experiment.",
              call. = FALSE
            )
          }

          stop(e)
        }
      )

      private$touch_project()

      invisible(self)
    },


    #' Remove a Treatment from an Active-Experiment Treatment Group
    remove_treatment_from_group = function(
      treatment_group,
      treatment_name,
      user_id = .tm_default_user_id()
    ) {
      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_ensure_treatment_schema(con)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      group_pk <- .tm_group_pk(
        con = con,
        experiment_id = experiment_id,
        treatment_group = treatment_group
      )

      treatment_pk <- .tm_treatment_pk(
        con = con,
        treatment_name = treatment_name
      )

      rows_deleted <- DBI::dbExecute(
        con,
        paste(
          "DELETE FROM treatment_group_regimens",
          "WHERE treatment_group_pk = ?",
          "AND treatment_pk = ?"
        ),
        params = list(
          group_pk,
          treatment_pk
        )
      )

      if (rows_deleted != 1L) {
        stop(
          "Treatment `",
          treatment_name,
          "` is not assigned to treatment group `",
          treatment_group,
          "` in the active experiment.",
          call. = FALSE
        )
      }

      private$touch_project()

      invisible(self)
    },


    #' Update an Active-Experiment Treatment Group Regimen
    update_treatment_group_regimen = function(
      treatment_group,
      treatment_name,
      display_order = NULL,
      schedule_description = NULL,
      start_day = NULL,
      end_day = NULL,
      notes = NULL,
      user_id = .tm_default_user_id()
    ) {
      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_ensure_treatment_schema(con)

      experiment_id <- .tm_get_active_experiment_id(
        con = con,
        user_id = user_id,
        minimum_role = "editor"
      )

      regimen <- .tm_regimen_row(
        con = con,
        experiment_id = experiment_id,
        treatment_group = treatment_group,
        treatment_name = treatment_name
      )

      set_clauses <- character(0)
      params <- list()

      if (!is.null(display_order)) {
        display_order <- .tm_optional_integer(
          display_order,
          "display_order"
        )

        if (is.na(display_order) || display_order < 1L) {
          stop(
            "`display_order` must be a positive integer.",
            call. = FALSE
          )
        }

        set_clauses <- c(
          set_clauses,
          "display_order = ?"
        )

        params <- c(
          params,
          list(display_order)
        )
      }

      if (!is.null(schedule_description)) {
        set_clauses <- c(
          set_clauses,
          "schedule_description = ?"
        )

        params <- c(
          params,
          list(
            .tm_text(
              schedule_description,
              "schedule_description"
            )
          )
        )
      }

      if (!is.null(start_day)) {
        set_clauses <- c(
          set_clauses,
          "start_day = ?"
        )

        params <- c(
          params,
          list(
            .tm_optional_integer(
              start_day,
              "start_day"
            )
          )
        )
      }

      if (!is.null(end_day)) {
        set_clauses <- c(
          set_clauses,
          "end_day = ?"
        )

        params <- c(
          params,
          list(
            .tm_optional_integer(
              end_day,
              "end_day"
            )
          )
        )
      }

      if (!is.null(notes)) {
        set_clauses <- c(
          set_clauses,
          "notes = ?"
        )

        params <- c(
          params,
          list(.tm_text(notes, "notes"))
        )
      }

      if (length(set_clauses) == 0L) {
        stop(
          "Provide at least one regimen field to update.",
          call. = FALSE
        )
      }

      updated_start_day <- if (is.null(start_day)) {
        regimen$start_day[[1L]]
      } else {
        .tm_optional_integer(start_day, "start_day")
      }

      updated_end_day <- if (is.null(end_day)) {
        regimen$end_day[[1L]]
      } else {
        .tm_optional_integer(end_day, "end_day")
      }

      if (
        !is.na(updated_start_day) &&
          !is.na(updated_end_day) &&
          updated_end_day < updated_start_day
      ) {
        stop(
          "`end_day` cannot be before `start_day`.",
          call. = FALSE
        )
      }

      set_clauses <- c(
        set_clauses,
        "updated_at_utc = ?"
      )

      params <- c(
        params,
        list(
          .tm_now_utc(),
          regimen$regimen_pk[[1L]]
        )
      )

      DBI::dbExecute(
        con,
        paste(
          "UPDATE treatment_group_regimens",
          "SET",
          paste(set_clauses, collapse = ", "),
          "WHERE regimen_pk = ?"
        ),
        params = params
      )

      private$touch_project()

      invisible(self)
    }
  )
)
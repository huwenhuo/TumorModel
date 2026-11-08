#' Tumor Model Study Project
#'
#' An R6 object representing one persistent animal tumor-model study.
#' Study data are stored in a SQLite database in the study folder.
#'
#' @field path Path to the study project folder.
#' @field db_path Path to the study SQLite database.
#'
#' @export
TumorModelProject <- R6::R6Class(
  classname = "TumorModelProject",

  public = c(
    list(
      path = NULL,
      db_path = NULL,

      #' Initialize a Tumor Model Project
      #'
      #' @param path Path to an existing tumor-model study project folder.
      #'
      #' @return A newly initialized `TumorModelProject` object.
      initialize = function(path) {
        self$path <- normalizePath(path, mustWork = TRUE)
        self$db_path <- file.path(self$path, "study.sqlite")

        if (!file.exists(self$db_path)) {
          stop(
            "No study.sqlite database was found in: ", self$path,
            "\nUse create_tumor_project() to create a new study.",
            call. = FALSE
          )
        }

        # Ensures new tables are added when an older study is opened.
        private$initialize_schema()
      },

      #' Return Project Metadata
      #'
      #' @return A one-row data frame containing project metadata.
      project_info = function() {
        con <- private$connect()
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        DBI::dbGetQuery(con, "SELECT * FROM project LIMIT 1")
      },
        
      #' Return Model Setup
      #'
      #' @return A one-row data frame containing tumor-model setup metadata,
      #'   or an empty data frame if setup has not been saved.
      model_setup = function() {
        con <- private$connect()
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        DBI::dbGetQuery(
          con,
          "SELECT * FROM model_setup WHERE setup_id = 1"
        )
      },

      #' Save Tumor Model Setup Metadata
      #'
      #' Replaces the stored model-setup values with the supplied values.
      #'
      #' @param experiment_start_date Optional experiment start date.
      #' @param inoculation_date Optional tumor-cell inoculation date.
      #' @param treatment_start_date Optional treatment start date.
      #' @param cell_line Optional tumor cell-line name.
      #' @param cells_per_animal Optional number of cells inoculated per animal.
      #' @param inoculation_route Optional inoculation route.
      #' @param inoculation_site Optional inoculation site.
      #' @param inoculation_volume_ul Optional inoculation volume in microliters.
      #' @param matrix_vehicle Optional matrix or vehicle description.
      #' @param species Optional animal species.
      #' @param strain Optional animal strain.
      #' @param sex Optional study-level animal sex.
      #' @param protocol_id Optional protocol or IACUC identifier.
      #' @param notes Optional free-text study notes.
      #'
      #' @return Invisibly, the current [TumorModelProject] object.
      set_model_setup = function(
        experiment_start_date = NULL,
        inoculation_date = NULL,
        treatment_start_date = NULL,
        cell_line = NULL,
        cells_per_animal = NULL,
        inoculation_route = NULL,
        inoculation_site = NULL,
        inoculation_volume_ul = NULL,
        matrix_vehicle = NULL,
        species = NULL,
        strain = NULL,
        sex = NULL,
        protocol_id = NULL,
        notes = NULL
      ) {
        con <- private$connect()
        on.exit(DBI::dbDisconnect(con), add = TRUE)

        now <- .tm_now_utc()

        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO model_setup (",
            "setup_id, experiment_start_date, inoculation_date,",
            "treatment_start_date, cell_line, cells_per_animal,",
            "inoculation_route, inoculation_site, inoculation_volume_ul,",
            "matrix_vehicle, species, strain, sex, protocol_id, notes,",
            "updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
            "ON CONFLICT(setup_id) DO UPDATE SET",
            "experiment_start_date = excluded.experiment_start_date,",
            "inoculation_date = excluded.inoculation_date,",
            "treatment_start_date = excluded.treatment_start_date,",
            "cell_line = excluded.cell_line,",
            "cells_per_animal = excluded.cells_per_animal,",
            "inoculation_route = excluded.inoculation_route,",
            "inoculation_site = excluded.inoculation_site,",
            "inoculation_volume_ul = excluded.inoculation_volume_ul,",
            "matrix_vehicle = excluded.matrix_vehicle,",
            "species = excluded.species,",
            "strain = excluded.strain,",
            "sex = excluded.sex,",
            "protocol_id = excluded.protocol_id,",
            "notes = excluded.notes,",
            "updated_at_utc = excluded.updated_at_utc"
          ),
          params = list(
            1L,
            private$nullable_text(experiment_start_date),
            private$nullable_text(inoculation_date),
            private$nullable_text(treatment_start_date),
            private$nullable_text(cell_line),
            private$nullable_number(cells_per_animal),
            private$nullable_text(inoculation_route),
            private$nullable_text(inoculation_site),
            private$nullable_number(inoculation_volume_ul),
            private$nullable_text(matrix_vehicle),
            private$nullable_text(species),
            private$nullable_text(strain),
            private$nullable_text(sex),
            private$nullable_text(protocol_id),
            private$nullable_text(notes),
            now
          )
        )

        private$touch_project()
        invisible(self)
      }
    ),

    # This list is defined in R/02_animal.R.
    .tm_experiment_public_methods,

    .tm_animal_public_methods,

    .tm_tumor_public_methods,
      
    .tm_drug_public_methods,
      
    .tm_treatment_public_methods,

    .tm_import_public_methods, 
      
    .tm_plot_public_methods, 

    list(
      #' Print a Tumor Model Project Summary
      #'
      #' @param ... Additional arguments, currently ignored.
      #'
      #' @return Invisibly, the current [TumorModelProject] object.
      print = function(...) {
        info <- self$project_info()

        cat("<TumorModelProject>\n")
        cat("  Title: ", info$title, "\n", sep = "")
        cat("  Path:  ", self$path, "\n", sep = "")
        cat("  DB:    ", self$db_path, "\n", sep = "")

        setup <- self$model_setup()

        if (nrow(setup) == 1 && !is.na(setup$cell_line)) {
          cat("  Model: ", setup$cell_line, "\n", sep = "")
        }

        invisible(self)
      }
    )
  ),

  private = list(
    connect = function() {
      .tm_connect(self$db_path)
    },

    initialize_schema = function() {
      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      .tm_initialize_schema(con)
    },

    touch_project = function() {
      con <- private$connect()
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      DBI::dbExecute(
        con,
        "UPDATE project SET updated_at_utc = ? WHERE project_id = 1",
        params = list(.tm_now_utc())
      )
    },

    nullable_text = function(value) {
      if (
        is.null(value) ||
        length(value) == 0 ||
        is.na(value) ||
        identical(trimws(as.character(value)), "")
      ) {
        return(NA_character_)
      }

      trimws(as.character(value))
    },

    nullable_number = function(value) {
      if (
        is.null(value) ||
        length(value) == 0 ||
        is.na(value) ||
        identical(trimws(as.character(value)), "")
      ) {
        return(NA_real_)
      }

      value <- suppressWarnings(as.numeric(value))

      if (is.na(value)) {
        stop("A numeric value was expected.", call. = FALSE)
      }

      value
    }
  )
)

#' Create a New Tumor Model Study Project
#'
#' Creates a project folder and initializes its SQLite study database.
#'
#' @param path Path for the new study project folder.
#' @param title Study title.
#' @param overwrite Whether to replace an existing `study.sqlite` database.
#'
#' @return A [TumorModelProject] object.
#' @export
create_tumor_project <- function(path, title, overwrite = FALSE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  path <- normalizePath(path, mustWork = TRUE)
  db_path <- file.path(path, "study.sqlite")

  if (file.exists(db_path)) {
    if (!isTRUE(overwrite)) {
      stop(
        "A study database already exists at: ", db_path,
        "\nOpen it with open_tumor_project() instead.",
        call. = FALSE
      )
    }

    unlink(db_path)
  }

  .tm_create_database(
    db_path = db_path,
    title = title
  )

  TumorModelProject$new(path)
}

#' Open an Existing Tumor Model Study Project
#'
#' @param path Path to an existing study project folder.
#'
#' @return A [TumorModelProject] object.
#' @export
open_tumor_project <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  db_path <- file.path(path, "study.sqlite")

  if (!file.exists(db_path)) {
    stop(
      "No study.sqlite database was found in: ", path,
      "\nUse create_tumor_project() to create a new study.",
      call. = FALSE
    )
  }

  TumorModelProject$new(path)
}
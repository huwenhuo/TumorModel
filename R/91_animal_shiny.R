#' Animal, Tumor, and Measurement Shiny Module UI
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny tag list.
#' @export
animal_roster_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::tabsetPanel(
      id = ns("animal_tabs"),

      # =====================================================================
      # Animal roster
      # =====================================================================
      shiny::tabPanel(
        title = "Animal roster",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 12,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("add_animal_rows"),
                label = "Add animals",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              ),

              shiny::actionButton(
                inputId = ns("refresh_animals"),
                label = "Refresh",
                icon = shiny::icon("rotate")
              ),

              shiny::actionButton(
                inputId = ns("delete_animal"),
                label = "Delete selected animal",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            ),

            shiny::uiOutput(ns("animal_entry_ui")),

            shiny::h4("Saved animal roster"),

            shiny::p(
              "To edit a saved animal, double-click an editable table cell. ",
              "Animal record ID, experiment ID, and timestamps cannot be edited."
            ),

            DT::DTOutput(ns("animal_table"))
          )
        )
      ),

      # =====================================================================
      # Tumors
      # =====================================================================
      shiny::tabPanel(
        title = "Tumors",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add tumor"),

              shiny::selectInput(
                inputId = ns("tumor_animal_pk"),
                label = "Animal",
                choices = character(0)
              ),

              shiny::textInput(
                inputId = ns("tumor_label"),
                label = "Tumor label",
                placeholder = "For example: Right flank"
              ),

              shiny::textInput(
                inputId = ns("tumor_site"),
                label = "Anatomical site (optional)",
                placeholder = "For example: Right flank"
              ),

              shiny::selectInput(
                inputId = ns("tumor_status"),
                label = "Tumor status",
                choices = c("Active", "Inactive", "Resolved"),
                selected = "Active"
              ),

              shiny::textAreaInput(
                inputId = ns("tumor_notes"),
                label = "Notes (optional)",
                rows = 3
              ),

              shiny::actionButton(
                inputId = ns("add_tumor"),
                label = "Add tumor",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              ),

              shiny::hr(),

              shiny::actionButton(
                inputId = ns("add_right_flank_all"),
                label = "Add Right flank to all active animals",
                icon = shiny::icon("plus")
              )
            )
          ),

          shiny::column(
            width = 8,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("refresh_tumors"),
                label = "Refresh tumors",
                icon = shiny::icon("rotate")
              ),

              shiny::actionButton(
                inputId = ns("delete_tumor"),
                label = "Delete selected tumor",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            ),

            shiny::h4("Saved tumors"),

            shiny::p(
              class = "text-danger",
              "Deleting a tumor also permanently deletes its measurements."
            ),

            DT::DTOutput(ns("tumor_table"))
          )
        )
      ),

      # =====================================================================
      # Tumor measurements
      # =====================================================================
      shiny::tabPanel(
        title = "Tumor measurements",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("New measurements"),

              shiny::dateInput(
                inputId = ns("measurement_date"),
                label = "Measurement date",
                value = Sys.Date(),
                format = "yyyy-mm-dd"
              ),

              shiny::p(
                "Creates a worksheet for active tumors that do not already ",
                "have a saved measurement on the selected date."
              ),

              shiny::actionButton(
                inputId = ns("prepare_measurements"),
                label = "Prepare measurement sheet",
                icon = shiny::icon("table"),
                class = "btn-primary"
              ),

              shiny::hr(),

              shiny::tags$p(
                shiny::tags$strong("Volume formula: "),
                "(largest dimension × smaller dimension²) / 2"
              ),

              shiny::tags$p(
                "Dimensions may be entered in either order. The study method ",
                "stores the larger value as length and the smaller as width."
              )
            )
          ),

          shiny::column(
            width = 8,
            shiny::uiOutput(ns("measurement_entry_ui"))
          )
        ),

        shiny::hr(),

        shiny::fluidRow(
          shiny::column(
            width = 12,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("refresh_measurements"),
                label = "Refresh measurements",
                icon = shiny::icon("rotate")
              ),

              shiny::actionButton(
                inputId = ns("delete_measurement"),
                label = "Delete selected measurement",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            ),

            shiny::h4("Saved tumor measurements"),

            DT::DTOutput(ns("measurement_table"))
          )
        )
      )
    )
  )
}


#' Animal, Tumor, and Measurement Shiny Module Server
#'
#' @param id Shiny module identifier.
#' @param study A TumorModelProject object.
#'
#' @return Invisibly, NULL.
#' @export
animal_roster_server <- function(id, study) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!inherits(study, "TumorModelProject")) {
        stop("`study` must be a TumorModelProject object.", call. = FALSE)
      }

      # ---------------------------------------------------------------------
      # Reactive data and table proxies
      # ---------------------------------------------------------------------

      animals_data <- shiny::reactiveVal(study$animals())
      tumors_data <- shiny::reactiveVal(study$tumors())
      measurements_data <- shiny::reactiveVal(study$tumor_measurements())

      draft_animals <- shiny::reactiveVal(NULL)
      draft_measurements <- shiny::reactiveVal(NULL)

      animal_to_delete <- shiny::reactiveVal(NULL)
      tumor_to_delete <- shiny::reactiveVal(NULL)
      measurement_to_delete <- shiny::reactiveVal(NULL)

      animal_table_proxy <- DT::dataTableProxy("animal_table", session)
      tumor_table_proxy <- DT::dataTableProxy("tumor_table", session)
      measurement_table_proxy <- DT::dataTableProxy(
        "measurement_table",
        session
      )

      # ---------------------------------------------------------------------
      # Refresh helpers
      # ---------------------------------------------------------------------

      refresh_animal_choices <- function() {
        data <- animals_data()

        if (nrow(data) == 0L) {
          shiny::updateSelectInput(
            session,
            "tumor_animal_pk",
            choices = character(0)
          )
          return(invisible(NULL))
        }

        active_animals <- data[
          tolower(data$status) == "active",
          ,
          drop = FALSE
        ]

        if (nrow(active_animals) == 0L) {
          active_animals <- data
        }

        cage_label <- ifelse(
          is.na(active_animals$cage_id) | active_animals$cage_id == "",
          "not recorded",
          active_animals$cage_id
        )

        choices <- stats::setNames(
          as.character(active_animals$animal_pk),
          paste0(
            active_animals$animal_id,
            " [Cage: ",
            cage_label,
            "]"
          )
        )

        shiny::updateSelectInput(
          session,
          "tumor_animal_pk",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_animals <- function(reset_paging = FALSE) {
        data <- study$animals()
        animals_data(data)

        DT::replaceData(
          proxy = animal_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        refresh_animal_choices()
        invisible(NULL)
      }

      refresh_tumors <- function(reset_paging = FALSE) {
        data <- study$tumors()
        tumors_data(data)

        DT::replaceData(
          proxy = tumor_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        invisible(NULL)
      }

      refresh_measurements <- function(reset_paging = FALSE) {
        data <- study$tumor_measurements()
        measurements_data(data)

        DT::replaceData(
          proxy = measurement_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        invisible(NULL)
      }

      refresh_everything <- function() {
        refresh_animals()
        refresh_tumors()
        refresh_measurements()
        invisible(NULL)
      }

      shiny::observe({
        refresh_animal_choices()
      })

      # =====================================================================
      # Animal roster
      # =====================================================================

      output$animal_table <- DT::renderDT({
        DT::datatable(
          animals_data(),
          rownames = FALSE,
          selection = "single",
          editable = "cell",

          options = list(
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
            autoWidth = TRUE
          )
        )
      })

      shiny::observeEvent(input$refresh_animals, {
        refresh_animals()
      })

      shiny::observeEvent(input$animal_table_cell_edit, {
        edit <- input$animal_table_cell_edit
        data <- animals_data()

        if (is.null(edit) || nrow(data) == 0L) {
          return()
        }

        column_name <- names(data)[edit$col + 1L]

        editable_columns <- c(
          "cage_id",
          "animal_id",
          "species",
          "strain",
          "age_weeks",
          "sex",
          "status",
          "killed_date",
          "notes"
        )

        if (!column_name %in% editable_columns) {
          refresh_animals()
          return()
        }

        animal_pk <- data$animal_pk[[edit$row]]

        tryCatch(
          {
            if (identical(column_name, "killed_date")) {
              study$set_animal_killed_date(
                animal_pk = animal_pk,
                killed_date = edit$value
              )
            } else {
              study$update_animal_field(
                animal_pk = animal_pk,
                field = column_name,
                value = edit$value
              )
            }

            shiny::showNotification(
              "Animal record updated.",
              type = "message",
              duration = 2
            )
          },
          error = function(e) {
            shiny::showNotification(
              conditionMessage(e),
              type = "error",
              duration = NULL
            )
          }
        )

        refresh_animals()
      })

      # ---------------------------------------------------------------------
      # Add animals
      # ---------------------------------------------------------------------

      shiny::observeEvent(input$add_animal_rows, {
        shiny::showModal(
          shiny::modalDialog(
            title = "Add animal rows",

            shiny::numericInput(
              inputId = session$ns("number_of_animal_rows"),
              label = "Number of animals to add",
              value = 10,
              min = 1,
              max = 500,
              step = 1
            ),

            shiny::p(
              "These rows are temporary until you select ",
              "'Save animals to database'."
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),

              shiny::actionButton(
                inputId = session$ns("create_animal_rows"),
                label = "Create rows",
                class = "btn-primary"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$create_animal_rows, {
        n <- suppressWarnings(as.integer(input$number_of_animal_rows))

        if (is.na(n) || n < 1L || n > 500L) {
          shiny::showNotification(
            "Enter a whole number from 1 through 500.",
            type = "error",
            duration = NULL
          )
          return()
        }

        draft_animals(
          data.frame(
            cage_id = rep("", n),
            animal_id = rep("", n),
            species = rep("", n),
            strain = rep("", n),
            age_weeks = rep("", n),
            sex = rep("", n),
            notes = rep("", n),
            stringsAsFactors = FALSE
          )
        )

        shiny::removeModal()
      })

      output$animal_entry_ui <- shiny::renderUI({
        if (is.null(draft_animals())) {
          return(NULL)
        }

        shiny::wellPanel(
          shiny::h4("New animal entries"),

          shiny::p(
            "Click a cell to type; use Tab or paste rows copied from Excel."
          ),

          rhandsontable::rHandsontableOutput(
            outputId = session$ns("new_animals_grid"),
            width = "100%",
            height = 450
          ),

          shiny::br(),

          shiny::actionButton(
            inputId = session$ns("save_animal_rows"),
            label = "Save animals to database",
            icon = shiny::icon("floppy-disk"),
            class = "btn-primary"
          ),

          shiny::actionButton(
            inputId = session$ns("discard_animal_rows"),
            label = "Discard unsaved rows",
            icon = shiny::icon("trash")
          )
        )
      })

      output$new_animals_grid <- rhandsontable::renderRHandsontable({
        draft <- draft_animals()

        if (is.null(draft)) {
          return(NULL)
        }

        rhandsontable::rhandsontable(
          draft,
          rowHeaders = TRUE,
          colHeaders = c(
            "Cage ID",
            "Animal ID",
            "Species",
            "Strain",
            "Age (weeks)",
            "Sex",
            "Notes"
          ),
          stretchH = "all",
          contextMenu = TRUE,
          manualColumnResize = TRUE,
          fillHandle = TRUE,
          height = 400
        )
      })

      shiny::observeEvent(input$discard_animal_rows, {
        draft_animals(NULL)
      })

      shiny::observeEvent(input$save_animal_rows, {
        if (is.null(input$new_animals_grid)) {
          return()
        }

        draft <- rhandsontable::hot_to_r(input$new_animals_grid)

        if (is.null(draft) || nrow(draft) == 0L) {
          return()
        }

        for (field in names(draft)) {
          draft[[field]] <- trimws(as.character(draft[[field]]))
        }

        missing_ids <- which(is.na(draft$animal_id) | draft$animal_id == "")

        if (length(missing_ids) > 0L) {
          shiny::showNotification(
            paste(
              "Animal ID is required for row(s):",
              paste(missing_ids, collapse = ", ")
            ),
            type = "error",
            duration = NULL
          )
          return()
        }

        duplicate_ids <- unique(draft$animal_id[duplicated(draft$animal_id)])

        if (length(duplicate_ids) > 0L) {
          shiny::showNotification(
            paste(
              "Duplicate Animal ID(s) in new rows:",
              paste(duplicate_ids, collapse = ", ")
            ),
            type = "error",
            duration = NULL
          )
          return()
        }

        saved_count <- 0L

        success <- tryCatch(
          {
            for (i in seq_len(nrow(draft))) {
              age_weeks <- if (nzchar(draft$age_weeks[[i]])) {
                suppressWarnings(as.numeric(draft$age_weeks[[i]]))
              } else {
                NULL
              }

              study$add_animal(
                animal_id = draft$animal_id[[i]],
                cage_id = draft$cage_id[[i]],
                species = draft$species[[i]],
                strain = draft$strain[[i]],
                age_weeks = age_weeks,
                sex = draft$sex[[i]],
                notes = draft$notes[[i]]
              )

              saved_count <- saved_count + 1L
            }

            TRUE
          },
          error = function(e) {
            shiny::showNotification(
              paste0(
                "Saving stopped after ",
                saved_count,
                " row(s). Error: ",
                conditionMessage(e)
              ),
              type = "error",
              duration = NULL
            )

            FALSE
          }
        )

        if (isTRUE(success)) {
          draft_animals(NULL)
          refresh_animals()

          shiny::showNotification(
            paste0(saved_count, " animal row(s) saved."),
            type = "message",
            duration = 4
          )
        }
      })

      # ---------------------------------------------------------------------
      # Delete animal
      # ---------------------------------------------------------------------

      shiny::observeEvent(input$delete_animal, {
        selected <- input$animal_table_rows_selected
        data <- animals_data()

        if (length(selected) != 1L || nrow(data) == 0L) {
          shiny::showNotification(
            "Select one animal before deleting.",
            type = "error"
          )
          return()
        }

        animal_to_delete(data[selected, , drop = FALSE])

        shiny::showModal(
          shiny::modalDialog(
            title = "Delete animal?",

            shiny::tags$p(
              "Permanently delete animal ",
              shiny::tags$strong(data$animal_id[[selected]]),
              "?"
            ),

            shiny::tags$p(
              class = "text-danger",
              "Associated tumors and measurements will also be deleted."
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                session$ns("confirm_delete_animal"),
                "Permanently delete",
                class = "btn-danger"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_delete_animal, {
        selected <- animal_to_delete()

        if (is.null(selected)) {
          return()
        }

        tryCatch(
          {
            study$delete_animal(animal_pk = selected$animal_pk[[1L]])
            shiny::removeModal()
            animal_to_delete(NULL)
            refresh_everything()
          },
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
      })

      # =====================================================================
      # Tumors
      # =====================================================================

      output$tumor_table <- DT::renderDT({
        DT::datatable(
          tumors_data(),
          rownames = FALSE,
          selection = "single",
          options = list(pageLength = 25, autoWidth = TRUE)
        )
      })

      shiny::observeEvent(input$refresh_tumors, {
        refresh_tumors()
      })

      shiny::observeEvent(input$add_tumor, {
        animal_pk <- suppressWarnings(as.integer(input$tumor_animal_pk))

        if (is.na(animal_pk)) {
          shiny::showNotification(
            "Select an animal before adding a tumor.",
            type = "error"
          )
          return()
        }

        tryCatch(
          {
            study$add_tumor(
              animal_pk = animal_pk,
              tumor_label = input$tumor_label,
              site = input$tumor_site,
              status = input$tumor_status,
              notes = input$tumor_notes
            )

            shiny::updateTextInput(session, "tumor_label", value = "")
            shiny::updateTextInput(session, "tumor_site", value = "")
            shiny::updateTextAreaInput(session, "tumor_notes", value = "")

            refresh_tumors()

            shiny::showNotification(
              "Tumor added.",
              type = "message",
              duration = 3
            )
          },
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
      })

      shiny::observeEvent(input$add_right_flank_all, {
        animals <- study$animals()
        tumors <- study$tumors()

        active_animals <- animals[
          tolower(animals$status) == "active",
          ,
          drop = FALSE
        ]

        existing <- tumors$animal_pk[
          tolower(trimws(tumors$tumor_label)) == "right flank"
        ]

        to_add <- active_animals[
          !active_animals$animal_pk %in% existing,
          ,
          drop = FALSE
        ]

        if (nrow(to_add) == 0L) {
          shiny::showNotification(
            "All active animals already have a Right flank tumor.",
            type = "message"
          )
          return()
        }

        tryCatch(
          {
            for (i in seq_len(nrow(to_add))) {
              study$add_tumor(
                animal_pk = to_add$animal_pk[[i]],
                tumor_label = "Right flank",
                site = "Right flank",
                status = "Active"
              )
            }

            refresh_tumors()

            shiny::showNotification(
              paste0("Added Right flank tumors for ", nrow(to_add), " animal(s)."),
              type = "message",
              duration = 4
            )
          },
          error = function(e) {
            refresh_tumors()
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
      })

      shiny::observeEvent(input$delete_tumor, {
        selected <- input$tumor_table_rows_selected
        data <- tumors_data()

        if (length(selected) != 1L || nrow(data) == 0L) {
          shiny::showNotification(
            "Select one tumor before deleting.",
            type = "error"
          )
          return()
        }

        tumor_to_delete(data[selected, , drop = FALSE])

        shiny::showModal(
          shiny::modalDialog(
            title = "Delete tumor?",

            shiny::tags$p(
              class = "text-danger",
              "All measurements for this tumor will also be deleted."
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                session$ns("confirm_delete_tumor"),
                "Permanently delete",
                class = "btn-danger"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_delete_tumor, {
        selected <- tumor_to_delete()

        if (is.null(selected)) {
          return()
        }

        tryCatch(
          {
            study$delete_tumor(tumor_pk = selected$tumor_pk[[1L]])
            shiny::removeModal()
            tumor_to_delete(NULL)
            refresh_tumors()
            refresh_measurements()
          },
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
      })

      # =====================================================================
      # Tumor measurements
      # =====================================================================

      shiny::observeEvent(input$prepare_measurements, {
        measurement_date <- as.character(input$measurement_date)
        tumors <- study$tumors()
        measurements <- study$tumor_measurements()

        active_tumors <- tumors[
          tolower(tumors$status) == "active",
          ,
          drop = FALSE
        ]

        already_measured <- measurements$tumor_pk[
          measurements$measurement_date == measurement_date
        ]

        to_measure <- active_tumors[
          !active_tumors$tumor_pk %in% already_measured,
          ,
          drop = FALSE
        ]

        to_measure <- to_measure[
          order(to_measure$animal_id, to_measure$tumor_label),
          ,
          drop = FALSE
        ]

        if (nrow(to_measure) == 0L) {
          draft_measurements(NULL)

          shiny::showNotification(
            "No active tumors require a measurement on this date.",
            type = "message"
          )
          return()
        }

        draft_measurements(
          data.frame(
            tumor_pk = to_measure$tumor_pk,
            animal_id = to_measure$animal_id,
            tumor_label = to_measure$tumor_label,
            site = ifelse(is.na(to_measure$site), "", to_measure$site),
            dimension_1_mm = "",
            dimension_2_mm = "",
            notes = "",
            stringsAsFactors = FALSE
          )
        )
      })

      output$measurement_entry_ui <- shiny::renderUI({
        if (is.null(draft_measurements())) {
          return(
            shiny::wellPanel(
              shiny::h4("Measurement spreadsheet"),
              shiny::p(
                "Choose a date and select ",
                shiny::tags$strong("Prepare measurement sheet"),
                "."
              )
            )
          )
        }

        shiny::wellPanel(
          shiny::h4("Measurement spreadsheet"),

          rhandsontable::rHandsontableOutput(
            session$ns("measurement_grid"),
            width = "100%",
            height = 500
          ),

          shiny::br(),

          shiny::actionButton(
            session$ns("save_measurements"),
            "Save measurements",
            icon = shiny::icon("floppy-disk"),
            class = "btn-primary"
          ),

          shiny::actionButton(
            session$ns("discard_measurements"),
            "Discard sheet",
            icon = shiny::icon("trash")
          )
        )
      })

      output$measurement_grid <- rhandsontable::renderRHandsontable({
        draft <- draft_measurements()

        if (is.null(draft)) {
          return(NULL)
        }

        rhandsontable::rhandsontable(
          draft,
          rowHeaders = TRUE,
          colHeaders = c(
            "Tumor record ID",
            "Animal ID",
            "Tumor label",
            "Site",
            "Dimension 1 (mm)",
            "Dimension 2 (mm)",
            "Notes"
          ),
          readOnly = FALSE,
          stretchH = "all",
          contextMenu = TRUE,
          manualColumnResize = TRUE,
          fillHandle = TRUE,
          height = 450
        ) |>
          rhandsontable::hot_col(
            column = 1:4,
            readOnly = TRUE
          )
      })

      shiny::observeEvent(input$discard_measurements, {
        draft_measurements(NULL)
      })

      shiny::observeEvent(input$save_measurements, {
        if (is.null(input$measurement_grid)) {
          return()
        }

        measurement_date <- as.character(input$measurement_date)
        draft <- rhandsontable::hot_to_r(input$measurement_grid)

        entered_1 <- !is.na(draft$dimension_1_mm) &
          nzchar(trimws(as.character(draft$dimension_1_mm)))

        entered_2 <- !is.na(draft$dimension_2_mm) &
          nzchar(trimws(as.character(draft$dimension_2_mm)))

        incomplete <- which(xor(entered_1, entered_2))

        if (length(incomplete) > 0L) {
          shiny::showNotification(
            paste(
              "Both dimensions are required for row(s):",
              paste(incomplete, collapse = ", ")
            ),
            type = "error",
            duration = NULL
          )
          return()
        }

        rows_to_save <- which(entered_1 & entered_2)

        if (length(rows_to_save) == 0L) {
          shiny::showNotification(
            "Enter two dimensions for at least one tumor.",
            type = "error"
          )
          return()
        }

        saved_count <- 0L

        success <- tryCatch(
          {
            for (i in rows_to_save) {
              study$add_tumor_measurement(
                tumor_pk = draft$tumor_pk[[i]],
                measurement_date = measurement_date,
                length_mm = draft$dimension_1_mm[[i]],
                width_mm = draft$dimension_2_mm[[i]],
                notes = draft$notes[[i]]
              )

              saved_count <- saved_count + 1L
            }

            TRUE
          },
          error = function(e) {
            shiny::showNotification(
              paste0(
                "Saving stopped after ",
                saved_count,
                " measurement(s): ",
                conditionMessage(e)
              ),
              type = "error",
              duration = NULL
            )

            FALSE
          }
        )

        if (isTRUE(success)) {
          draft_measurements(NULL)
          refresh_measurements()
          refresh_tumors()

          shiny::showNotification(
            paste0(saved_count, " measurement(s) saved."),
            type = "message",
            duration = 4
          )
        }
      })

      # ---------------------------------------------------------------------
      # Saved measurements and deletion
      # ---------------------------------------------------------------------

      output$measurement_table <- DT::renderDT({
        DT::datatable(
          measurements_data(),
          rownames = FALSE,
          selection = "single",
          options = list(pageLength = 25, autoWidth = TRUE)
        )
      })

      shiny::observeEvent(input$refresh_measurements, {
        refresh_measurements()
      })

      shiny::observeEvent(input$delete_measurement, {
        selected <- input$measurement_table_rows_selected
        data <- measurements_data()

        if (length(selected) != 1L || nrow(data) == 0L) {
          shiny::showNotification(
            "Select one measurement before deleting.",
            type = "error"
          )
          return()
        }

        measurement_to_delete(data[selected, , drop = FALSE])

        shiny::showModal(
          shiny::modalDialog(
            title = "Delete tumor measurement?",

            shiny::tags$p(
              class = "text-danger",
              "This action cannot be undone."
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),
              shiny::actionButton(
                session$ns("confirm_delete_measurement"),
                "Permanently delete",
                class = "btn-danger"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_delete_measurement, {
        selected <- measurement_to_delete()

        if (is.null(selected)) {
          return()
        }

        tryCatch(
          {
            study$delete_tumor_measurement(
              measurement_pk = selected$measurement_pk[[1L]]
            )

            shiny::removeModal()
            measurement_to_delete(NULL)
            refresh_measurements()
          },
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
          }
        )
      })

      invisible(NULL)
    }
  )
}
#' Treatment Assignment and Dose Administration Shiny Module UI
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny tag list.
#' @export
treatment_admin_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::br(),

    shiny::tabsetPanel(
      id = ns("treatment_tabs"),

      # =====================================================================
      # Treatment groups
      # =====================================================================
      shiny::tabPanel(
        title = "Treatment groups",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add treatment group"),

              shiny::textInput(
                inputId = ns("group_name"),
                label = "Treatment group name",
                placeholder = "For example: Vehicle, g1, anti_PD1"
              ),

              shiny::textInput(
                inputId = ns("group_description"),
                label = "Description (optional)",
                placeholder = "For example: PBS control"
              ),

              shiny::textAreaInput(
                inputId = ns("group_notes"),
                label = "Notes (optional)",
                rows = 3
              ),

              shiny::actionButton(
                inputId = ns("add_treatment_group"),
                label = "Add treatment group",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              )
            )
          ),

          shiny::column(
            width = 8,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("refresh_groups"),
                label = "Refresh treatment groups",
                icon = shiny::icon("rotate")
              )
            ),

            shiny::h4("Treatment groups in the active experiment"),

            shiny::p(
              "Treatment groups are specific to the currently active ",
              "experiment. Add reusable treatments to a group in the ",
              shiny::tags$strong("Group regimens"),
              " tab."
            ),

            DT::DTOutput(ns("group_table"))
          )
        )
      ),

      # =====================================================================
      # Group regimens
      # =====================================================================
      shiny::tabPanel(
        title = "Group regimens",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add treatment to group"),

              shiny::selectInput(
                inputId = ns("regimen_group"),
                label = "Treatment group",
                choices = character(0)
              ),

              shiny::selectInput(
                inputId = ns("regimen_treatment"),
                label = "Reusable treatment definition",
                choices = character(0)
              ),

              shiny::numericInput(
                inputId = ns("regimen_display_order"),
                label = "Display order",
                value = 1,
                min = 1,
                step = 1
              ),

              shiny::textInput(
                inputId = ns("regimen_schedule_description"),
                label = "Schedule description (optional)",
                placeholder = "For example: Q3D beginning Day 7"
              ),

              shiny::numericInput(
                inputId = ns("regimen_start_day"),
                label = "Start study day (optional)",
                value = NA_integer_,
                min = 0,
                step = 1
              ),

              shiny::numericInput(
                inputId = ns("regimen_end_day"),
                label = "End study day (optional)",
                value = NA_integer_,
                min = 0,
                step = 1
              ),

              shiny::textAreaInput(
                inputId = ns("regimen_notes"),
                label = "Notes (optional)",
                rows = 3
              ),

              shiny::actionButton(
                inputId = ns("add_regimen"),
                label = "Add treatment to group",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              )
            )
          ),

          shiny::column(
            width = 8,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("refresh_regimens"),
                label = "Refresh regimens",
                icon = shiny::icon("rotate")
              ),

              shiny::actionButton(
                inputId = ns("remove_regimen"),
                label = "Remove selected regimen",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            ),

            shiny::h4("Treatment-group regimens"),

            shiny::p(
              class = "text-danger",
              "Removing a regimen removes its link to the treatment group. ",
              "It does not delete the reusable treatment definition."
            ),

            DT::DTOutput(ns("regimen_table"))
          )
        )
      ),

      # =====================================================================
      # Animal assignments
      # =====================================================================
      shiny::tabPanel(
        title = "Animal assignments",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Assign animals to a treatment group"),

              shiny::selectizeInput(
                inputId = ns("assignment_animals"),
                label = "Animals",
                choices = character(0),
                multiple = TRUE,
                options = list(
                  placeholder = "Select one or more animals"
                )
              ),

              shiny::selectInput(
                inputId = ns("assignment_group"),
                label = "Treatment group",
                choices = character(0)
              ),

              shiny::dateInput(
                inputId = ns("assignment_date"),
                label = "Assignment date",
                value = Sys.Date(),
                format = "yyyy-mm-dd"
              ),

              shiny::dateInput(
                inputId = ns("assignment_start_date"),
                label = "Treatment start date",
                value = Sys.Date(),
                format = "yyyy-mm-dd"
              ),

              shiny::dateInput(
                inputId = ns("assignment_end_date"),
                label = "Treatment end date (optional)",
                value = NULL,
                format = "yyyy-mm-dd"
              ),

              shiny::textAreaInput(
                inputId = ns("assignment_notes"),
                label = "Notes (optional)",
                rows = 3
              ),

              shiny::actionButton(
                inputId = ns("assign_animals"),
                label = "Assign selected animals",
                icon = shiny::icon("plus"),
                class = "btn-primary"
              )
            )
          ),

          shiny::column(
            width = 8,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("refresh_assignments"),
                label = "Refresh assignments",
                icon = shiny::icon("rotate")
              )
            ),

            shiny::h4("Animal treatment assignments"),

            shiny::p(
              "Assignments define which treatment group applies to each ",
              "animal and the period during which it is active."
            ),

            DT::DTOutput(ns("assignment_table"))
          )
        )
      ),

      # =====================================================================
      # Actual dose administration
      # =====================================================================
      shiny::tabPanel(
        title = "Dose administration",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Record an administered dose"),

              shiny::dateInput(
                inputId = ns("dose_date"),
                label = "Administration date",
                value = Sys.Date(),
                format = "yyyy-mm-dd"
              ),

              shiny::selectInput(
                inputId = ns("dose_animal"),
                label = "Animal",
                choices = character(0)
              ),

              shiny::selectInput(
                inputId = ns("dose_group"),
                label = "Treatment group",
                choices = character(0)
              ),

              shiny::selectInput(
                inputId = ns("dose_regimen"),
                label = "Treatment / regimen",
                choices = character(0)
              ),

              shiny::selectInput(
                inputId = ns("dose_lot"),
                label = "Drug lot",
                choices = character(0)
              ),

              shiny::numericInput(
                inputId = ns("dose_amount"),
                label = "Actual dose",
                value = NA_real_,
                min = 0
              ),

              shiny::textInput(
                inputId = ns("dose_unit"),
                label = "Dose unit",
                placeholder = "For example: mg/kg"
              ),

              shiny::textInput(
                inputId = ns("dose_route"),
                label = "Route (optional)",
                placeholder = "For example: IP"
              ),

              shiny::numericInput(
                inputId = ns("dose_volume_ul"),
                label = "Injection volume (µL; optional)",
                value = NA_real_,
                min = 0
              ),

              shiny::textAreaInput(
                inputId = ns("dose_notes"),
                label = "Notes (optional)",
                rows = 3
              ),

              shiny::actionButton(
                inputId = ns("record_dose"),
                label = "Record administered dose",
                icon = shiny::icon("syringe"),
                class = "btn-primary"
              )
            )
          ),

          shiny::column(
            width = 8,

            shiny::div(
              class = "mb-3",

              shiny::actionButton(
                inputId = ns("refresh_doses"),
                label = "Refresh dose records",
                icon = shiny::icon("rotate")
              )
            ),

            shiny::h4("Recorded dose administrations"),

            shiny::p(
              "Only actual administered doses should be entered here. ",
              "The selected animal must have an active assignment to the ",
              "selected group on the administration date."
            ),

            DT::DTOutput(ns("dose_table"))
          )
        )
      )
    )
  )
}


#' Treatment Assignment and Dose Administration Shiny Module Server
#'
#' @param id Shiny module identifier.
#' @param study A TumorModelProject object.
#'
#' @return Invisibly, NULL.
#' @export
treatment_admin_server <- function(id, study) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      if (!inherits(study, "TumorModelProject")) {
        stop(
          "`study` must be a TumorModelProject object.",
          call. = FALSE
        )
      }

      # ---------------------------------------------------------------------
      # Reactive data
      # ---------------------------------------------------------------------

      groups_data <- shiny::reactiveVal(study$treatment_groups())
      regimens_data <- shiny::reactiveVal(study$treatment_group_regimens())
      assignments_data <- shiny::reactiveVal(study$treatment_assignments())
      doses_data <- shiny::reactiveVal(study$doses())

      animals_data <- shiny::reactiveVal(study$animals())
      treatments_data <- shiny::reactiveVal(study$treatments())
      lots_data <- shiny::reactiveVal(study$drug_lots())

      regimen_to_remove <- shiny::reactiveVal(NULL)

      group_table_proxy <- DT::dataTableProxy(
        outputId = "group_table",
        session = session
      )

      regimen_table_proxy <- DT::dataTableProxy(
        outputId = "regimen_table",
        session = session
      )

      assignment_table_proxy <- DT::dataTableProxy(
        outputId = "assignment_table",
        session = session
      )

      dose_table_proxy <- DT::dataTableProxy(
        outputId = "dose_table",
        session = session
      )

      # ---------------------------------------------------------------------
      # Helpers
      # ---------------------------------------------------------------------

      nullable_number <- function(x) {
        if (
          is.null(x) ||
            length(x) == 0L ||
            is.na(x) ||
            !is.finite(x)
        ) {
          return(NULL)
        }

        as.numeric(x)
      }

      nullable_date <- function(x) {
        if (
          is.null(x) ||
            length(x) == 0L ||
            is.na(x) ||
            !nzchar(as.character(x))
        ) {
          return(NULL)
        }

        as.character(x)
      }

      active_animals <- function() {
        data <- animals_data()

        if (nrow(data) == 0L) {
          return(data)
        }

        if (!"status" %in% names(data)) {
          return(data)
        }

        data[
          tolower(trimws(as.character(data$status))) != "killed",
          ,
          drop = FALSE
        ]
      }

      refresh_group_choices <- function() {
        data <- groups_data()

        choices <- if (nrow(data) == 0L) {
          character(0)
        } else {
          stats::setNames(
            data$group_name,
            ifelse(
              is.na(data$description) | data$description == "",
              data$group_name,
              paste0(data$group_name, " — ", data$description)
            )
          )
        }

        shiny::updateSelectInput(
          session = session,
          inputId = "regimen_group",
          choices = choices
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "assignment_group",
          choices = choices
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "dose_group",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_animal_choices <- function() {
        data <- active_animals()

        choices <- if (nrow(data) == 0L) {
          character(0)
        } else {
          cage <- ifelse(
            is.na(data$cage_id) | data$cage_id == "",
            "cage not recorded",
            paste0("Cage: ", data$cage_id)
          )

          stats::setNames(
            data$animal_id,
            paste0(data$animal_id, " [", cage, "]")
          )
        }

        shiny::updateSelectizeInput(
          session = session,
          inputId = "assignment_animals",
          choices = choices,
          server = TRUE
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "dose_animal",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_treatment_choices <- function() {
        data <- treatments_data()

        choices <- if (nrow(data) == 0L) {
          character(0)
        } else {
          stats::setNames(
            data$treatment_name,
            paste0(
              data$treatment_name,
              " — ",
              data$drug,
              " ",
              data$treatment_dose,
              " ",
              data$dose_unit
            )
          )
        }

        shiny::updateSelectInput(
          session = session,
          inputId = "regimen_treatment",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_dose_regimen_choices <- function() {
        data <- regimens_data()
        group <- input$dose_group

        if (is.null(group) || !nzchar(group) || nrow(data) == 0L) {
          shiny::updateSelectInput(
            session = session,
            inputId = "dose_regimen",
            choices = character(0)
          )
          return(invisible(NULL))
        }

        data <- data[
          data$treatment_group == group,
          ,
          drop = FALSE
        ]

        choices <- if (nrow(data) == 0L) {
          character(0)
        } else {
          stats::setNames(
            data$treatment_name,
            paste0(
              data$treatment_name,
              " — ",
              data$drug,
              " ",
              data$treatment_dose,
              " ",
              data$dose_unit
            )
          )
        }

        shiny::updateSelectInput(
          session = session,
          inputId = "dose_regimen",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_dose_lot_choices <- function() {
        treatment_name <- input$dose_regimen
        treatment_data <- treatments_data()
        lot_data <- lots_data()

        if (
          is.null(treatment_name) ||
            !nzchar(treatment_name) ||
            nrow(treatment_data) == 0L ||
            nrow(lot_data) == 0L
        ) {
          shiny::updateSelectInput(
            session = session,
            inputId = "dose_lot",
            choices = character(0)
          )
          return(invisible(NULL))
        }

        treatment <- treatment_data[
          treatment_data$treatment_name == treatment_name,
          ,
          drop = FALSE
        ]

        if (nrow(treatment) != 1L) {
          shiny::updateSelectInput(
            session = session,
            inputId = "dose_lot",
            choices = character(0)
          )
          return(invisible(NULL))
        }

        matching_lots <- lot_data[
          lot_data$drug == treatment$drug[[1L]] &
            lot_data$company_name == treatment$company_name[[1L]] &
            lot_data$catalog_number == treatment$catalog_number[[1L]],
          ,
          drop = FALSE
        ]

        choices <- if (nrow(matching_lots) == 0L) {
          character(0)
        } else {
          stats::setNames(
            matching_lots$lot_number,
            paste0(
              matching_lots$lot_number,
              ifelse(
                is.na(matching_lots$expiration_date) |
                  matching_lots$expiration_date == "",
                "",
                paste0(" (expires ", matching_lots$expiration_date, ")")
              )
            )
          )
        }

        shiny::updateSelectInput(
          session = session,
          inputId = "dose_lot",
          choices = choices
        )

        invisible(NULL)
      }

      selected_regimen <- function() {
        treatment_name <- input$dose_regimen
        group_name <- input$dose_group
        data <- regimens_data()

        if (
          is.null(treatment_name) ||
            is.null(group_name) ||
            nrow(data) == 0L
        ) {
          return(NULL)
        }

        matches <- data[
          data$treatment_group == group_name &
            data$treatment_name == treatment_name,
          ,
          drop = FALSE
        ]

        if (nrow(matches) != 1L) {
          return(NULL)
        }

        matches
      }

      # ---------------------------------------------------------------------
      # Refresh data
      # ---------------------------------------------------------------------

      refresh_groups <- function(reset_paging = FALSE) {
        data <- study$treatment_groups()
        groups_data(data)

        DT::replaceData(
          proxy = group_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        refresh_group_choices()
        invisible(NULL)
      }

      refresh_regimens <- function(reset_paging = FALSE) {
        data <- study$treatment_group_regimens()
        regimens_data(data)

        DT::replaceData(
          proxy = regimen_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        refresh_dose_regimen_choices()
        invisible(NULL)
      }

      refresh_assignments <- function(reset_paging = FALSE) {
        data <- study$treatment_assignments()
        assignments_data(data)

        DT::replaceData(
          proxy = assignment_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        invisible(NULL)
      }

      refresh_doses <- function(reset_paging = FALSE) {
        data <- study$doses()
        doses_data(data)

        DT::replaceData(
          proxy = dose_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        invisible(NULL)
      }

      refresh_catalog_data <- function() {
        animals_data(study$animals())
        treatments_data(study$treatments())
        lots_data(study$drug_lots())

        refresh_animal_choices()
        refresh_treatment_choices()
        refresh_dose_lot_choices()

        invisible(NULL)
      }

      refresh_everything <- function() {
        refresh_catalog_data()
        refresh_groups()
        refresh_regimens()
        refresh_assignments()
        refresh_doses()

        invisible(NULL)
      }

      # Ensure dependent dose selections update when group/treatment changes.
      shiny::observeEvent(input$dose_group, {
        refresh_dose_regimen_choices()
      }, ignoreInit = TRUE)

      shiny::observeEvent(input$dose_regimen, {
        refresh_dose_lot_choices()

        regimen <- selected_regimen()

        if (!is.null(regimen)) {
          shiny::updateNumericInput(
            session = session,
            inputId = "dose_amount",
            value = regimen$treatment_dose[[1L]]
          )

          shiny::updateTextInput(
            session = session,
            inputId = "dose_unit",
            value = regimen$dose_unit[[1L]]
          )

          shiny::updateTextInput(
            session = session,
            inputId = "dose_route",
            value = regimen$route[[1L]]
          )
        }
      }, ignoreInit = TRUE)

      # =====================================================================
      # Treatment groups
      # =====================================================================

      output$group_table <- DT::renderDT({
        DT::datatable(
          groups_data(),
          rownames = FALSE,
          selection = "none",
          options = list(
            pageLength = 25,
            autoWidth = TRUE,
            columnDefs = list(
              list(
                targets = 0,
                visible = FALSE,
                searchable = FALSE
              )
            )
          )
        )
      })

      shiny::observeEvent(input$refresh_groups, {
        refresh_groups()
      })

      shiny::observeEvent(input$add_treatment_group, {
        tryCatch(
          {
            study$add_treatment_group(
              treatment_group = input$group_name,
              description = input$group_description,
              notes = input$group_notes
            )

            shiny::updateTextInput(
              session,
              "group_name",
              value = ""
            )

            shiny::updateTextInput(
              session,
              "group_description",
              value = ""
            )

            shiny::updateTextAreaInput(
              session,
              "group_notes",
              value = ""
            )

            refresh_groups()

            shiny::showNotification(
              "Treatment group added.",
              type = "message",
              duration = 3
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
      })

      # =====================================================================
      # Group regimens
      # =====================================================================

      output$regimen_table <- DT::renderDT({
        DT::datatable(
          regimens_data(),
          rownames = FALSE,
          selection = "single",
          options = list(
            pageLength = 25,
            autoWidth = TRUE,
            columnDefs = list(
              list(
                targets = c(0, 1, 2, 5),
                visible = FALSE,
                searchable = FALSE
              )
            )
          )
        )
      })

      shiny::observeEvent(input$refresh_regimens, {
        refresh_catalog_data()
        refresh_regimens()
      })

      shiny::observeEvent(input$add_regimen, {
        start_day <- nullable_number(input$regimen_start_day)
        end_day <- nullable_number(input$regimen_end_day)

        if (!is.null(start_day)) {
          start_day <- as.integer(start_day)
        }

        if (!is.null(end_day)) {
          end_day <- as.integer(end_day)
        }

        tryCatch(
          {
            study$add_treatment_to_group(
              treatment_group = input$regimen_group,
              treatment_name = input$regimen_treatment,
              display_order = as.integer(input$regimen_display_order),
              schedule_description = input$regimen_schedule_description,
              start_day = start_day,
              end_day = end_day,
              notes = input$regimen_notes
            )

            shiny::updateNumericInput(
              session,
              "regimen_display_order",
              value = 1
            )

            shiny::updateTextInput(
              session,
              "regimen_schedule_description",
              value = ""
            )

            shiny::updateNumericInput(
              session,
              "regimen_start_day",
              value = NA_integer_
            )

            shiny::updateNumericInput(
              session,
              "regimen_end_day",
              value = NA_integer_
            )

            shiny::updateTextAreaInput(
              session,
              "regimen_notes",
              value = ""
            )

            refresh_regimens()

            shiny::showNotification(
              "Treatment added to group.",
              type = "message",
              duration = 3
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
      })

      shiny::observeEvent(input$remove_regimen, {
        selected <- input$regimen_table_rows_selected
        data <- regimens_data()

        if (length(selected) != 1L || nrow(data) == 0L) {
          shiny::showNotification(
            "Select one regimen before removing it.",
            type = "error",
            duration = 4
          )
          return()
        }

        regimen_to_remove(data[selected, , drop = FALSE])

        shiny::showModal(
          shiny::modalDialog(
            title = "Remove treatment from group?",

            shiny::tags$p(
              "Remove treatment ",
              shiny::tags$strong(
                data$treatment_name[[selected]]
              ),
              " from group ",
              shiny::tags$strong(
                data$treatment_group[[selected]]
              ),
              "?"
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),

              shiny::actionButton(
                inputId = session$ns("confirm_remove_regimen"),
                label = "Remove treatment",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_remove_regimen, {
        regimen <- regimen_to_remove()

        if (is.null(regimen)) {
          shiny::removeModal()
          return()
        }

        tryCatch(
          {
            study$remove_treatment_from_group(
              treatment_group = regimen$treatment_group[[1L]],
              treatment_name = regimen$treatment_name[[1L]]
            )

            shiny::removeModal()
            regimen_to_remove(NULL)
            refresh_regimens()

            shiny::showNotification(
              "Treatment removed from group.",
              type = "message",
              duration = 3
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
      })

      # =====================================================================
      # Animal assignments
      # =====================================================================

      output$assignment_table <- DT::renderDT({
        DT::datatable(
          assignments_data(),
          rownames = FALSE,
          selection = "none",
          options = list(
            pageLength = 25,
            autoWidth = TRUE,
            columnDefs = list(
              list(
                targets = c(0, 1, 2, 7),
                visible = FALSE,
                searchable = FALSE
              )
            )
          )
        )
      })

      shiny::observeEvent(input$refresh_assignments, {
        refresh_catalog_data()
        refresh_assignments()
      })

      shiny::observeEvent(input$assign_animals, {
        animal_ids <- input$assignment_animals

        if (is.null(animal_ids) || length(animal_ids) == 0L) {
          shiny::showNotification(
            "Select at least one animal.",
            type = "error",
            duration = 4
          )
          return()
        }

        assigned <- 0L

        success <- tryCatch(
          {
            for (animal_id in animal_ids) {
              study$assign_animal_to_treatment_group(
                animal_id = animal_id,
                treatment_group = input$assignment_group,
                assignment_date = as.character(input$assignment_date),
                treatment_start_date = as.character(
                  input$assignment_start_date
                ),
                treatment_end_date = nullable_date(
                  input$assignment_end_date
                ),
                notes = input$assignment_notes
              )

              assigned <- assigned + 1L
            }

            TRUE
          },
          error = function(e) {
            shiny::showNotification(
              paste0(
                "Assignment stopped after ",
                assigned,
                " animal(s): ",
                conditionMessage(e)
              ),
              type = "error",
              duration = NULL
            )

            FALSE
          }
        )

        if (isTRUE(success)) {
          shiny::updateSelectizeInput(
            session,
            "assignment_animals",
            selected = character(0)
          )

          shiny::updateTextAreaInput(
            session,
            "assignment_notes",
            value = ""
          )

          refresh_assignments()

          shiny::showNotification(
            paste0(
              assigned,
              " animal(s) assigned to treatment group."
            ),
            type = "message",
            duration = 4
          )
        }
      })

      # =====================================================================
      # Dose administration
      # =====================================================================

      output$dose_table <- DT::renderDT({
        DT::datatable(
          doses_data(),
          rownames = FALSE,
          selection = "none",
          options = list(
            pageLength = 25,
            autoWidth = TRUE,
            columnDefs = list(
              list(
                targets = c(0, 1, 2),
                visible = FALSE,
                searchable = FALSE
              )
            )
          )
        )
      })

      shiny::observeEvent(input$refresh_doses, {
        refresh_catalog_data()
        refresh_regimens()
        refresh_doses()
      })

      shiny::observeEvent(input$record_dose, {
        regimen <- selected_regimen()

        if (is.null(regimen)) {
          shiny::showNotification(
            "Select a valid treatment group and treatment regimen.",
            type = "error",
            duration = 4
          )
          return()
        }

        if (is.null(input$dose_lot) || !nzchar(input$dose_lot)) {
          shiny::showNotification(
            "Select a drug lot before recording a dose.",
            type = "error",
            duration = 4
          )
          return()
        }

        tryCatch(
          {
            study$record_dose(
              animal_id = input$dose_animal,
              treatment_group = input$dose_group,
              drug = regimen$drug[[1L]],
              company_name = regimen$company_name[[1L]],
              catalog_number = regimen$catalog_number[[1L]],
              lot_number = input$dose_lot,
              administration_date = as.character(input$dose_date),
              dose = input$dose_amount,
              dose_unit = input$dose_unit,
              route = input$dose_route,
              volume_ul = nullable_number(input$dose_volume_ul),
              notes = input$dose_notes
            )

            shiny::updateNumericInput(
              session,
              "dose_volume_ul",
              value = NA_real_
            )

            shiny::updateTextAreaInput(
              session,
              "dose_notes",
              value = ""
            )

            refresh_doses()

            shiny::showNotification(
              "Dose administration recorded.",
              type = "message",
              duration = 3
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
      })

      # Populate inputs and tables when the module starts.
      refresh_everything()

      invisible(NULL)
    }
  )
}
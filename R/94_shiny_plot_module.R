#' Plotting Shiny Module UI
#'
#' Interactive visualizations for tumor growth and time-to-euthanasia outcomes.
#' Plot data and figures are scoped to the active experiment.
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny tag list.
#' @export
plotting_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,

        shiny::wellPanel(
          shiny::h4("Plot settings"),

          shiny::dateInput(
            inputId = ns("study_start_date"),
            label = "Study Day 0 / inoculation date",
            value = Sys.Date(),
            format = "yyyy-mm-dd"
          ),

          shiny::checkboxInput(
            inputId = ns("use_cap_volume"),
            label = "Show tumor-volume cap",
            value = TRUE
          ),

          shiny::conditionalPanel(
            condition = paste0(
              "input['",
              ns("use_cap_volume"),
              "']"
            ),

            shiny::numericInput(
              inputId = ns("cap_volume_mm3"),
              label = "Tumor-volume cap (mm³)",
              value = 1000,
              min = 1,
              step = 50
            )
          ),

          shiny::hr(),

          shiny::selectizeInput(
            inputId = ns("treatment_order"),
            label = "Treatments to include and display order",
            choices = character(0),
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = "Select treatment groups"
            )
          ),

          shiny::actionButton(
            inputId = ns("select_all_treatments"),
            label = "Select all treatments",
            icon = shiny::icon("check")
          ),

          shiny::hr(),

          shiny::selectizeInput(
            inputId = ns("exclude_animal_ids"),
            label = "Exclude animal IDs (optional)",
            choices = character(0),
            multiple = TRUE,
            options = list(
              plugins = list("remove_button"),
              placeholder = "No animals excluded"
            )
          ),

          shiny::hr(),

          shiny::actionButton(
            inputId = ns("refresh_plot_choices"),
            label = "Refresh data and choices",
            icon = shiny::icon("rotate")
          ),

          shiny::br(),
          shiny::br(),

          shiny::p(
            class = "text-muted small",
            "Plots use records from the currently active experiment only. ",
            "Study Day 0 should ordinarily be the inoculation date."
          )
        )
      ),

      shiny::column(
        width = 9,

        shiny::tabsetPanel(
          id = ns("plot_tabs"),

          # ================================================================
          # Individual tumor trajectories
          # ================================================================
          shiny::tabPanel(
            title = "Individual tumors",

            shiny::br(),

            shiny::fluidRow(
              shiny::column(
                width = 4,

                shiny::selectInput(
                  inputId = ns("individual_facet_by"),
                  label = "Facet by",
                  choices = c(
                    "Treatment" = "treatment",
                    "Animal ID" = "animal_id",
                    "Cage ID" = "cage_id",
                    "Sex" = "sex",
                    "Species" = "species"
                  ),
                  selected = "treatment"
                )
              ),

              shiny::column(
                width = 4,

                shiny::numericInput(
                  inputId = ns("individual_ncol"),
                  label = "Facet columns",
                  value = 4,
                  min = 1,
                  max = 12,
                  step = 1
                )
              ),

              shiny::column(
                width = 4,

                shiny::checkboxInput(
                  inputId = ns("individual_free_y"),
                  label = "Use independent y-axis per facet",
                  value = FALSE
                ),

                shiny::checkboxInput(
                  inputId = ns("individual_show_points"),
                  label = "Show measurement points",
                  value = TRUE
                ),

                shiny::checkboxInput(
                  inputId = ns("individual_show_animal_ids"),
                  label = "Label final animal measurements",
                  value = TRUE
                )
              )
            ),

            shiny::hr(),

            shiny::plotOutput(
              outputId = ns("individual_plot"),
              height = "750px"
            ),

            shiny::hr(),

            shiny::downloadButton(
              outputId = ns("download_individual_data"),
              label = "Download individual plot data (CSV)"
            )
          ),

          # ================================================================
          # Group tumor growth
          # ================================================================
          shiny::tabPanel(
            title = "Tumor growth by treatment",

            shiny::br(),

            shiny::fluidRow(
              shiny::column(
                width = 4,

                shiny::selectInput(
                  inputId = ns("group_summary"),
                  label = "Group summary",
                  choices = c(
                    "Mean" = "mean",
                    "Median" = "median"
                  ),
                  selected = "mean"
                )
              ),

              shiny::column(
                width = 4,

                shiny::selectInput(
                  inputId = ns("group_error_bar"),
                  label = "Error bars",
                  choices = c(
                    "SEM" = "sem",
                    "Standard deviation" = "sd",
                    "None" = "none"
                  ),
                  selected = "sem"
                )
              ),

              shiny::column(
                width = 4,

                shiny::checkboxInput(
                  inputId = ns("group_show_individuals"),
                  label = "Show faint individual trajectories",
                  value = FALSE
                )
              )
            ),

            shiny::hr(),

            shiny::plotOutput(
              outputId = ns("group_plot"),
              height = "700px"
            ),

            shiny::hr(),

            shiny::h4("Summary data"),

            DT::DTOutput(ns("group_summary_table")),

            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_group_summary"),
              label = "Download group summary (CSV)"
            )
          ),

          # ================================================================
          # LOCF sensitivity analysis
          # ================================================================
          shiny::tabPanel(
            title = "LOCF sensitivity",

            shiny::br(),

            shiny::tags$p(
              class = "text-warning",
              shiny::tags$strong("Sensitivity analysis only. "),
              "LOCF carries an animal's final observed tumor volume forward ",
              "to later scheduled study days. It should not replace plots ",
              "based on observed measurements."
            ),

            shiny::fluidRow(
              shiny::column(
                width = 4,

                shiny::selectInput(
                  inputId = ns("locf_summary"),
                  label = "Group summary",
                  choices = c(
                    "Mean" = "mean",
                    "Median" = "median"
                  ),
                  selected = "mean"
                ),

                shiny::selectInput(
                  inputId = ns("locf_error_bar"),
                  label = "Error bars",
                  choices = c(
                    "SEM" = "sem",
                    "Standard deviation" = "sd",
                    "None" = "none"
                  ),
                  selected = "sem"
                )
              ),

              shiny::column(
                width = 4,

                shiny::checkboxInput(
                  inputId = ns("locf_use_observed_days"),
                  label = "Use observed measurement days",
                  value = TRUE
                ),

                shiny::conditionalPanel(
                  condition = paste0(
                    "!input['",
                    ns("locf_use_observed_days"),
                    "']"
                  ),

                  shiny::textInput(
                    inputId = ns("locf_schedule_days"),
                    label = "Scheduled study days",
                    value = "0, 4, 7, 11, 14",
                    placeholder = "For example: 0, 3, 7, 10, 14"
                  )
                ),

                shiny::numericInput(
                  inputId = ns("locf_end_day"),
                  label = "Final study day for LOCF",
                  value = 14,
                  min = 0,
                  step = 1
                )
              ),

              shiny::column(
                width = 4,

                shiny::selectizeInput(
                  inputId = ns("locf_animal_ids"),
                  label = "Carry forward selected animals",
                  choices = character(0),
                  multiple = TRUE,
                  options = list(
                    plugins = list("remove_button"),
                    placeholder = paste(
                      "Leave blank to carry forward all animals"
                    )
                  )
                ),

                shiny::checkboxInput(
                  inputId = ns("locf_show_individuals"),
                  label = "Show observed and LOCF trajectories",
                  value = FALSE
                )
              )
            ),

            shiny::hr(),

            shiny::plotOutput(
              outputId = ns("locf_plot"),
              height = "700px"
            ),

            shiny::hr(),

            shiny::h4("LOCF summary data"),

            DT::DTOutput(ns("locf_summary_table")),

            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_locf_summary"),
              label = "Download LOCF summary (CSV)"
            )
          ),

          # ================================================================
          # Survival / Kaplan-Meier
          # ================================================================
          shiny::tabPanel(
            title = "Survival",

            shiny::br(),

            shiny::tags$p(
              "A recorded killed/euthanasia date on or before the censor ",
              "date is treated as an event. All other animals are ",
              "right-censored at the selected censor date."
            ),

            shiny::fluidRow(
              shiny::column(
                width = 4,

                shiny::dateInput(
                  inputId = ns("survival_censor_date"),
                  label = "Administrative censor date",
                  value = Sys.Date(),
                  format = "yyyy-mm-dd"
                )
              ),

              shiny::column(
                width = 4,

                shiny::checkboxInput(
                  inputId = ns("survival_show_censor_marks"),
                  label = "Show censor marks",
                  value = TRUE
                )
              ),

              shiny::column(
                width = 4,

                shiny::checkboxInput(
                  inputId = ns("survival_show_end_labels"),
                  label = "Label curve endpoints",
                  value = TRUE
                )
              )
            ),

            shiny::hr(),

            shiny::plotOutput(
              outputId = ns("survival_plot"),
              height = "700px"
            ),

            shiny::hr(),

            shiny::h4("Survival analysis data"),

            DT::DTOutput(ns("survival_data_table")),

            shiny::br(),

            shiny::downloadButton(
              outputId = ns("download_survival_data"),
              label = "Download survival data (CSV)"
            )
          )
        )
      )
    )
  )
}


#' Plotting Shiny Module Server
#'
#' @param id Shiny module identifier.
#' @param study A TumorModelProject object.
#'
#' @return Invisibly, NULL.
#' @export
plotting_server <- function(id, study) {
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
      # Helpers
      # ---------------------------------------------------------------------

      parse_schedule_days <- function(x) {
        x <- trimws(as.character(x))

        if (length(x) != 1L || is.na(x) || !nzchar(x)) {
          stop(
            "Enter one or more comma-separated numeric study days.",
            call. = FALSE
          )
        }

        days <- unlist(
          strsplit(x, "[,;[:space:]]+"),
          use.names = FALSE
        )

        days <- suppressWarnings(as.numeric(days))
        days <- sort(unique(days))

        if (
          length(days) == 0L ||
            any(is.na(days)) ||
            any(!is.finite(days))
        ) {
          stop(
            paste(
              "Scheduled study days must be comma-separated numbers,",
              "for example: 0, 4, 7, 11, 14."
            ),
            call. = FALSE
          )
        }

        days
      }

      get_cap_volume <- function() {
        if (!isTRUE(input$use_cap_volume)) {
          return(NULL)
        }

        cap <- suppressWarnings(as.numeric(input$cap_volume_mm3))

        if (
          length(cap) != 1L ||
            is.na(cap) ||
            !is.finite(cap) ||
            cap <= 0
        ) {
          stop(
            "Tumor-volume cap must be a positive number.",
            call. = FALSE
          )
        }

        cap
      }

      get_treatment_order <- function() {
        selected <- input$treatment_order

        if (is.null(selected) || length(selected) == 0L) {
          return(NULL)
        }

        as.character(selected)
      }

      get_excluded_animals <- function() {
        selected <- input$exclude_animal_ids

        if (is.null(selected) || length(selected) == 0L) {
          return(NULL)
        }

        as.character(selected)
      }

      # ---------------------------------------------------------------------
      # Refresh selectable values from the active experiment
      # ---------------------------------------------------------------------

      refresh_plot_choices <- function() {
        animals <- study$animals()
        groups <- study$treatment_groups()
        measurements <- study$tumor_measurements()

        animal_ids <- character(0)

        if (
          is.data.frame(animals) &&
            nrow(animals) > 0L &&
            "animal_id" %in% names(animals)
        ) {
          animal_ids <- sort(unique(
            as.character(animals$animal_id)
          ))

          animal_ids <- animal_ids[
            !is.na(animal_ids) & nzchar(animal_ids)
          ]
        }

        treatment_labels <- character(0)

        if (
          is.data.frame(groups) &&
            nrow(groups) > 0L &&
            all(c("group_name", "description") %in% names(groups))
        ) {
          treatment_labels <- as.character(groups$description)

          missing_description <- is.na(treatment_labels) |
            !nzchar(trimws(treatment_labels))

          treatment_labels[missing_description] <- as.character(
            groups$group_name[missing_description]
          )

          treatment_labels <- unique(treatment_labels)
          treatment_labels <- treatment_labels[
            !is.na(treatment_labels) & nzchar(treatment_labels)
          ]
        }

        # Preserve currently selected values only if still valid.
        current_treatments <- input$treatment_order
        current_treatments <- current_treatments[
          current_treatments %in% treatment_labels
        ]

        current_excluded <- input$exclude_animal_ids
        current_excluded <- current_excluded[
          current_excluded %in% animal_ids
        ]

        current_locf <- input$locf_animal_ids
        current_locf <- current_locf[
          current_locf %in% animal_ids
        ]

        shiny::updateSelectizeInput(
          session,
          "treatment_order",
          choices = treatment_labels,
          selected = current_treatments,
          server = TRUE
        )

        shiny::updateSelectizeInput(
          session,
          "exclude_animal_ids",
          choices = animal_ids,
          selected = current_excluded,
          server = TRUE
        )

        shiny::updateSelectizeInput(
          session,
          "locf_animal_ids",
          choices = animal_ids,
          selected = current_locf,
          server = TRUE
        )

        # Use available measurement dates to provide sensible defaults.
        if (
          is.data.frame(measurements) &&
            nrow(measurements) > 0L
        ) {
          date_column <- NULL

          if ("measurement_date" %in% names(measurements)) {
            date_column <- measurements$measurement_date
          } else if ("observation_date" %in% names(measurements)) {
            date_column <- measurements$observation_date
          }

          if (!is.null(date_column)) {
            measurement_dates <- suppressWarnings(as.Date(date_column))
            measurement_dates <- measurement_dates[
              !is.na(measurement_dates)
            ]

            if (length(measurement_dates) > 0L) {
              first_date <- min(measurement_dates)
              last_date <- max(measurement_dates)

              shiny::updateDateInput(
                session,
                "study_start_date",
                value = first_date
              )

              shiny::updateDateInput(
                session,
                "survival_censor_date",
                value = last_date
              )

              observed_days <- sort(unique(as.integer(
                measurement_dates - first_date
              )))

              if (length(observed_days) > 0L) {
                shiny::updateNumericInput(
                  session,
                  "locf_end_day",
                  value = max(observed_days)
                )

                shiny::updateTextInput(
                  session,
                  "locf_schedule_days",
                  value = paste(observed_days, collapse = ", ")
                )
              }
            }
          }
        }

        invisible(NULL)
      }

      shiny::observeEvent(
        input$refresh_plot_choices,
        refresh_plot_choices(),
        ignoreInit = FALSE
      )

      shiny::observeEvent(input$select_all_treatments, {
        groups <- study$treatment_groups()

        if (nrow(groups) == 0L) {
          return()
        }

        treatments <- as.character(groups$description)

        missing_description <- is.na(treatments) |
          !nzchar(trimws(treatments))

        treatments[missing_description] <- as.character(
          groups$group_name[missing_description]
        )

        treatments <- unique(treatments[
          !is.na(treatments) & nzchar(treatments)
        ])

        shiny::updateSelectizeInput(
          session,
          "treatment_order",
          selected = treatments,
          server = TRUE
        )
      })

      # ---------------------------------------------------------------------
      # Reactive active-experiment data
      # ---------------------------------------------------------------------

      tumor_plot_data <- shiny::reactive({
        shiny::req(input$study_start_date)

        study$data_tumor_plot(
          study_start_date = as.character(input$study_start_date),
          treatment_order = get_treatment_order(),
          exclude_animal_ids = get_excluded_animals(),
          cap_volume_mm3 = get_cap_volume()
        )
      })

      survival_plot_data <- shiny::reactive({
        shiny::req(
          input$study_start_date,
          input$survival_censor_date
        )

        study$data_survival(
          study_start_date = as.character(input$study_start_date),
          censor_date = as.character(input$survival_censor_date),
          treatment_order = get_treatment_order(),
          exclude_animal_ids = get_excluded_animals()
        )
      })

      # ---------------------------------------------------------------------
      # Individual tumor plot
      # ---------------------------------------------------------------------

      output$individual_plot <- shiny::renderPlot({
        shiny::req(input$study_start_date)

        tryCatch(
          {
            study$plot_individual_tumors(
              study_start_date = as.character(input$study_start_date),
              treatment_order = get_treatment_order(),
              exclude_animal_ids = get_excluded_animals(),
              cap_volume_mm3 = get_cap_volume(),
              ncol = input$individual_ncol,
              facet_by = input$individual_facet_by,
              show_points = isTRUE(input$individual_show_points),
              show_animal_ids = isTRUE(
                input$individual_show_animal_ids
              ),
              free_y = isTRUE(input$individual_free_y)
            )
          },
          error = function(e) {
            shiny::validate(
              shiny::need(FALSE, conditionMessage(e))
            )
          }
        )
      })

      output$download_individual_data <- shiny::downloadHandler(
        filename = function() {
          paste0(
            "individual_tumor_plot_data_",
            Sys.Date(),
            ".csv"
          )
        },
        content = function(file) {
          utils::write.csv(
            tumor_plot_data(),
            file,
            row.names = FALSE,
            na = ""
          )
        }
      )

      # ---------------------------------------------------------------------
      # Group tumor-growth plot
      # ---------------------------------------------------------------------

      group_plot <- shiny::reactive({
        shiny::req(input$study_start_date)

        study$plot_tumor_groups(
          study_start_date = as.character(input$study_start_date),
          treatment_order = get_treatment_order(),
          exclude_animal_ids = get_excluded_animals(),
          cap_volume_mm3 = get_cap_volume(),
          summary = input$group_summary,
          error_bar = input$group_error_bar,
          show_individuals = isTRUE(input$group_show_individuals)
        )
      })

      output$group_plot <- shiny::renderPlot({
        tryCatch(
          {
            group_plot()
          },
          error = function(e) {
            shiny::validate(
              shiny::need(FALSE, conditionMessage(e))
            )
          }
        )
      })

      group_summary_data <- shiny::reactive({
        plot <- group_plot()
        attr(plot, "plot_data")
      })

      output$group_summary_table <- DT::renderDT({
        DT::datatable(
          group_summary_data(),
          rownames = FALSE,
          options = list(
            pageLength = 25,
            scrollX = TRUE
          )
        )
      })

      output$download_group_summary <- shiny::downloadHandler(
        filename = function() {
          paste0(
            "tumor_growth_group_summary_",
            Sys.Date(),
            ".csv"
          )
        },
        content = function(file) {
          utils::write.csv(
            group_summary_data(),
            file,
            row.names = FALSE,
            na = ""
          )
        }
      )

      # ---------------------------------------------------------------------
      # LOCF sensitivity analysis
      # ---------------------------------------------------------------------

      get_locf_schedule_days <- function() {
        if (isTRUE(input$locf_use_observed_days)) {
          return(NULL)
        }

        parse_schedule_days(input$locf_schedule_days)
      }

      get_locf_animal_ids <- function() {
        selected <- input$locf_animal_ids

        if (is.null(selected) || length(selected) == 0L) {
          return(NULL)
        }

        as.character(selected)
      }

      locf_plot <- shiny::reactive({
        shiny::req(input$study_start_date)

        study$plot_tumor_groups_locf(
          study_start_date = as.character(input$study_start_date),
          treatment_order = get_treatment_order(),
          exclude_animal_ids = get_excluded_animals(),
          cap_volume_mm3 = get_cap_volume(),
          summary = input$locf_summary,
          error_bar = input$locf_error_bar,
          show_individuals = isTRUE(input$locf_show_individuals),
          schedule_days = get_locf_schedule_days(),
          end_day = input$locf_end_day,
          carry_forward_animal_ids = get_locf_animal_ids()
        )
      })

      output$locf_plot <- shiny::renderPlot({
        tryCatch(
          {
            locf_plot()
          },
          error = function(e) {
            shiny::validate(
              shiny::need(FALSE, conditionMessage(e))
            )
          }
        )
      })

      locf_summary_data <- shiny::reactive({
        plot <- locf_plot()
        attr(plot, "plot_data")
      })

      output$locf_summary_table <- DT::renderDT({
        DT::datatable(
          locf_summary_data(),
          rownames = FALSE,
          options = list(
            pageLength = 25,
            scrollX = TRUE
          )
        )
      })

      output$download_locf_summary <- shiny::downloadHandler(
        filename = function() {
          paste0(
            "tumor_growth_locf_summary_",
            Sys.Date(),
            ".csv"
          )
        },
        content = function(file) {
          utils::write.csv(
            locf_summary_data(),
            file,
            row.names = FALSE,
            na = ""
          )
        }
      )

      # ---------------------------------------------------------------------
      # Survival / Kaplan-Meier plot
      # ---------------------------------------------------------------------

      output$survival_plot <- shiny::renderPlot({
        shiny::req(
          input$study_start_date,
          input$survival_censor_date
        )

        tryCatch(
          {
            study$plot_survival(
              study_start_date = as.character(input$study_start_date),
              censor_date = as.character(input$survival_censor_date),
              treatment_order = get_treatment_order(),
              exclude_animal_ids = get_excluded_animals(),
              show_censor_marks = isTRUE(
                input$survival_show_censor_marks
              ),
              show_end_labels = isTRUE(
                input$survival_show_end_labels
              )
            )
          },
          error = function(e) {
            shiny::validate(
              shiny::need(FALSE, conditionMessage(e))
            )
          }
        )
      })

      output$survival_data_table <- DT::renderDT({
        DT::datatable(
          survival_plot_data(),
          rownames = FALSE,
          options = list(
            pageLength = 25,
            scrollX = TRUE
          )
        )
      })

      output$download_survival_data <- shiny::downloadHandler(
        filename = function() {
          paste0(
            "survival_analysis_data_",
            Sys.Date(),
            ".csv"
          )
        },
        content = function(file) {
          utils::write.csv(
            survival_plot_data(),
            file,
            row.names = FALSE,
            na = ""
          )
        }
      )

      invisible(NULL)
    }
  )
}
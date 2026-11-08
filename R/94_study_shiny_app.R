#' Tumor Model Study Shiny Application UI
#'
#' @return A Shiny UI object.
#' @keywords internal
study_app_ui <- function() {
  shiny::tagList(
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          "
          .study-app-stop-button {
            position: fixed;
            top: 8px;
            right: 16px;
            z-index: 2000;
          }

          .study-app-stop-button .btn {
            color: #ffffff;
          }
          "
        )
      )
    ),

    shiny::div(
      class = "study-app-stop-button",

      shiny::actionButton(
        inputId = "stop_app",
        label = "Stop app",
        icon = shiny::icon("power-off"),
        class = "btn-danger"
      )
    ),

    shiny::navbarPage(
      title = "Tumor Model Study",
      id = "study_navigation",

      shiny::tabPanel(
        title = "Animals and tumors",
        value = "animals",

        animal_roster_ui("animal_roster")
      ),

      shiny::tabPanel(
        title = "Drugs and products",
        value = "drugs",

        drug_catalog_ui("drug_catalog")
      ),

      shiny::tabPanel(
        title = "Animal treatments",
        value = "treatments",

        treatment_admin_ui("treatment_admin")
      )
    )
  )
}
#' Tumor Model Study Shiny Application Server
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param study A TumorModelProject object.
#'
#' @return Invisibly, NULL.
#' @keywords internal
study_app_server <- function(input, output, session, study) {
  if (!inherits(study, "TumorModelProject")) {
    stop(
      "`study` must be a TumorModelProject object.",
      call. = FALSE
    )
  }

  # Existing animal/tumor/measurement module.
  animal_roster_server(
    id = "animal_roster",
    study = study
  )

  drug_catalog_server(
    id = "drug_catalog",
    study = study
  )

  treatment_admin_server(
    id = "treatment_admin",
    study = study
  )

  shiny::observeEvent(input$stop_app, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Stop the Shiny app?",

        shiny::tags$p(
          "This will close the current Tumor Model Study application."
        ),

        footer = shiny::tagList(
          shiny::modalButton("Cancel"),

          shiny::actionButton(
            inputId = "confirm_stop_app",
            label = "Stop app",
            icon = shiny::icon("power-off"),
            class = "btn-danger"
          )
        )
      )
    )
  })

  shiny::observeEvent(input$confirm_stop_app, {
    shiny::removeModal()
    shiny::stopApp()
  })

  # Future modules will be enabled here:
  #
  # drug_catalog_server(
  #   id = "drug_catalog",
  #   study = study
  # )
  #
  # treatment_admin_server(
  #   id = "treatment_admin",
  #   study = study
  # )

  invisible(NULL)
}


#' Run the Tumor Model Study Shiny Application
#'
#' Opens the Shiny application for entering and reviewing tumor-model study
#' data. The current application includes animal, tumor, and tumor-measurement
#' entry. Drug/product and animal-treatment modules will be added separately.
#'
#' @param study A TumorModelProject object.
#' @param host Host interface on which Shiny should listen.
#' @param port Port on which Shiny should listen.
#' @param proxy_base_url Optional Open OnDemand/Jupyter URL, excluding the
#'   `/proxy/<port>/` portion.
#'
#' @return Invisibly, the result of [shiny::runApp()].
#' @export
run_study_app <- function(
  study,
  host = "0.0.0.0",
  port = 3838,
  proxy_base_url = NULL
) {
  if (!inherits(study, "TumorModelProject")) {
    stop(
      "`study` must be a TumorModelProject object.",
      call. = FALSE
    )
  }

  if (
    length(port) != 1L ||
      is.na(port) ||
      !is.numeric(port) ||
      !is.finite(port) ||
      port < 1 ||
      port > 65535
  ) {
    stop(
      "`port` must be one number between 1 and 65535.",
      call. = FALSE
    )
  }

  port <- as.integer(port)

  app <- shiny::shinyApp(
    ui = study_app_ui(),

    server = function(input, output, session) {
      study_app_server(
        input = input,
        output = output,
        session = session,
        study = study
      )
    }
  )

  if (!is.null(proxy_base_url)) {
    proxy_base_url <- sub("/+$", "", proxy_base_url)

    shiny_url <- paste0(
      proxy_base_url,
      "/proxy/",
      port,
      "/"
    )

    cat(
      "\nShiny server is starting.\n",
      "Open the tumor-model study app at:\n",
      shiny_url,
      "\n\n",
      sep = ""
    )
  }

  shiny::runApp(
    app = app,
    host = host,
    port = port,
    launch.browser = FALSE
  )
}
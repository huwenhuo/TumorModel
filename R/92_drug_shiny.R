#' Drug Catalog Shiny Module UI
#'
#' Provides optional point-and-click entry for the shared drug catalog:
#' drugs, drug products, drug lots, and reusable treatment definitions.
#'
#' @param id Shiny module identifier.
#'
#' @return A Shiny tag list.
#' @export
drug_catalog_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::br(),

    shiny::tabsetPanel(
      id = ns("drug_tabs"),

      # =====================================================================
      # Drugs
      # =====================================================================
      shiny::tabPanel(
        title = "Drugs",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add drug"),

              shiny::textInput(
                inputId = ns("drug"),
                label = "Drug identifier",
                placeholder = "For example: anti_PD1"
              ),

              shiny::textInput(
                inputId = ns("formal_name"),
                label = "Formal drug name (optional)",
                placeholder = "For example: Pembrolizumab"
              ),

              shiny::textInput(
                inputId = ns("drug_type"),
                label = "Drug type",
                placeholder = "For example: Antibody, small molecule, vehicle"
              ),

              shiny::textInput(
                inputId = ns("drug_target"),
                label = "Target (optional)",
                placeholder = "For example: PD-1"
              ),

              shiny::textAreaInput(
                inputId = ns("drug_description"),
                label = "Description (optional)",
                rows = 2
              ),

              shiny::textAreaInput(
                inputId = ns("drug_notes"),
                label = "Notes (optional)",
                rows = 2
              ),

              shiny::actionButton(
                inputId = ns("add_drug"),
                label = "Add drug",
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
                inputId = ns("refresh_drugs"),
                label = "Refresh drugs",
                icon = shiny::icon("rotate")
              )
            ),

            shiny::h4("Shared drug catalog"),

            shiny::p(
              "Drug definitions are shared across experiments. ",
              "Drugs cannot be deleted from this screen."
            ),

            DT::DTOutput(ns("drug_table"))
          )
        )
      ),

      # =====================================================================
      # Drug products
      # =====================================================================
      shiny::tabPanel(
        title = "Drug products",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add drug product"),

              shiny::selectInput(
                inputId = ns("product_drug_pk"),
                label = "Drug",
                choices = character(0)
              ),

              shiny::textInput(
                inputId = ns("product_company_name"),
                label = "Company / manufacturer",
                placeholder = "For example: Bio X Cell"
              ),

              shiny::textInput(
                inputId = ns("product_catalog_number"),
                label = "Catalog number",
                placeholder = "For example: BE0146"
              ),

              shiny::textInput(
                inputId = ns("product_name"),
                label = "Product name (optional)",
                placeholder = "For example: InVivoMAb anti-mouse PD-1"
              ),

              shiny::numericInput(
                inputId = ns("stock_concentration"),
                label = "Stock concentration (optional)",
                value = NA_real_,
                min = 0
              ),

              shiny::textInput(
                inputId = ns("stock_concentration_unit"),
                label = "Stock concentration unit (optional)",
                placeholder = "For example: mg/mL"
              ),

              shiny::textInput(
                inputId = ns("product_formulation"),
                label = "Formulation (optional)",
                placeholder = "For example: 5 mg/mL in PBS"
              ),

              shiny::textInput(
                inputId = ns("storage_conditions"),
                label = "Storage conditions (optional)",
                placeholder = "For example: Store at 4 °C"
              ),

              shiny::textAreaInput(
                inputId = ns("product_notes"),
                label = "Notes (optional)",
                rows = 2
              ),

              shiny::actionButton(
                inputId = ns("add_drug_product"),
                label = "Add drug product",
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
                inputId = ns("refresh_products"),
                label = "Refresh products",
                icon = shiny::icon("rotate")
              )
            ),

            shiny::h4("Drug products"),

            shiny::p(
              "Products are linked to a shared drug. Products cannot be ",
              "deleted from this screen."
            ),

            DT::DTOutput(ns("product_table"))
          )
        )
      ),

      # =====================================================================
      # Drug lots
      # =====================================================================
      shiny::tabPanel(
        title = "Drug lots",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add drug lot"),

              shiny::selectInput(
                inputId = ns("lot_product_pk"),
                label = "Drug product",
                choices = character(0)
              ),

              shiny::textInput(
                inputId = ns("lot_number"),
                label = "Lot number",
                placeholder = "For example: 123456"
              ),

              shiny::dateInput(
                inputId = ns("lot_received_date"),
                label = "Received date (optional)",
                value = NULL,
                format = "yyyy-mm-dd"
              ),

              shiny::dateInput(
                inputId = ns("lot_expiration_date"),
                label = "Expiration date (optional)",
                value = NULL,
                format = "yyyy-mm-dd"
              ),

              shiny::dateInput(
                inputId = ns("lot_reconstitution_date"),
                label = "Reconstitution date (optional)",
                value = NULL,
                format = "yyyy-mm-dd"
              ),

              shiny::numericInput(
                inputId = ns("lot_reconstituted_concentration"),
                label = "Reconstituted concentration (optional)",
                value = NA_real_,
                min = 0
              ),

              shiny::textInput(
                inputId = ns("lot_reconstituted_concentration_unit"),
                label = "Reconstituted concentration unit (optional)",
                placeholder = "For example: mg/mL"
              ),

              shiny::textInput(
                inputId = ns("lot_storage_location"),
                label = "Storage location (optional)",
                placeholder = "For example: 4 °C refrigerator, shelf 2"
              ),

              shiny::textAreaInput(
                inputId = ns("lot_notes"),
                label = "Notes (optional)",
                rows = 2
              ),

              shiny::actionButton(
                inputId = ns("add_drug_lot"),
                label = "Add drug lot",
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
                inputId = ns("refresh_lots"),
                label = "Refresh lots",
                icon = shiny::icon("rotate")
              ),

              shiny::actionButton(
                inputId = ns("delete_drug_lot"),
                label = "Delete selected lot",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            ),

            shiny::h4("Drug lots"),

            shiny::p(
              class = "text-danger",
              "A lot cannot be deleted if it is referenced by treatment records."
            ),

            DT::DTOutput(ns("lot_table"))
          )
        )
      ),

      # =====================================================================
      # Reusable treatment definitions
      # =====================================================================
      shiny::tabPanel(
        title = "Treatment doses",

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::wellPanel(
              shiny::h4("Add treatment definition"),

              shiny::p(
                "A treatment definition links a product to a dose and ",
                "administration details. It can later be assigned to one ",
                "or more experiment-specific treatment groups."
              ),

              shiny::textInput(
                inputId = ns("treatment_name"),
                label = "Treatment name",
                placeholder = "For example: anti_PD1_10_mg_kg_IP"
              ),

              shiny::selectInput(
                inputId = ns("treatment_product_pk"),
                label = "Drug product",
                choices = character(0)
              ),

              shiny::numericInput(
                inputId = ns("treatment_dose"),
                label = "Treatment dose",
                value = NA_real_,
                min = 0
              ),

              shiny::textInput(
                inputId = ns("treatment_dose_unit"),
                label = "Dose unit",
                placeholder = "For example: mg/kg"
              ),

              shiny::textInput(
                inputId = ns("treatment_route"),
                label = "Route (optional)",
                placeholder = "For example: IP, PO, IV"
              ),

              shiny::numericInput(
                inputId = ns("treatment_component_volume_ul"),
                label = "Component volume (µL; optional)",
                value = NA_real_,
                min = 0
              ),

              shiny::textInput(
                inputId = ns("treatment_vehicle"),
                label = "Vehicle (optional)",
                placeholder = "For example: PBS"
              ),

              shiny::numericInput(
                inputId = ns("treatment_final_injection_volume_ul"),
                label = "Final injection volume (µL; optional)",
                value = NA_real_,
                min = 0
              ),

              shiny::textAreaInput(
                inputId = ns("treatment_description"),
                label = "Description (optional)",
                rows = 2
              ),

              shiny::textAreaInput(
                inputId = ns("treatment_notes"),
                label = "Notes (optional)",
                rows = 2
              ),

              shiny::actionButton(
                inputId = ns("add_treatment"),
                label = "Add treatment definition",
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
                inputId = ns("refresh_treatments"),
                label = "Refresh treatment definitions",
                icon = shiny::icon("rotate")
              ),

              shiny::actionButton(
                inputId = ns("delete_treatment"),
                label = "Delete selected treatment",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            ),

            shiny::h4("Reusable treatment definitions"),

            shiny::p(
              class = "text-danger",
              "A treatment definition cannot be deleted if it has been ",
              "assigned to a treatment group or used by other records."
            ),

            DT::DTOutput(ns("treatment_table"))
          )
        )
      )
    )
  )
}


#' Drug Catalog Shiny Module Server
#'
#' @param id Shiny module identifier.
#' @param study A TumorModelProject object.
#'
#' @return Invisibly, NULL.
#' @export
drug_catalog_server <- function(id, study) {
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

      drugs_data <- shiny::reactiveVal(study$drugs())
      products_data <- shiny::reactiveVal(study$drug_products())
      lots_data <- shiny::reactiveVal(study$drug_lots())
      treatments_data <- shiny::reactiveVal(study$treatments())

      lot_to_delete <- shiny::reactiveVal(NULL)
      treatment_to_delete <- shiny::reactiveVal(NULL)

      drug_table_proxy <- DT::dataTableProxy(
        outputId = "drug_table",
        session = session
      )

      product_table_proxy <- DT::dataTableProxy(
        outputId = "product_table",
        session = session
      )

      lot_table_proxy <- DT::dataTableProxy(
        outputId = "lot_table",
        session = session
      )

      treatment_table_proxy <- DT::dataTableProxy(
        outputId = "treatment_table",
        session = session
      )

      # ---------------------------------------------------------------------
      # Shared helpers
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

      product_record <- function(product_pk) {
        product_pk <- suppressWarnings(as.integer(product_pk))
        data <- products_data()

        if (is.na(product_pk) || nrow(data) == 0L) {
          return(NULL)
        }

        matches <- data[
          data$drug_product_pk == product_pk,
          ,
          drop = FALSE
        ]

        if (nrow(matches) != 1L) {
          return(NULL)
        }

        matches
      }

      refresh_drug_choices <- function() {
        data <- drugs_data()

        if (nrow(data) == 0L) {
          choices <- character(0)
        } else {
          choices <- stats::setNames(
            as.character(data$drug_pk),
            paste0(
              data$drug,
              ifelse(
                is.na(data$drug_name) |
                  data$drug_name == "" |
                  data$drug_name == data$drug,
                "",
                paste0(" — ", data$drug_name)
              )
            )
          )
        }

        shiny::updateSelectInput(
          session = session,
          inputId = "product_drug_pk",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_product_choices <- function() {
        data <- products_data()

        if (nrow(data) == 0L) {
          choices <- character(0)
        } else {
          product_label <- paste0(
            data$drug,
            " — ",
            data$company_name,
            " — ",
            data$catalog_number,
            ifelse(
              is.na(data$product_name) | data$product_name == "",
              "",
              paste0(" — ", data$product_name)
            )
          )

          choices <- stats::setNames(
            as.character(data$drug_product_pk),
            product_label
          )
        }

        shiny::updateSelectInput(
          session = session,
          inputId = "lot_product_pk",
          choices = choices
        )

        shiny::updateSelectInput(
          session = session,
          inputId = "treatment_product_pk",
          choices = choices
        )

        invisible(NULL)
      }

      refresh_drugs <- function(reset_paging = FALSE) {
        data <- study$drugs()
        drugs_data(data)

        DT::replaceData(
          proxy = drug_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        refresh_drug_choices()
        invisible(NULL)
      }

      refresh_products <- function(reset_paging = FALSE) {
        data <- study$drug_products()
        products_data(data)

        DT::replaceData(
          proxy = product_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        refresh_product_choices()
        invisible(NULL)
      }

      refresh_lots <- function(reset_paging = FALSE) {
        data <- study$drug_lots()
        lots_data(data)

        DT::replaceData(
          proxy = lot_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        invisible(NULL)
      }

      refresh_treatments <- function(reset_paging = FALSE) {
        data <- study$treatments()
        treatments_data(data)

        DT::replaceData(
          proxy = treatment_table_proxy,
          data = data,
          resetPaging = reset_paging,
          rownames = FALSE
        )

        invisible(NULL)
      }

      refresh_everything <- function() {
        refresh_drugs()
        refresh_products()
        refresh_lots()
        refresh_treatments()

        invisible(NULL)
      }

      # Populate select inputs after the module enters a reactive context.
      shiny::observeEvent(
        TRUE,
        {
          refresh_drug_choices()
          refresh_product_choices()
        },
        once = TRUE,
        ignoreInit = FALSE
      )

      # =====================================================================
      # Drugs
      # =====================================================================

      output$drug_table <- DT::renderDT({
        DT::datatable(
          drugs_data(),
          rownames = FALSE,
          selection = "none",

          options = list(
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
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

      shiny::observeEvent(input$refresh_drugs, {
        refresh_drugs()
      })

      shiny::observeEvent(input$add_drug, {
        tryCatch(
          {
            study$add_drug(
              drug = input$drug,
              drug_type = input$drug_type,
              formal_name = input$formal_name,
              target = input$drug_target,
              description = input$drug_description,
              notes = input$drug_notes
            )

            shiny::updateTextInput(
              session = session,
              inputId = "drug",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "formal_name",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "drug_type",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "drug_target",
              value = ""
            )

            shiny::updateTextAreaInput(
              session = session,
              inputId = "drug_description",
              value = ""
            )

            shiny::updateTextAreaInput(
              session = session,
              inputId = "drug_notes",
              value = ""
            )

            refresh_drugs()

            shiny::showNotification(
              "Drug added to the shared catalog.",
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
      # Drug products
      # =====================================================================

      output$product_table <- DT::renderDT({
        DT::datatable(
          products_data(),
          rownames = FALSE,
          selection = "none",

          options = list(
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
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

      shiny::observeEvent(input$refresh_products, {
        refresh_products()
      })

      shiny::observeEvent(input$add_drug_product, {
        drug_pk <- suppressWarnings(as.integer(input$product_drug_pk))
        data <- drugs_data()

        selected_drug <- data[
          data$drug_pk == drug_pk,
          ,
          drop = FALSE
        ]

        if (is.na(drug_pk) || nrow(selected_drug) != 1L) {
          shiny::showNotification(
            "Select a drug before adding a product.",
            type = "error",
            duration = 4
          )
          return()
        }

        tryCatch(
          {
            study$add_drug_product(
              drug = selected_drug$drug[[1L]],
              company_name = input$product_company_name,
              catalog_number = input$product_catalog_number,
              product_name = input$product_name,
              stock_concentration = nullable_number(
                input$stock_concentration
              ),
              stock_concentration_unit = input$stock_concentration_unit,
              formulation = input$product_formulation,
              storage_conditions = input$storage_conditions,
              notes = input$product_notes
            )

            shiny::updateTextInput(
              session = session,
              inputId = "product_company_name",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "product_catalog_number",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "product_name",
              value = ""
            )

            shiny::updateNumericInput(
              session = session,
              inputId = "stock_concentration",
              value = NA_real_
            )

            shiny::updateTextInput(
              session = session,
              inputId = "stock_concentration_unit",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "product_formulation",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "storage_conditions",
              value = ""
            )

            shiny::updateTextAreaInput(
              session = session,
              inputId = "product_notes",
              value = ""
            )

            refresh_products()

            shiny::showNotification(
              "Drug product added.",
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
      # Drug lots
      # =====================================================================

      output$lot_table <- DT::renderDT({
        DT::datatable(
          lots_data(),
          rownames = FALSE,
          selection = "single",

          options = list(
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
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

      shiny::observeEvent(input$refresh_lots, {
        refresh_lots()
      })

      shiny::observeEvent(input$add_drug_lot, {
        product <- product_record(input$lot_product_pk)

        if (is.null(product)) {
          shiny::showNotification(
            "Select a drug product before adding a lot.",
            type = "error",
            duration = 4
          )
          return()
        }

        tryCatch(
          {
            study$add_drug_lot(
              drug = product$drug[[1L]],
              company_name = product$company_name[[1L]],
              catalog_number = product$catalog_number[[1L]],
              lot_number = input$lot_number,
              received_date = nullable_date(input$lot_received_date),
              expiration_date = nullable_date(input$lot_expiration_date),
              reconstitution_date = nullable_date(
                input$lot_reconstitution_date
              ),
              reconstituted_concentration = nullable_number(
                input$lot_reconstituted_concentration
              ),
              reconstituted_concentration_unit = input$lot_reconstituted_concentration_unit,
              storage_location = input$lot_storage_location,
              notes = input$lot_notes
            )

            shiny::updateTextInput(
              session = session,
              inputId = "lot_number",
              value = ""
            )

            shiny::updateDateInput(
              session = session,
              inputId = "lot_received_date",
              value = NULL
            )

            shiny::updateDateInput(
              session = session,
              inputId = "lot_expiration_date",
              value = NULL
            )

            shiny::updateDateInput(
              session = session,
              inputId = "lot_reconstitution_date",
              value = NULL
            )

            shiny::updateNumericInput(
              session = session,
              inputId = "lot_reconstituted_concentration",
              value = NA_real_
            )

            shiny::updateTextInput(
              session = session,
              inputId = "lot_reconstituted_concentration_unit",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "lot_storage_location",
              value = ""
            )

            shiny::updateTextAreaInput(
              session = session,
              inputId = "lot_notes",
              value = ""
            )

            refresh_lots()

            shiny::showNotification(
              "Drug lot added.",
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

      shiny::observeEvent(input$delete_drug_lot, {
        selected_row <- input$lot_table_rows_selected
        data <- lots_data()

        if (length(selected_row) != 1L || nrow(data) == 0L) {
          shiny::showNotification(
            "Select one drug lot before deleting.",
            type = "error",
            duration = 4
          )
          return()
        }

        selected_lot <- data[selected_row, , drop = FALSE]
        lot_to_delete(selected_lot)

        shiny::showModal(
          shiny::modalDialog(
            title = "Delete drug lot?",

            shiny::tags$p(
              "You are about to permanently delete lot ",
              shiny::tags$strong(selected_lot$lot_number[[1L]]),
              "."
            ),

            shiny::tags$p(
              paste0(
                selected_lot$drug[[1L]],
                " — ",
                selected_lot$company_name[[1L]],
                " — ",
                selected_lot$catalog_number[[1L]]
              )
            ),

            shiny::tags$p(
              class = "text-danger",
              "This cannot be undone. Deletion will fail if the lot is ",
              "referenced by treatment records."
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),

              shiny::actionButton(
                inputId = session$ns("confirm_delete_drug_lot"),
                label = "Permanently delete",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_delete_drug_lot, {
        selected_lot <- lot_to_delete()

        if (is.null(selected_lot)) {
          shiny::removeModal()
          return()
        }

        tryCatch(
          {
            study$remove_drug_lot(
              drug = selected_lot$drug[[1L]],
              company_name = selected_lot$company_name[[1L]],
              catalog_number = selected_lot$catalog_number[[1L]],
              lot_number = selected_lot$lot_number[[1L]]
            )

            shiny::removeModal()
            lot_to_delete(NULL)
            refresh_lots()

            shiny::showNotification(
              "Drug lot deleted.",
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
      # Reusable treatment definitions
      # =====================================================================

      output$treatment_table <- DT::renderDT({
        DT::datatable(
          treatments_data(),
          rownames = FALSE,
          selection = "single",

          options = list(
            pageLength = 25,
            lengthMenu = c(10, 25, 50, 100),
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

      shiny::observeEvent(input$refresh_treatments, {
        refresh_treatments()
      })

      shiny::observeEvent(input$add_treatment, {
        product <- product_record(input$treatment_product_pk)

        if (is.null(product)) {
          shiny::showNotification(
            "Select a drug product before adding a treatment definition.",
            type = "error",
            duration = 4
          )
          return()
        }

        tryCatch(
          {
            study$add_treatment(
              treatment_name = input$treatment_name,
              drug = product$drug[[1L]],
              company_name = product$company_name[[1L]],
              catalog_number = product$catalog_number[[1L]],
              treatment_dose = input$treatment_dose,
              dose_unit = input$treatment_dose_unit,
              route = input$treatment_route,
              component_volume_ul = nullable_number(
                input$treatment_component_volume_ul
              ),
              vehicle = input$treatment_vehicle,
              final_injection_volume_ul = nullable_number(
                input$treatment_final_injection_volume_ul
              ),
              description = input$treatment_description,
              notes = input$treatment_notes
            )

            shiny::updateTextInput(
              session = session,
              inputId = "treatment_name",
              value = ""
            )

            shiny::updateNumericInput(
              session = session,
              inputId = "treatment_dose",
              value = NA_real_
            )

            shiny::updateTextInput(
              session = session,
              inputId = "treatment_dose_unit",
              value = ""
            )

            shiny::updateTextInput(
              session = session,
              inputId = "treatment_route",
              value = ""
            )

            shiny::updateNumericInput(
              session = session,
              inputId = "treatment_component_volume_ul",
              value = NA_real_
            )

            shiny::updateTextInput(
              session = session,
              inputId = "treatment_vehicle",
              value = ""
            )

            shiny::updateNumericInput(
              session = session,
              inputId = "treatment_final_injection_volume_ul",
              value = NA_real_
            )

            shiny::updateTextAreaInput(
              session = session,
              inputId = "treatment_description",
              value = ""
            )

            shiny::updateTextAreaInput(
              session = session,
              inputId = "treatment_notes",
              value = ""
            )

            refresh_treatments()

            shiny::showNotification(
              "Treatment definition added.",
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

      shiny::observeEvent(input$delete_treatment, {
        selected_row <- input$treatment_table_rows_selected
        data <- treatments_data()

        if (length(selected_row) != 1L || nrow(data) == 0L) {
          shiny::showNotification(
            "Select one treatment definition before deleting.",
            type = "error",
            duration = 4
          )
          return()
        }

        selected_treatment <- data[selected_row, , drop = FALSE]
        treatment_to_delete(selected_treatment)

        shiny::showModal(
          shiny::modalDialog(
            title = "Delete treatment definition?",

            shiny::tags$p(
              "You are about to permanently delete treatment ",
              shiny::tags$strong(
                selected_treatment$treatment_name[[1L]]
              ),
              "."
            ),

            shiny::tags$p(
              class = "text-danger",
              "This cannot be undone. Deletion will fail if this treatment ",
              "is assigned to a treatment group or referenced elsewhere."
            ),

            footer = shiny::tagList(
              shiny::modalButton("Cancel"),

              shiny::actionButton(
                inputId = session$ns("confirm_delete_treatment"),
                label = "Permanently delete",
                icon = shiny::icon("trash"),
                class = "btn-danger"
              )
            )
          )
        )
      })

      shiny::observeEvent(input$confirm_delete_treatment, {
        selected_treatment <- treatment_to_delete()

        if (is.null(selected_treatment)) {
          shiny::removeModal()
          return()
        }

        tryCatch(
          {
            study$remove_treatment(
              treatment_name = selected_treatment$treatment_name[[1L]]
            )

            shiny::removeModal()
            treatment_to_delete(NULL)
            refresh_treatments()

            shiny::showNotification(
              "Treatment definition deleted.",
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

      invisible(NULL)
    }
  )
}
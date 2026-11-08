# Shared drug catalog, drug products, drug lots, and reusable treatment
# definitions.
#
# These records are global to the project and can be reused in more than one
# experiment.
#
# Experiment-specific treatment groups and group regimens are defined in:
# R/05_drug_2ndpart.R
#
# Add `.tm_drug_public_methods` to public = c(...) in
# R/90_tumormodel_project.R.


.tm_treatment_date <- function(x, argument_name) {
  if (length(x) != 1L || is.na(x)) {
    stop(
      paste0(
        "`", argument_name,
        "` must be one YYYY-MM-DD date."
      ),
      call. = FALSE
    )
  }

  x <- as.character(x)

  value <- suppressWarnings(
    as.Date(x, format = "%Y-%m-%d")
  )

  if (
    is.na(value) ||
      !identical(format(value, "%Y-%m-%d"), x)
  ) {
    stop(
      paste0(
        "`", argument_name,
        "` must be one YYYY-MM-DD date."
      ),
      call. = FALSE
    )
  }

  format(value, "%Y-%m-%d")
}


.tm_optional_treatment_date <- function(
  x,
  argument_name
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      is.na(x) ||
      !nzchar(trimws(as.character(x)))
  ) {
    return(NA_character_)
  }

  .tm_treatment_date(x, argument_name)
}


.tm_optional_number <- function(
  x,
  argument_name,
  positive = FALSE
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      is.na(x) ||
      !nzchar(trimws(as.character(x)))
  ) {
    return(NA_real_)
  }

  value <- suppressWarnings(as.numeric(x))

  invalid <- (
    length(value) != 1L ||
      is.na(value) ||
      !is.finite(value) ||
      (positive && value <= 0) ||
      (!positive && value < 0)
  )

  if (invalid) {
    qualifier <- if (positive) {
      "positive"
    } else {
      "non-negative"
    }

    stop(
      paste0(
        "`", argument_name,
        "` must be one ", qualifier,
        " numeric value or NULL."
      ),
      call. = FALSE
    )
  }

  value
}


.tm_text <- function(
  x,
  argument_name,
  required = FALSE
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      is.na(x)
  ) {
    if (isTRUE(required)) {
      stop(
        paste0("`", argument_name, "` is required."),
        call. = FALSE
      )
    }

    return(NA_character_)
  }

  if (length(x) != 1L) {
    stop(
      paste0(
        "`", argument_name,
        "` must contain one value."
      ),
      call. = FALSE
    )
  }

  value <- trimws(as.character(x))

  if (!nzchar(value)) {
    if (isTRUE(required)) {
      stop(
        paste0("`", argument_name, "` is required."),
        call. = FALSE
      )
    }

    return(NA_character_)
  }

  value
}


.tm_optional_integer <- function(
  x,
  argument_name
) {
  if (
    is.null(x) ||
      length(x) == 0L ||
      is.na(x) ||
      !nzchar(trimws(as.character(x)))
  ) {
    return(NA_integer_)
  }

  value <- suppressWarnings(as.integer(x))

  if (length(value) != 1L || is.na(value)) {
    stop(
      paste0(
        "`", argument_name,
        "` must be one integer or NULL."
      ),
      call. = FALSE
    )
  }

  value
}


.tm_one_row <- function(
  con,
  query,
  params,
  description
) {
  record <- DBI::dbGetQuery(
    con,
    query,
    params = params
  )

  if (nrow(record) == 0L) {
    stop(
      "No ", description, " was found.",
      call. = FALSE
    )
  }

  if (nrow(record) > 1L) {
    stop(
      "More than one ", description,
      " was found. Please use a more specific identifier.",
      call. = FALSE
    )
  }

  record
}


.tm_drug_pk <- function(con, drug) {
  drug <- .tm_text(
    drug,
    "drug",
    required = TRUE
  )

  record <- .tm_one_row(
    con = con,
    query = paste(
      "SELECT drug_pk",
      "FROM drugs",
      "WHERE drug = ?"
    ),
    params = list(drug),
    description = paste0(
      "drug `",
      drug,
      "`"
    )
  )

  record$drug_pk[[1L]]
}


.tm_product_pk <- function(
  con,
  drug,
  company_name,
  catalog_number
) {
  drug_pk <- .tm_drug_pk(
    con = con,
    drug = drug
  )

  company_name <- .tm_text(
    company_name,
    "company_name",
    required = TRUE
  )

  catalog_number <- .tm_text(
    catalog_number,
    "catalog_number",
    required = TRUE
  )

  record <- .tm_one_row(
    con = con,
    query = paste(
      "SELECT drug_product_pk",
      "FROM drug_products",
      "WHERE drug_pk = ?",
      "AND company_name = ?",
      "AND catalog_number = ?"
    ),
    params = list(
      drug_pk,
      company_name,
      catalog_number
    ),
    description = paste0(
      "product for drug `",
      drug,
      "`, company `",
      company_name,
      "`, catalog number `",
      catalog_number,
      "`"
    )
  )

  record$drug_product_pk[[1L]]
}


.tm_lot_pk <- function(
  con,
  drug,
  company_name,
  catalog_number,
  lot_number
) {
  product_pk <- .tm_product_pk(
    con = con,
    drug = drug,
    company_name = company_name,
    catalog_number = catalog_number
  )

  lot_number <- .tm_text(
    lot_number,
    "lot_number",
    required = TRUE
  )

  record <- .tm_one_row(
    con = con,
    query = paste(
      "SELECT drug_lot_pk",
      "FROM drug_lots",
      "WHERE drug_product_pk = ?",
      "AND lot_number = ?"
    ),
    params = list(
      product_pk,
      lot_number
    ),
    description = paste0(
      "drug lot `",
      lot_number,
      "`"
    )
  )

  record$drug_lot_pk[[1L]]
}


.tm_treatment_pk <- function(
  con,
  treatment_name
) {
  treatment_name <- .tm_text(
    treatment_name,
    "treatment_name",
    required = TRUE
  )

  record <- .tm_one_row(
    con = con,
    query = paste(
      "SELECT treatment_pk",
      "FROM treatments",
      "WHERE treatment_name = ?"
    ),
    params = list(treatment_name),
    description = paste0(
      "treatment `",
      treatment_name,
      "`"
    )
  )

  record$treatment_pk[[1L]]
}


.tm_resolve_lot_number <- function(
  con,
  drug,
  company_name,
  catalog_number,
  lot_numbers = NULL
) {
  drug <- .tm_text(
    drug,
    "drug",
    required = TRUE
  )

  company_name <- .tm_text(
    company_name,
    "company_name",
    required = TRUE
  )

  catalog_number <- .tm_text(
    catalog_number,
    "catalog_number",
    required = TRUE
  )

  if (
    !is.null(lot_numbers) &&
      drug %in% names(lot_numbers)
  ) {
    lot_number <- lot_numbers[[drug]]

    if (
      !is.na(lot_number) &&
        nzchar(trimws(as.character(lot_number)))
    ) {
      .tm_lot_pk(
        con = con,
        drug = drug,
        company_name = company_name,
        catalog_number = catalog_number,
        lot_number = lot_number
      )

      return(as.character(lot_number))
    }
  }

  product_pk <- .tm_product_pk(
    con = con,
    drug = drug,
    company_name = company_name,
    catalog_number = catalog_number
  )

  lots <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT lot_number",
      "FROM drug_lots",
      "WHERE drug_product_pk = ?",
      "ORDER BY lot_number"
    ),
    params = list(product_pk)
  )

  if (nrow(lots) == 0L) {
    stop(
      "No drug lot is registered for drug `",
      drug,
      "` (",
      company_name,
      ", ",
      catalog_number,
      ").",
      call. = FALSE
    )
  }

  if (nrow(lots) > 1L) {
    stop(
      "More than one lot is registered for drug `",
      drug,
      "` (",
      company_name,
      ", ",
      catalog_number,
      "): ",
      paste(lots$lot_number, collapse = ", "),
      ". Supply `lot_numbers`, for example c(",
      drug,
      " = \"LOT_NUMBER\").",
      call. = FALSE
    )
  }

  lots$lot_number[[1L]]
}


.tm_drug_public_methods <- list(

  #' Return the Shared Drug Catalog
  drugs = function() {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    DBI::dbGetQuery(
      con,
      paste(
        "SELECT",
        "drug_pk,",
        "drug,",
        "drug_name,",
        "drug_type,",
        "target,",
        "description,",
        "notes,",
        "created_at_utc,",
        "updated_at_utc",
        "FROM drugs",
        "ORDER BY drug"
      )
    )
  },


  #' Add a Drug to the Shared Catalog
  add_drug = function(
    drug,
    drug_type,
    formal_name = NULL,
    target = NULL,
    description = NULL,
    notes = NULL
  ) {
    drug <- .tm_text(
      drug,
      "drug",
      required = TRUE
    )

    drug_type <- .tm_text(
      drug_type,
      "drug_type",
      required = TRUE
    )

    formal_name <- .tm_text(
      formal_name,
      "formal_name"
    )

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    now <- .tm_now_utc()

    tryCatch(
      {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO drugs (",
            "drug_name,",
            "drug,",
            "drug_type,",
            "target,",
            "description,",
            "notes,",
            "created_at_utc,",
            "updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            ifelse(is.na(formal_name), drug, formal_name),
            drug,
            drug_type,
            .tm_text(target, "target"),
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
            "A drug with identifier `",
            drug,
            "` already exists.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  },


  #' Return Drug Products
  drug_products = function(drug = NULL) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    query <- paste(
      "SELECT",
      "dp.drug_product_pk,",
      "d.drug,",
      "d.drug_name,",
      "dp.company_name,",
      "dp.catalog_number,",
      "dp.product_name,",
      "dp.stock_concentration,",
      "dp.stock_concentration_unit,",
      "dp.formulation,",
      "dp.storage_conditions,",
      "dp.notes,",
      "dp.created_at_utc,",
      "dp.updated_at_utc",
      "FROM drug_products AS dp",
      "INNER JOIN drugs AS d",
      "ON d.drug_pk = dp.drug_pk"
    )

    if (is.null(drug)) {
      return(
        DBI::dbGetQuery(
          con,
          paste(
            query,
            "ORDER BY d.drug, dp.company_name, dp.catalog_number"
          )
        )
      )
    }

    drug_pk <- .tm_drug_pk(
      con = con,
      drug = drug
    )

    DBI::dbGetQuery(
      con,
      paste(
        query,
        "WHERE dp.drug_pk = ?",
        "ORDER BY dp.company_name, dp.catalog_number"
      ),
      params = list(drug_pk)
    )
  },


  #' Add a Product to the Shared Drug Catalog
  add_drug_product = function(
    drug,
    company_name,
    catalog_number,
    product_name = NULL,
    stock_concentration = NULL,
    stock_concentration_unit = NULL,
    formulation = NULL,
    storage_conditions = NULL,
    notes = NULL
  ) {
    company_name <- .tm_text(
      company_name,
      "company_name",
      required = TRUE
    )

    catalog_number <- .tm_text(
      catalog_number,
      "catalog_number",
      required = TRUE
    )

    stock_concentration <- .tm_optional_number(
      stock_concentration,
      "stock_concentration"
    )

    stock_concentration_unit <- .tm_text(
      stock_concentration_unit,
      "stock_concentration_unit"
    )

    if (
      is.na(stock_concentration) !=
        is.na(stock_concentration_unit)
    ) {
      stop(
        paste(
          "`stock_concentration` and",
          "`stock_concentration_unit` must either both be supplied",
          "or both be NULL."
        ),
        call. = FALSE
      )
    }

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    drug_pk <- .tm_drug_pk(
      con = con,
      drug = drug
    )

    now <- .tm_now_utc()

    tryCatch(
      {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO drug_products (",
            "drug_pk,",
            "company_name,",
            "catalog_number,",
            "product_name,",
            "stock_concentration,",
            "stock_concentration_unit,",
            "formulation,",
            "storage_conditions,",
            "notes,",
            "created_at_utc,",
            "updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            drug_pk,
            company_name,
            catalog_number,
            .tm_text(product_name, "product_name"),
            stock_concentration,
            stock_concentration_unit,
            .tm_text(formulation, "formulation"),
            .tm_text(storage_conditions, "storage_conditions"),
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
            "This drug product already exists for drug `",
            drug,
            "`, company `",
            company_name,
            "`, catalog number `",
            catalog_number,
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


  #' Return Drug Lots
  drug_lots = function(drug = NULL) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    query <- paste(
      "SELECT",
      "dl.drug_lot_pk,",
      "d.drug,",
      "d.drug_name,",
      "dp.company_name,",
      "dp.catalog_number,",
      "dp.product_name,",
      "dl.lot_number,",
      "dl.received_date,",
      "dl.expiration_date,",
      "dl.reconstitution_date,",
      "dl.reconstituted_concentration,",
      "dl.reconstituted_concentration_unit,",
      "dl.storage_location,",
      "dl.notes,",
      "dl.created_at_utc,",
      "dl.updated_at_utc",
      "FROM drug_lots AS dl",
      "INNER JOIN drug_products AS dp",
      "ON dp.drug_product_pk = dl.drug_product_pk",
      "INNER JOIN drugs AS d",
      "ON d.drug_pk = dp.drug_pk"
    )

    if (is.null(drug)) {
      return(
        DBI::dbGetQuery(
          con,
          paste(
            query,
            "ORDER BY d.drug, dp.company_name, dl.lot_number"
          )
        )
      )
    }

    drug_pk <- .tm_drug_pk(
      con = con,
      drug = drug
    )

    DBI::dbGetQuery(
      con,
      paste(
        query,
        "WHERE dp.drug_pk = ?",
        "ORDER BY dp.company_name, dl.lot_number"
      ),
      params = list(drug_pk)
    )
  },


  #' Add a Drug Lot
  add_drug_lot = function(
    drug,
    company_name,
    catalog_number,
    lot_number,
    received_date = NULL,
    expiration_date = NULL,
    reconstitution_date = NULL,
    reconstituted_concentration = NULL,
    reconstituted_concentration_unit = NULL,
    storage_location = NULL,
    notes = NULL
  ) {
    lot_number <- .tm_text(
      lot_number,
      "lot_number",
      required = TRUE
    )

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    product_pk <- .tm_product_pk(
      con = con,
      drug = drug,
      company_name = company_name,
      catalog_number = catalog_number
    )

    now <- .tm_now_utc()

    tryCatch(
      {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO drug_lots (",
            "drug_product_pk,",
            "lot_number,",
            "received_date,",
            "expiration_date,",
            "reconstitution_date,",
            "reconstituted_concentration,",
            "reconstituted_concentration_unit,",
            "storage_location,",
            "notes,",
            "created_at_utc,",
            "updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            product_pk,
            lot_number,
            .tm_optional_treatment_date(
              received_date,
              "received_date"
            ),
            .tm_optional_treatment_date(
              expiration_date,
              "expiration_date"
            ),
            .tm_optional_treatment_date(
              reconstitution_date,
              "reconstitution_date"
            ),
            .tm_optional_number(
              reconstituted_concentration,
              "reconstituted_concentration"
            ),
            .tm_text(
              reconstituted_concentration_unit,
              "reconstituted_concentration_unit"
            ),
            .tm_text(storage_location, "storage_location"),
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
            "Lot `",
            lot_number,
            "` already exists for the specified product.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  },


  #' Remove a Drug Lot
  remove_drug_lot = function(
    drug,
    company_name,
    catalog_number,
    lot_number
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    drug_lot_pk <- .tm_lot_pk(
      con = con,
      drug = drug,
      company_name = company_name,
      catalog_number = catalog_number,
      lot_number = lot_number
    )

    tryCatch(
      {
        DBI::dbExecute(
          con,
          "DELETE FROM drug_lots WHERE drug_lot_pk = ?",
          params = list(drug_lot_pk)
        )
      },
      error = function(e) {
        if (grepl(
          "FOREIGN KEY constraint failed",
          conditionMessage(e),
          fixed = TRUE
        )) {
          stop(
            "This drug lot cannot be removed because it is referenced ",
            "by one or more treatment records.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  },


  #' Return Reusable Treatment Definitions
  treatments = function(
    treatment_name = NULL,
    drug = NULL
  ) {
    if (!is.null(treatment_name) && !is.null(drug)) {
      stop(
        "Supply either `treatment_name` or `drug`, not both.",
        call. = FALSE
      )
    }

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    query <- paste(
      "SELECT",
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
      "t.description,",
      "t.notes,",
      "t.created_at_utc,",
      "t.updated_at_utc",
      "FROM treatments AS t",
      "INNER JOIN drug_products AS dp",
      "ON dp.drug_product_pk = t.drug_product_pk",
      "INNER JOIN drugs AS d",
      "ON d.drug_pk = dp.drug_pk"
    )

    if (!is.null(treatment_name)) {
      treatment_name <- .tm_text(
        treatment_name,
        "treatment_name",
        required = TRUE
      )

      return(
        DBI::dbGetQuery(
          con,
          paste(
            query,
            "WHERE t.treatment_name = ?",
            "ORDER BY t.treatment_name"
          ),
          params = list(treatment_name)
        )
      )
    }

    if (!is.null(drug)) {
      drug <- .tm_text(
        drug,
        "drug",
        required = TRUE
      )

      return(
        DBI::dbGetQuery(
          con,
          paste(
            query,
            "WHERE d.drug = ?",
            "ORDER BY t.treatment_name"
          ),
          params = list(drug)
        )
      )
    }

    DBI::dbGetQuery(
      con,
      paste(
        query,
        "ORDER BY d.drug, t.treatment_name"
      )
    )
  },


  #' Add a Reusable Treatment Definition
  add_treatment = function(
    treatment_name,
    drug,
    company_name,
    catalog_number,
    treatment_dose,
    dose_unit,
    route = NULL,
    component_volume_ul = NULL,
    vehicle = NULL,
    final_injection_volume_ul = NULL,
    description = NULL,
    notes = NULL
  ) {
    treatment_name <- .tm_text(
      treatment_name,
      "treatment_name",
      required = TRUE
    )

    treatment_dose <- .tm_optional_number(
      treatment_dose,
      "treatment_dose",
      positive = TRUE
    )

    if (is.na(treatment_dose)) {
      stop(
        "`treatment_dose` is required and must be positive.",
        call. = FALSE
      )
    }

    dose_unit <- .tm_text(
      dose_unit,
      "dose_unit",
      required = TRUE
    )

    component_volume_ul <- .tm_optional_number(
      component_volume_ul,
      "component_volume_ul",
      positive = TRUE
    )

    final_injection_volume_ul <- .tm_optional_number(
      final_injection_volume_ul,
      "final_injection_volume_ul",
      positive = TRUE
    )

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    product_pk <- .tm_product_pk(
      con = con,
      drug = drug,
      company_name = company_name,
      catalog_number = catalog_number
    )

    now <- .tm_now_utc()

    tryCatch(
      {
        DBI::dbExecute(
          con,
          paste(
            "INSERT INTO treatments (",
            "treatment_name,",
            "drug_product_pk,",
            "treatment_dose,",
            "dose_unit,",
            "route,",
            "component_volume_ul,",
            "vehicle,",
            "final_injection_volume_ul,",
            "description,",
            "notes,",
            "created_at_utc,",
            "updated_at_utc",
            ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          ),
          params = list(
            treatment_name,
            product_pk,
            treatment_dose,
            dose_unit,
            .tm_text(route, "route"),
            component_volume_ul,
            .tm_text(vehicle, "vehicle"),
            final_injection_volume_ul,
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
            "A treatment named `",
            treatment_name,
            "` already exists.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  },


  #' Remove a Reusable Treatment Definition
  remove_treatment = function(treatment_name) {
    treatment_name <- .tm_text(
      treatment_name,
      "treatment_name",
      required = TRUE
    )

    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_ensure_treatment_schema(con)

    treatment_pk <- .tm_treatment_pk(
      con = con,
      treatment_name = treatment_name
    )

    tryCatch(
      {
        DBI::dbExecute(
          con,
          "DELETE FROM treatments WHERE treatment_pk = ?",
          params = list(treatment_pk)
        )
      },
      error = function(e) {
        if (grepl(
          "FOREIGN KEY constraint failed",
          conditionMessage(e),
          fixed = TRUE
        )) {
          stop(
            "Treatment `",
            treatment_name,
            "` cannot be removed because it is assigned to a ",
            "treatment group or used by other records.",
            call. = FALSE
          )
        }

        stop(e)
      }
    )

    private$touch_project()

    invisible(self)
  }
)
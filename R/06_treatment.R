# Animal treatment assignments, treatment-day rosters, and actual
# treatment-dose administration records.
#
# Requires:
#   * 05_drug.R
#   * 05_drug_2ndpart.R
#
# Treatment groups are experiment-specific. Treatments, drugs, products,
# and lots are shared catalog records.
#
# Add .tm_treatment_public_methods to public = c(...) in
# R/90_tumormodel_project.R.


.tm_animal_pk_in_experiment <- function(
  con,
  experiment_id,
  animal_id
) {
  animal_id <- .tm_text(
    animal_id,
    "animal_id",
    required = TRUE
  )

  record <- .tm_one_row(
    con = con,
    query = paste(
      "SELECT",
      "  a.animal_pk,",
      "  a.animal_id,",
      "  a.cage_id,",
      "  ea.status AS experiment_animal_status,",
      "  a.killed_date",
      "FROM experiment_animals AS ea",
      "INNER JOIN animals AS a",
      "  ON a.animal_pk = ea.animal_pk",
      "WHERE ea.experiment_id = ?",
      "  AND trim(a.animal_id) = ?"
    ),
    params = list(
      experiment_id,
      animal_id
    ),
    description = paste0(
      "enrolled animal `",
      animal_id,
      "` in experiment `",
      experiment_id,
      "`"
    )
  )

  record
}


.tm_assignment_row <- function(
  con,
  experiment_id,
  animal_id,
  treatment_group,
  administration_date = NULL
) {
  animal <- .tm_animal_pk_in_experiment(
    con = con,
    experiment_id = experiment_id,
    animal_id = animal_id
  )

  group_pk <- .tm_group_pk(
    con = con,
    experiment_id = experiment_id,
    treatment_group = treatment_group
  )

  query <- paste(
    "SELECT",
    "  ata.assignment_pk,",
    "  ata.animal_pk,",
    "  ata.treatment_group_pk,",
    "  ata.assignment_date,",
    "  ata.treatment_start_date,",
    "  ata.treatment_end_date,",
    "  ata.notes",
    "FROM animal_treatment_assignments AS ata",
    "INNER JOIN treatment_groups AS tg",
    "  ON tg.treatment_group_pk = ata.treatment_group_pk",
    "WHERE ata.animal_pk = ?",
    "  AND ata.treatment_group_pk = ?",
    "  AND tg.experiment_id = ?"
  )

  params <- list(
    animal$animal_pk[[1L]],
    group_pk,
    experiment_id
  )

  if (!is.null(administration_date)) {
    administration_date <- .tm_treatment_date(
      administration_date,
      "administration_date"
    )

    query <- paste(
      query,
      "AND ata.treatment_start_date <= ?",
      "AND (",
      "  ata.treatment_end_date IS NULL",
      "  OR trim(ata.treatment_end_date) = ''",
      "  OR ata.treatment_end_date >= ?",
      ")"
    )

    params <- c(
      params,
      list(
        administration_date,
        administration_date
      )
    )
  }

  query <- paste(
    query,
    "ORDER BY",
    "  ata.treatment_start_date DESC,",
    "  ata.assignment_date DESC,",
    "  ata.assignment_pk DESC"
  )

  record <- DBI::dbGetQuery(
    con,
    query,
    params = params
  )

  if (nrow(record) == 0L) {
    if (is.null(administration_date)) {
      stop(
        "No treatment assignment was found for animal `",
        animal_id,
        "` and group `",
        treatment_group,
        "` in active experiment `",
        experiment_id,
        "`.",
        call. = FALSE
      )
    }

    stop(
      "No treatment assignment was active for animal `",
      animal_id,
      "` in group `",
      treatment_group,
      "` on ",
      administration_date,
      ".",
      call. = FALSE
    )
  }

  if (nrow(record) > 1L) {
    stop(
      "More than one treatment assignment matched animal `",
      animal_id,
      "`, group `",
      treatment_group,
      "`, and date `",
      administration_date,
      "`. Resolve overlapping assignments first.",
      call. = FALSE
    )
  }

  record
}


.tm_animal_alive_on_date <- function(
  con,
  experiment_id,
  animal_id,
  administration_date
) {
  administration_date <- .tm_treatment_date(
    administration_date,
    "administration_date"
  )

  animal <- .tm_animal_pk_in_experiment(
    con = con,
    experiment_id = experiment_id,
    animal_id = animal_id
  )

  killed_date <- animal$killed_date[[1L]]

  if (
    !is.na(killed_date) &&
      nzchar(trimws(as.character(killed_date)))
  ) {
    killed_date <- .tm_treatment_date(
      killed_date,
      "killed_date"
    )

    if (administration_date >= killed_date) {
      stop(
        "Animal `",
        animal_id,
        "` was killed/euthanized on ",
        killed_date,
        "` and cannot receive treatment on ",
        administration_date,
        ".",
        call. = FALSE
      )
    }
  }

  experiment_status <- trimws(
    as.character(animal$experiment_animal_status[[1L]])
  )

  if (
    !is.na(experiment_status) &&
      tolower(experiment_status) == "killed"
  ) {
    stop(
      "Animal `",
      animal_id,
      "` is marked Killed in active experiment `",
      experiment_id,
      "` and cannot receive treatment.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.tm_regimen_for_product_row <- function(
  con,
  experiment_id,
  treatment_group,
  drug,
  company_name,
  catalog_number
) {
  group_pk <- .tm_group_pk(
    con = con,
    experiment_id = experiment_id,
    treatment_group = treatment_group
  )

  product_pk <- .tm_product_pk(
    con = con,
    drug = drug,
    company_name = company_name,
    catalog_number = catalog_number
  )

  record <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "  r.regimen_pk,",
      "  r.treatment_group_pk,",
      "  r.treatment_pk,",
      "  t.treatment_name,",
      "  t.drug_product_pk,",
      "  t.treatment_dose,",
      "  t.dose_unit,",
      "  t.route,",
      "  t.component_volume_ul,",
      "  t.final_injection_volume_ul",
      "FROM treatment_group_regimens AS r",
      "INNER JOIN treatment_groups AS tg",
      "  ON tg.treatment_group_pk = r.treatment_group_pk",
      "INNER JOIN treatments AS t",
      "  ON t.treatment_pk = r.treatment_pk",
      "WHERE r.treatment_group_pk = ?",
      "  AND tg.experiment_id = ?",
      "  AND t.drug_product_pk = ?"
    ),
    params = list(
      group_pk,
      experiment_id,
      product_pk
    )
  )

  if (nrow(record) == 0L) {
    stop(
      "No regimen was found for treatment group `",
      treatment_group,
      "` and drug product `",
      drug,
      "` / `",
      company_name,
      "` / `",
      catalog_number,
      "` in the active experiment.",
      call. = FALSE
    )
  }

  if (nrow(record) > 1L) {
    stop(
      "More than one regimen matches treatment group `",
      treatment_group,
      "` and drug product `",
      drug,
      "` / `",
      company_name,
      "` / `",
      catalog_number,
      "`. Use distinct drug products or treatment definitions.",
      call. = FALSE
    )
  }

  record
}


.tm_treatment_public_methods <- list(

  #' Return Treatment Assignments in the Active Experiment
  treatment_assignments = function(
    animal_id = NULL,
    treatment_group = NULL,
    active_on_date = NULL,
    alive_only = FALSE,
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
      "  ata.assignment_pk,",
      "  tg.experiment_id,",
      "  ata.animal_pk,",
      "  a.animal_id,",
      "  a.cage_id,",
      "  ea.status AS animal_status,",
      "  a.killed_date,",
      "  ata.treatment_group_pk,",
      "  tg.group_name AS treatment_group,",
      "  tg.description AS treatment_description,",
      "  ata.assignment_date,",
      "  ata.treatment_start_date,",
      "  ata.treatment_end_date,",
      "  ata.notes,",
      "  ata.created_at_utc,",
      "  ata.updated_at_utc",
      "FROM animal_treatment_assignments AS ata",
      "INNER JOIN animals AS a",
      "  ON a.animal_pk = ata.animal_pk",
      "INNER JOIN treatment_groups AS tg",
      "  ON tg.treatment_group_pk = ata.treatment_group_pk",
      "INNER JOIN experiment_animals AS ea",
      "  ON ea.experiment_id = tg.experiment_id",
      "  AND ea.animal_pk = ata.animal_pk",
      "WHERE tg.experiment_id = ?"
    )

    params <- list(experiment_id)

    if (!is.null(animal_id)) {
      query <- paste(
        query,
        "AND trim(a.animal_id) = ?"
      )

      params <- c(
        params,
        list(
          .tm_text(
            animal_id,
            "animal_id",
            required = TRUE
          )
        )
      )
    }

    if (!is.null(treatment_group)) {
      query <- paste(
        query,
        "AND tg.group_name = ?"
      )

      params <- c(
        params,
        list(
          .tm_text(
            treatment_group,
            "treatment_group",
            required = TRUE
          )
        )
      )
    }

    if (!is.null(active_on_date)) {
      active_on_date <- .tm_treatment_date(
        active_on_date,
        "active_on_date"
      )

      query <- paste(
        query,
        "AND ata.treatment_start_date <= ?",
        "AND (",
        "  ata.treatment_end_date IS NULL",
        "  OR trim(ata.treatment_end_date) = ''",
        "  OR ata.treatment_end_date >= ?",
        ")"
      )

      params <- c(
        params,
        list(
          active_on_date,
          active_on_date
        )
      )

      if (isTRUE(alive_only)) {
        query <- paste(
          query,
          "AND (",
          "  a.killed_date IS NULL",
          "  OR trim(a.killed_date) = ''",
          "  OR a.killed_date > ?",
          ")",
          "AND (",
          "  ea.status IS NULL",
          "  OR lower(trim(ea.status)) != 'killed'",
          ")"
        )

        params <- c(
          params,
          list(active_on_date)
        )
      }
    } else if (isTRUE(alive_only)) {
      stop(
        "`alive_only = TRUE` requires `active_on_date`.",
        call. = FALSE
      )
    }

    query <- paste(
      query,
      "ORDER BY",
      "ata.treatment_start_date,",
      "tg.group_name,",
      "a.cage_id,",
      "a.animal_id"
    )

    DBI::dbGetQuery(
      con,
      query,
      params = params
    )
  },


  #' Assign an Active-Experiment Animal to a Treatment Group
  assign_animal_to_treatment_group = function(
    animal_id,
    treatment_group,
    treatment_start_date,
    assignment_date = treatment_start_date,
    treatment_end_date = NULL,
    notes = NULL,
    user_id = .tm_default_user_id()
  ) {
    treatment_start_date <- .tm_treatment_date(
      treatment_start_date,
      "treatment_start_date"
    )

    assignment_date <- .tm_treatment_date(
      assignment_date,
      "assignment_date"
    )

    treatment_end_date <- .tm_optional_treatment_date(
      treatment_end_date,
      "treatment_end_date"
    )

    if (
      !is.na(treatment_end_date) &&
        treatment_end_date < treatment_start_date
    ) {
      stop(
        "`treatment_end_date` cannot be before `treatment_start_date`.",
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

    animal <- .tm_animal_pk_in_experiment(
      con = con,
      experiment_id = experiment_id,
      animal_id = animal_id
    )

    group_pk <- .tm_group_pk(
      con = con,
      experiment_id = experiment_id,
      treatment_group = treatment_group
    )

    now <- .tm_now_utc()

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO animal_treatment_assignments (",
        "animal_pk,",
        "treatment_group_pk,",
        "assignment_date,",
        "treatment_start_date,",
        "treatment_end_date,",
        "notes,",
        "created_at_utc,",
        "updated_at_utc",
        ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        animal$animal_pk[[1L]],
        group_pk,
        assignment_date,
        treatment_start_date,
        treatment_end_date,
        .tm_text(notes, "notes"),
        now,
        now
      )
    )

    private$touch_project()

    invisible(self)
  },


  #' Update an Active-Experiment Treatment Assignment
  update_treatment_assignment = function(
    animal_id,
    treatment_group,
    treatment_start_date = NULL,
    treatment_end_date = NULL,
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

    assignment <- .tm_assignment_row(
      con = con,
      experiment_id = experiment_id,
      animal_id = animal_id,
      treatment_group = treatment_group
    )

    new_start_date <- if (is.null(treatment_start_date)) {
      assignment$treatment_start_date[[1L]]
    } else {
      .tm_treatment_date(
        treatment_start_date,
        "treatment_start_date"
      )
    }

    new_end_date <- if (is.null(treatment_end_date)) {
      assignment$treatment_end_date[[1L]]
    } else {
      .tm_optional_treatment_date(
        treatment_end_date,
        "treatment_end_date"
      )
    }

    if (
      !is.na(new_end_date) &&
        new_end_date < new_start_date
    ) {
      stop(
        "`treatment_end_date` cannot be before `treatment_start_date`.",
        call. = FALSE
      )
    }

    set_clauses <- character(0)
    params <- list()

    if (!is.null(treatment_start_date)) {
      set_clauses <- c(
        set_clauses,
        "treatment_start_date = ?"
      )

      params <- c(
        params,
        list(new_start_date)
      )
    }

    if (!is.null(treatment_end_date)) {
      set_clauses <- c(
        set_clauses,
        "treatment_end_date = ?"
      )

      params <- c(
        params,
        list(new_end_date)
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
        "Provide at least one value to update.",
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
        assignment$assignment_pk[[1L]]
      )
    )

    DBI::dbExecute(
      con,
      paste(
        "UPDATE animal_treatment_assignments",
        "SET",
        paste(set_clauses, collapse = ", "),
        "WHERE assignment_pk = ?"
      ),
      params = params
    )

    private$touch_project()

    invisible(self)
  },


  #' Return Dose Records in the Active Experiment
  doses = function(
    animal_id = NULL,
    treatment_group = NULL,
    drug = NULL,
    start_date = NULL,
    end_date = NULL,
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
      "  td.dose_pk,",
      "  tg.experiment_id,",
      "  td.animal_pk,",
      "  a.animal_id,",
      "  a.cage_id,",
      "  tg.group_name AS treatment_group,",
      "  tg.description AS treatment_description,",
      "  t.treatment_name,",
      "  d.drug,",
      "  dp.company_name,",
      "  dp.catalog_number,",
      "  dl.lot_number,",
      "  td.administration_date,",
      "  td.dose,",
      "  td.dose_unit,",
      "  td.route,",
      "  td.volume_ul,",
      "  td.notes,",
      "  td.created_at_utc,",
      "  td.updated_at_utc",
      "FROM treatment_doses AS td",
      "INNER JOIN animal_treatment_assignments AS ata",
      "  ON ata.assignment_pk = td.assignment_pk",
      "INNER JOIN treatment_groups AS tg",
      "  ON tg.treatment_group_pk = ata.treatment_group_pk",
      "INNER JOIN animals AS a",
      "  ON a.animal_pk = td.animal_pk",
      "INNER JOIN experiment_animals AS ea",
      "  ON ea.experiment_id = tg.experiment_id",
      "  AND ea.animal_pk = td.animal_pk",
      "INNER JOIN treatments AS t",
      "  ON t.treatment_pk = td.treatment_pk",
      "INNER JOIN drug_lots AS dl",
      "  ON dl.drug_lot_pk = td.drug_lot_pk",
      "INNER JOIN drug_products AS dp",
      "  ON dp.drug_product_pk = dl.drug_product_pk",
      "INNER JOIN drugs AS d",
      "  ON d.drug_pk = dp.drug_pk",
      "WHERE tg.experiment_id = ?"
    )

    params <- list(experiment_id)

    if (!is.null(animal_id)) {
      query <- paste(query, "AND trim(a.animal_id) = ?")
      params <- c(
        params,
        list(.tm_text(animal_id, "animal_id", required = TRUE))
      )
    }

    if (!is.null(treatment_group)) {
      query <- paste(query, "AND tg.group_name = ?")
      params <- c(
        params,
        list(.tm_text(
          treatment_group,
          "treatment_group",
          required = TRUE
        ))
      )
    }

    if (!is.null(drug)) {
      query <- paste(query, "AND d.drug = ?")
      params <- c(
        params,
        list(.tm_text(drug, "drug", required = TRUE))
      )
    }

    if (!is.null(start_date)) {
      query <- paste(
        query,
        "AND td.administration_date >= ?"
      )

      params <- c(
        params,
        list(.tm_treatment_date(start_date, "start_date"))
      )
    }

    if (!is.null(end_date)) {
      query <- paste(
        query,
        "AND td.administration_date <= ?"
      )

      params <- c(
        params,
        list(.tm_treatment_date(end_date, "end_date"))
      )
    }

    query <- paste(
      query,
      "ORDER BY",
      "td.administration_date,",
      "a.cage_id,",
      "a.animal_id,",
      "d.drug"
    )

    DBI::dbGetQuery(
      con,
      query,
      params = params
    )
  },


  #' Record an Actual Dose in the Active Experiment
  record_dose = function(
    animal_id,
    treatment_group,
    drug,
    company_name,
    catalog_number,
    lot_number,
    administration_date,
    dose,
    dose_unit,
    route = NULL,
    volume_ul = NULL,
    notes = NULL,
    user_id = .tm_default_user_id()
  ) {
    animal_id <- .tm_text(
      animal_id,
      "animal_id",
      required = TRUE
    )

    treatment_group <- .tm_text(
      treatment_group,
      "treatment_group",
      required = TRUE
    )

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

    lot_number <- .tm_text(
      lot_number,
      "lot_number",
      required = TRUE
    )

    administration_date <- .tm_treatment_date(
      administration_date,
      "administration_date"
    )

    dose <- .tm_optional_number(
      dose,
      "dose",
      positive = TRUE
    )

    if (is.na(dose)) {
      stop(
        "`dose` is required and must be positive.",
        call. = FALSE
      )
    }

    dose_unit <- .tm_text(
      dose_unit,
      "dose_unit",
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

    .tm_animal_alive_on_date(
      con = con,
      experiment_id = experiment_id,
      animal_id = animal_id,
      administration_date = administration_date
    )

    animal <- .tm_animal_pk_in_experiment(
      con = con,
      experiment_id = experiment_id,
      animal_id = animal_id
    )

    assignment <- .tm_assignment_row(
      con = con,
      experiment_id = experiment_id,
      animal_id = animal_id,
      treatment_group = treatment_group,
      administration_date = administration_date
    )

    regimen <- .tm_regimen_for_product_row(
      con = con,
      experiment_id = experiment_id,
      treatment_group = treatment_group,
      drug = drug,
      company_name = company_name,
      catalog_number = catalog_number
    )

    lot_pk <- .tm_lot_pk(
      con = con,
      drug = drug,
      company_name = company_name,
      catalog_number = catalog_number,
      lot_number = lot_number
    )

    existing <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT dose_pk",
        "FROM treatment_doses",
        "WHERE assignment_pk = ?",
        "  AND regimen_pk = ?",
        "  AND administration_date = ?"
      ),
      params = list(
        assignment$assignment_pk[[1L]],
        regimen$regimen_pk[[1L]],
        administration_date
      )
    )

    if (nrow(existing) > 0L) {
      stop(
        "A dose is already recorded for animal `",
        animal_id,
        "`, group `",
        treatment_group,
        "`, drug `",
        drug,
        "`, on ",
        administration_date,
        ".",
        call. = FALSE
      )
    }

    now <- .tm_now_utc()

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO treatment_doses (",
        "animal_pk,",
        "assignment_pk,",
        "regimen_pk,",
        "treatment_pk,",
        "drug_lot_pk,",
        "administration_date,",
        "dose,",
        "dose_unit,",
        "route,",
        "volume_ul,",
        "notes,",
        "created_at_utc,",
        "updated_at_utc",
        ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        animal$animal_pk[[1L]],
        assignment$assignment_pk[[1L]],
        regimen$regimen_pk[[1L]],
        regimen$treatment_pk[[1L]],
        lot_pk,
        administration_date,
        dose,
        dose_unit,
        .tm_text(route, "route"),
        .tm_optional_number(
          volume_ul,
          "volume_ul",
          positive = TRUE
        ),
        .tm_text(notes, "notes"),
        now,
        now
      )
    )

    private$touch_project()

    invisible(self)
  },


  #' Remove All Treatment Assignments in the Active Experiment
  #'
  #' This affects assignments associated with treatment groups belonging to
  #' the current active experiment only.
  remove_all_treatment_assignments = function(
    confirm = FALSE,
    user_id = .tm_default_user_id()
  ) {
    if (!isTRUE(confirm)) {
      stop(
        "This deletes all treatment assignments in the active experiment. ",
        "Use `confirm = TRUE` to proceed.",
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

    rows_deleted <- DBI::dbWithTransaction(
      con,
      {
        DBI::dbExecute(
          con,
          paste(
            "DELETE FROM animal_treatment_assignments",
            "WHERE treatment_group_pk IN (",
            "  SELECT treatment_group_pk",
            "  FROM treatment_groups",
            "  WHERE experiment_id = ?",
            ")"
          ),
          params = list(experiment_id)
        )
      }
    )

    private$touch_project()

    message(
      "Removed ",
      rows_deleted,
      " treatment assignment(s) from active experiment `",
      experiment_id,
      "`."
    )

    invisible(self)
  }
)
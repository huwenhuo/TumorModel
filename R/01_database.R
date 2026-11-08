# Database helpers for tumor-model study projects.
#
# These are internal package functions. They are intentionally not exported.
#
# Current migration stage:
# - Adds support for multiple experiments in a shared database.
# - Adds user-specific active experiments.
# - Preserves the existing animal/tumor/treatment schema.
# - Does NOT yet modify existing plotting, import, or analysis queries to
#   filter automatically by active experiment.
#
# Existing data relationships remain:
#
#   animals
#     -> tumors
#       -> tumor_measurements
#
#   animals
#     -> animal_treatment_assignments
#       -> treatment_doses
#
# New experiment membership relationship:
#
#   experiments
#     -> experiment_animals
#       -> animals
#
# This lets an animal be enrolled in a particular experiment while keeping
# the current legacy tables functional during migration.

.tm_now_utc <- function() {
  format(
    Sys.time(),
    tz = "UTC",
    usetz = TRUE
  )
}

.tm_default_user_id <- function() {
  user_id <- Sys.info()[["user"]]

  if (
    length(user_id) != 1L ||
      is.na(user_id) ||
      !nzchar(trimws(user_id))
  ) {
    user_id <- "unknown_user"
  }

  trimws(user_id)
}

.tm_connect <- function(db_path) {
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    db_path
  )

  DBI::dbExecute(
    con,
    "PRAGMA foreign_keys = ON"
  )

  # Wait briefly if another user/process is writing to the SQLite database.
  DBI::dbExecute(
    con,
    "PRAGMA busy_timeout = 5000"
  )

  con
}

.tm_table_exists <- function(con, table_name) {
  table_name %in% DBI::dbListTables(con)
}

.tm_table_columns <- function(con, table_name) {
  if (!.tm_table_exists(con, table_name)) {
    return(character(0))
  }

  DBI::dbGetQuery(
    con,
    paste0(
      "PRAGMA table_info(",
      DBI::dbQuoteIdentifier(con, table_name),
      ")"
    )
  )$name
}

.tm_require_columns <- function(
  con,
  table_name,
  required_columns
) {
  available_columns <- .tm_table_columns(
    con = con,
    table_name = table_name
  )

  missing_columns <- setdiff(
    required_columns,
    available_columns
  )

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Table `",
        table_name,
        "` is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# Backward-compatible name used by your existing treatment helper.
.tm_require_treatment_columns <- function(
  con,
  table_name,
  required_columns
) {
  .tm_require_columns(
    con = con,
    table_name = table_name,
    required_columns = required_columns
  )
}

.tm_initialize_schema <- function(con) {
  DBI::dbExecute(
    con,
    "PRAGMA foreign_keys = ON"
  )

  # ---------------------------------------------------------------------------
  # Users and experiments
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS users (",
      "user_id TEXT PRIMARY KEY,",
      "display_name TEXT,",
      "email TEXT,",
      "is_active INTEGER NOT NULL DEFAULT 1",
      "  CHECK (is_active IN (0, 1)),",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS experiments (",
      "experiment_id TEXT PRIMARY KEY,",
      "experiment_name TEXT NOT NULL,",
      "description TEXT,",
      "status TEXT NOT NULL DEFAULT 'active'",
      "  CHECK (status IN ('draft', 'active', 'closed', 'archived')),",
      "study_start_date TEXT,",
      "inoculation_date TEXT,",
      "created_by_user_id TEXT NOT NULL,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "closed_at_utc TEXT,",
      "FOREIGN KEY (created_by_user_id)",
      "  REFERENCES users(user_id)",
      "  ON DELETE RESTRICT",
      ")"
    )
  )

  # User-specific permission for each experiment.
  #
  # owner  = may manage experiment access and edit data
  # editor = may edit data
  # viewer = may view/query/plot data
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS user_experiment_roles (",
      "user_id TEXT NOT NULL,",
      "experiment_id TEXT NOT NULL,",
      "role_name TEXT NOT NULL DEFAULT 'editor'",
      "  CHECK (role_name IN ('owner', 'editor', 'viewer')),",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "PRIMARY KEY (user_id, experiment_id),",
      "FOREIGN KEY (user_id)",
      "  REFERENCES users(user_id)",
      "  ON DELETE CASCADE,",
      "FOREIGN KEY (experiment_id)",
      "  REFERENCES experiments(experiment_id)",
      "  ON DELETE CASCADE",
      ")"
    )
  )

  # User-specific application settings.
  #
  # Important setting:
  #
  #   setting_key   = "active_experiment_id"
  #   setting_value = "TM-2026-001"
  #
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS user_settings (",
      "user_id TEXT NOT NULL,",
      "setting_key TEXT NOT NULL,",
      "setting_value TEXT,",
      "updated_at_utc TEXT NOT NULL,",
      "PRIMARY KEY (user_id, setting_key),",
      "FOREIGN KEY (user_id)",
      "  REFERENCES users(user_id)",
      "  ON DELETE CASCADE",
      ")"
    )
  )

  # ---------------------------------------------------------------------------
  # Project / setup tables
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS drug_imports (",
      "drug_import_pk INTEGER PRIMARY KEY,",
      "source_database_path TEXT NOT NULL,",
      "source_project_title TEXT,",
      "imported_at_utc TEXT NOT NULL,",
      "imported_by TEXT,",
      "include_lots INTEGER NOT NULL,",
      "drugs_imported INTEGER NOT NULL DEFAULT 0,",
      "products_imported INTEGER NOT NULL DEFAULT 0,",
      "lots_imported INTEGER NOT NULL DEFAULT 0,",
      "notes TEXT",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS project (",
      "project_id INTEGER PRIMARY KEY,",
      "title TEXT NOT NULL,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL",
      ")"
    )
  )

  # Legacy/global model setup.
  #
  # This remains unchanged now so existing code continues working.
  # In a later migration, this will be replaced or supplemented by a
  # per-experiment setup table.
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS model_setup (",
      "setup_id INTEGER PRIMARY KEY CHECK (setup_id = 1),",
      "experiment_start_date TEXT,",
      "inoculation_date TEXT,",
      "treatment_start_date TEXT,",
      "cell_line TEXT,",
      "cells_per_animal REAL,",
      "inoculation_route TEXT,",
      "inoculation_site TEXT,",
      "inoculation_volume_ul REAL,",
      "matrix_vehicle TEXT,",
      "species TEXT,",
      "strain TEXT,",
      "sex TEXT,",
      "protocol_id TEXT,",
      "notes TEXT,",
      "updated_at_utc TEXT NOT NULL",
      ")"
    )
  )

  # ---------------------------------------------------------------------------
  # Animal / tumor tables
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS animals (",
      "animal_pk INTEGER PRIMARY KEY,",
      "cage_id TEXT,",
      "animal_id TEXT NOT NULL UNIQUE,",
      "species TEXT,",
      "strain TEXT,",
      "age_weeks REAL,",
      "sex TEXT,",
      "status TEXT NOT NULL DEFAULT 'Active',",
      "killed_date TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS tumors (",
      "tumor_pk INTEGER PRIMARY KEY,",
      "experiment_id TEXT NOT NULL,",
      "animal_pk INTEGER NOT NULL,",
      "tumor_label TEXT NOT NULL,",
      "site TEXT,",
      "status TEXT NOT NULL DEFAULT 'Active',",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (experiment_id)",
      "  REFERENCES experiments(experiment_id)",
      "  ON DELETE RESTRICT,",
      "FOREIGN KEY (animal_pk)",
      "  REFERENCES animals(animal_pk)",
      "  ON DELETE CASCADE,",
      "UNIQUE (experiment_id, animal_pk, tumor_label)",
      ")"
    )
  )
    
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS tumor_measurements (",
      "measurement_pk INTEGER PRIMARY KEY,",
      "tumor_pk INTEGER NOT NULL,",
      "measurement_date TEXT NOT NULL,",
      "length_mm REAL NOT NULL CHECK (length_mm > 0),",
      "width_mm REAL NOT NULL CHECK (width_mm > 0),",
      "depth_mm REAL CHECK (depth_mm > 0),",
      "volume_mm3 REAL NOT NULL CHECK (volume_mm3 >= 0),",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (tumor_pk)",
      "  REFERENCES tumors(tumor_pk)",
      "  ON DELETE CASCADE,",
      "UNIQUE (tumor_pk, measurement_date)",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS animal_vars (",
      "var_pk INTEGER PRIMARY KEY,",
      "experiment_id TEXT,",
      "animal_pk INTEGER NOT NULL,",
      "observation_date TEXT NOT NULL,",
      "variable_name TEXT NOT NULL,",
      "value_numeric REAL,",
      "value_text TEXT,",
      "unit TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (experiment_id)",
      "  REFERENCES experiments(experiment_id)",
      "  ON DELETE RESTRICT,",
      "FOREIGN KEY (animal_pk)",
      "  REFERENCES animals(animal_pk)",
      "  ON DELETE CASCADE,",
      "CHECK (",
      "  (value_numeric IS NOT NULL AND value_text IS NULL)",
      "  OR",
      "  (value_numeric IS NULL AND value_text IS NOT NULL)",
      ")",
      ")"
    )
  )

  # ---------------------------------------------------------------------------
  # Experiment enrollment
  #
  # `animals` is the master animal registry.
  #
  # `experiment_animals` records that a particular animal participates in
  # a particular experiment.
  #
  # This is deliberately compatible with your existing tables, which still
  # use animal_pk. Later, we will update queries and tables to use this
  # relationship as their experiment scope.
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS experiment_animals (",
      "experiment_animal_pk INTEGER PRIMARY KEY,",
      "experiment_id TEXT NOT NULL,",
      "animal_pk INTEGER NOT NULL,",
      "cage_id TEXT,",
      "status TEXT NOT NULL DEFAULT 'Active'",
      "  CHECK (",
      "    status IN ('Planned', 'Active', 'Removed', 'Killed', 'Excluded')",
      "  ),",
      "enrollment_date TEXT,",
      "removal_date TEXT,",
      "removal_reason TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (experiment_id)",
      "  REFERENCES experiments(experiment_id)",
      "  ON DELETE RESTRICT,",
      "FOREIGN KEY (animal_pk)",
      "  REFERENCES animals(animal_pk)",
      "  ON DELETE RESTRICT,",
      "UNIQUE (experiment_id, animal_pk)",
      ")"
    )
  )

  # ---------------------------------------------------------------------------
  # Drug / treatment-definition tables
  #
  # Drugs, products, lots, and treatment definitions remain globally shared.
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS drugs (",
      "drug_pk INTEGER PRIMARY KEY,",
      "drug_name TEXT NOT NULL,",
      "drug TEXT NOT NULL UNIQUE,",
      "drug_type TEXT NOT NULL,",
      "target TEXT,",
      "description TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS drug_products (",
      "drug_product_pk INTEGER PRIMARY KEY,",
      "drug_pk INTEGER NOT NULL,",
      "company_name TEXT NOT NULL,",
      "catalog_number TEXT NOT NULL,",
      "product_name TEXT,",
      "stock_concentration REAL,",
      "stock_concentration_unit TEXT,",
      "formulation TEXT,",
      "storage_conditions TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (drug_pk)",
      "  REFERENCES drugs(drug_pk),",
      "UNIQUE (drug_pk, company_name, catalog_number)",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS drug_lots (",
      "drug_lot_pk INTEGER PRIMARY KEY,",
      "drug_product_pk INTEGER NOT NULL,",
      "lot_number TEXT NOT NULL,",
      "received_date TEXT,",
      "expiration_date TEXT,",
      "reconstitution_date TEXT,",
      "reconstituted_concentration REAL,",
      "reconstituted_concentration_unit TEXT,",
      "storage_location TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (drug_product_pk)",
      "  REFERENCES drug_products(drug_product_pk),",
      "UNIQUE (drug_product_pk, lot_number)",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS treatments (",
      "treatment_pk INTEGER PRIMARY KEY,",
      "treatment_name TEXT NOT NULL UNIQUE,",
      "drug_product_pk INTEGER NOT NULL,",
      "treatment_dose REAL NOT NULL CHECK (treatment_dose > 0),",
      "dose_unit TEXT NOT NULL,",
      "route TEXT,",
      "component_volume_ul REAL CHECK (",
      "  component_volume_ul IS NULL OR component_volume_ul > 0",
      "),",
      "vehicle TEXT,",
      "final_injection_volume_ul REAL CHECK (",
      "  final_injection_volume_ul IS NULL",
      "  OR final_injection_volume_ul > 0",
      "),",
      "description TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (drug_product_pk)",
      "  REFERENCES drug_products(drug_product_pk)",
      ")"
    )
  )

  # ---------------------------------------------------------------------------
  # Legacy treatment groups and regimens
  #
  # This remains globally scoped temporarily so your existing treatment code
  # continues to work. The next major migration should rebuild this table with
  # experiment_id and scoped uniqueness:
  #
  # UNIQUE(experiment_id, group_name)
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS treatment_groups (",
      "treatment_group_pk INTEGER PRIMARY KEY,",
      "experiment_id TEXT NOT NULL,",
      "group_name TEXT NOT NULL,",
      "description TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (experiment_id)",
      "  REFERENCES experiments(experiment_id)",
      "  ON DELETE CASCADE,",
      "UNIQUE (experiment_id, group_name)",
      ")"
    )
  )
    
  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS treatment_group_regimens (",
      "regimen_pk INTEGER PRIMARY KEY,",
      "treatment_group_pk INTEGER NOT NULL,",
      "treatment_pk INTEGER NOT NULL,",
      "display_order INTEGER NOT NULL DEFAULT 1,",
      "schedule_description TEXT,",
      "start_day INTEGER,",
      "end_day INTEGER,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (treatment_group_pk)",
      "  REFERENCES treatment_groups(treatment_group_pk)",
      "  ON DELETE CASCADE,",
      "FOREIGN KEY (treatment_pk)",
      "  REFERENCES treatments(treatment_pk)",
      "  ON DELETE RESTRICT,",
      "UNIQUE (treatment_group_pk, treatment_pk)",
      ")"
    )
  )
    
  # ---------------------------------------------------------------------------
  # Animal group assignments and actual administered doses
  #
  # These remain on animal_pk during this first migration.
  # ---------------------------------------------------------------------------

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS animal_treatment_assignments (",
      "assignment_pk INTEGER PRIMARY KEY,",
      "animal_pk INTEGER NOT NULL,",
      "treatment_group_pk INTEGER NOT NULL,",
      "assignment_date TEXT NOT NULL,",
      "treatment_start_date TEXT NOT NULL,",
      "treatment_end_date TEXT,",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (animal_pk)",
      "  REFERENCES animals(animal_pk),",
      "FOREIGN KEY (treatment_group_pk)",
      "  REFERENCES treatment_groups(treatment_group_pk),",
      "CHECK (",
      "  treatment_end_date IS NULL",
      "  OR treatment_end_date >= treatment_start_date",
      ")",
      ")"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE TABLE IF NOT EXISTS treatment_doses (",
      "dose_pk INTEGER PRIMARY KEY,",
      "animal_pk INTEGER NOT NULL,",
      "assignment_pk INTEGER,",
      "regimen_pk INTEGER,",
      "treatment_pk INTEGER NOT NULL,",
      "drug_lot_pk INTEGER NOT NULL,",
      "administration_date TEXT NOT NULL,",
      "dose REAL NOT NULL CHECK (dose > 0),",
      "dose_unit TEXT NOT NULL,",
      "route TEXT,",
      "volume_ul REAL CHECK (volume_ul IS NULL OR volume_ul > 0),",
      "notes TEXT,",
      "created_at_utc TEXT NOT NULL,",
      "updated_at_utc TEXT NOT NULL,",
      "FOREIGN KEY (animal_pk)",
      "  REFERENCES animals(animal_pk),",
      "FOREIGN KEY (assignment_pk)",
      "  REFERENCES animal_treatment_assignments(assignment_pk),",
      "FOREIGN KEY (regimen_pk)",
      "  REFERENCES treatment_group_regimens(regimen_pk),",
      "FOREIGN KEY (treatment_pk)",
      "  REFERENCES treatments(treatment_pk),",
      "FOREIGN KEY (drug_lot_pk)",
      "  REFERENCES drug_lots(drug_lot_pk),",
      "CHECK (",
      "  (assignment_pk IS NULL AND regimen_pk IS NULL)",
      "  OR",
      "  (assignment_pk IS NOT NULL AND regimen_pk IS NOT NULL)",
      "),",
      "UNIQUE (animal_pk, treatment_pk, administration_date)",
      ")"
    )
  )

  # ---------------------------------------------------------------------------
  # Indexes
  # ---------------------------------------------------------------------------
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_treatment_groups_experiment",
      "ON treatment_groups(experiment_id, group_name)"
    )
  )
    
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_treatment_groups_experiment",
      "ON treatment_groups(experiment_id, group_name)"
    )
  )
    
  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_tumors_animal_pk",
      "ON tumors(animal_pk)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_tumor_measurements_tumor_pk",
      "ON tumor_measurements(tumor_pk)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_animal_vars_animal_date",
      "ON animal_vars(animal_pk, observation_date)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_animal_vars_variable",
      "ON animal_vars(variable_name)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_dose_date",
      "ON treatment_doses(administration_date)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_assignment_animal",
      "ON animal_treatment_assignments(animal_pk)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_regimen_group",
      "ON treatment_group_regimens(treatment_group_pk)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_experiment_animals_experiment",
      "ON experiment_animals(experiment_id)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_experiment_animals_animal",
      "ON experiment_animals(animal_pk)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_user_experiment_roles_experiment",
      "ON user_experiment_roles(experiment_id)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_animal_vars_experiment",
      "ON animal_vars(experiment_id)"
    )
  )

  DBI::dbExecute(
    con,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_tumors_experiment_animal",
      "ON tumors(experiment_id, animal_pk)"
    )
  )
    
  invisible(TRUE)
}

.tm_ensure_treatment_schema <- function(con) {
  DBI::dbExecute(
    con,
    "PRAGMA foreign_keys = ON"
  )

  .tm_initialize_schema(con)

  .tm_require_treatment_columns(
    con = con,
    table_name = "treatment_doses",
    required_columns = c(
      "dose_pk",
      "animal_pk",
      "assignment_pk",
      "regimen_pk",
      "drug_lot_pk",
      "administration_date",
      "dose",
      "dose_unit"
    )
  )

  .tm_require_treatment_columns(
    con = con,
    table_name = "treatment_group_regimens",
    required_columns = c(
      "regimen_pk",
      "treatment_group_pk",
      "treatment_pk"
    )
  )

  invisible(TRUE)
}

.tm_create_database <- function(
  db_path,
  title
) {
  con <- .tm_connect(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  .tm_initialize_schema(con)

  existing_project <- DBI::dbGetQuery(
    con,
    "SELECT COUNT(*) AS n FROM project"
  )$n[[1L]]

  if (existing_project == 0L) {
    now <- .tm_now_utc()

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO project",
        "(project_id, title, created_at_utc, updated_at_utc)",
        "VALUES (1, ?, ?, ?)"
      ),
      params = list(
        title,
        now,
        now
      )
    )
  }

  invisible(db_path)
}

# ---------------------------------------------------------------------------
# User helpers
# ---------------------------------------------------------------------------

.tm_ensure_user <- function(
  con,
  user_id = .tm_default_user_id(),
  display_name = user_id,
  email = NULL
) {
  if (
    length(user_id) != 1L ||
      is.na(user_id) ||
      !is.character(user_id) ||
      !nzchar(trimws(user_id))
  ) {
    stop(
      "`user_id` must be one non-empty character value.",
      call. = FALSE
    )
  }

  user_id <- trimws(user_id)

  # DBI parameter bindings require a length-one value.
  # NA_character_ is written to SQLite as SQL NULL.
  if (is.null(display_name)) {
    display_name <- NA_character_
  } else {
    display_name <- as.character(display_name)
  }

  if (is.null(email)) {
    email <- NA_character_
  } else {
    email <- as.character(email)
  }

  existing <- DBI::dbGetQuery(
    con,
    "SELECT user_id FROM users WHERE user_id = ?",
    params = list(user_id)
  )

  if (nrow(existing) == 0L) {
    now <- .tm_now_utc()

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO users (",
        "user_id, display_name, email, is_active,",
        "created_at_utc, updated_at_utc",
        ") VALUES (?, ?, ?, 1, ?, ?)"
      ),
      params = list(
        user_id,
        display_name,
        email,
        now,
        now
      )
    )
  }

  invisible(user_id)
}

.tm_user_experiment_role <- function(
  con,
  user_id,
  experiment_id
) {
  role <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT role_name",
      "FROM user_experiment_roles",
      "WHERE user_id = ?",
      "AND experiment_id = ?"
    ),
    params = list(
      user_id,
      experiment_id
    )
  )

  if (nrow(role) == 0L) {
    return(NA_character_)
  }

  role$role_name[[1L]]
}

.tm_user_can_access_experiment <- function(
  con,
  user_id,
  experiment_id,
  minimum_role = c("viewer", "editor", "owner")
) {
  minimum_role <- match.arg(minimum_role)

  current_role <- .tm_user_experiment_role(
    con = con,
    user_id = user_id,
    experiment_id = experiment_id
  )

  if (is.na(current_role)) {
    return(FALSE)
  }

  role_rank <- c(
    viewer = 1L,
    editor = 2L,
    owner = 3L
  )

  role_rank[[current_role]] >= role_rank[[minimum_role]]
}

# ---------------------------------------------------------------------------
# Experiment helpers
# ---------------------------------------------------------------------------

.tm_create_experiment <- function(
  con,
  experiment_id,
  experiment_name,
  description = NULL,
  study_start_date = NULL,
  inoculation_date = NULL,
  user_id = .tm_default_user_id()
) {
  if (
    length(experiment_id) != 1L ||
      is.na(experiment_id) ||
      !is.character(experiment_id) ||
      !nzchar(trimws(experiment_id))
  ) {
    stop(
      "`experiment_id` must be one non-empty character value.",
      call. = FALSE
    )
  }

  if (
    length(experiment_name) != 1L ||
      is.na(experiment_name) ||
      !is.character(experiment_name) ||
      !nzchar(trimws(experiment_name))
  ) {
    stop(
      "`experiment_name` must be one non-empty character value.",
      call. = FALSE
    )
  }

  experiment_id <- trimws(experiment_id)
  experiment_name <- trimws(experiment_name)

  # DBI uses NA_character_ to represent SQL NULL in bound parameters.
  if (is.null(description)) {
    description <- NA_character_
  } else {
    if (
      length(description) != 1L ||
      !is.character(description)
    ) {
      stop(
        "`description` must be NULL or one character value.",
        call. = FALSE
      )
    }

    description <- trimws(description)
  }
    
  if (!is.null(study_start_date)) {
    study_start_date <- as.Date(study_start_date)

    if (length(study_start_date) != 1L || is.na(study_start_date)) {
      stop(
        "`study_start_date` must be NULL or one valid date.",
        call. = FALSE
      )
    }

    study_start_date <- as.character(study_start_date)
  }

  if (!is.null(inoculation_date)) {
    inoculation_date <- as.Date(inoculation_date)

    if (length(inoculation_date) != 1L || is.na(inoculation_date)) {
      stop(
        "`inoculation_date` must be NULL or one valid date.",
        call. = FALSE
      )
    }

    inoculation_date <- as.character(inoculation_date)
  }

  .tm_ensure_user(
    con = con,
    user_id = user_id
  )

  now <- .tm_now_utc()

  DBI::dbWithTransaction(con, {
    existing <- DBI::dbGetQuery(
      con,
      "SELECT experiment_id FROM experiments WHERE experiment_id = ?",
      params = list(experiment_id)
    )

    if (nrow(existing) > 0L) {
      stop(
        "An experiment already exists with ID `",
        experiment_id,
        "`.",
        call. = FALSE
      )
    }

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO experiments (",
        "experiment_id, experiment_name, description, status,",
        "study_start_date, inoculation_date, created_by_user_id,",
        "created_at_utc, updated_at_utc",
        ") VALUES (?, ?, ?, 'active', ?, ?, ?, ?, ?)"
      ),
      params = list(
        experiment_id,
        experiment_name,
        description,
        study_start_date,
        inoculation_date,
        user_id,
        now,
        now
      )
    )

    DBI::dbExecute(
      con,
      paste(
        "INSERT INTO user_experiment_roles (",
        "user_id, experiment_id, role_name,",
        "created_at_utc, updated_at_utc",
        ") VALUES (?, ?, 'owner', ?, ?)"
      ),
      params = list(
        user_id,
        experiment_id,
        now,
        now
      )
    )
  })

  invisible(experiment_id)
}

.tm_grant_experiment_access <- function(
  con,
  experiment_id,
  user_id,
  role_name = c("viewer", "editor", "owner"),
  granted_by_user_id = .tm_default_user_id()
) {
  role_name <- match.arg(role_name)

  if (!.tm_user_can_access_experiment(
    con = con,
    user_id = granted_by_user_id,
    experiment_id = experiment_id,
    minimum_role = "owner"
  )) {
    stop(
      "Only an experiment owner can grant experiment access.",
      call. = FALSE
    )
  }

  .tm_ensure_user(
    con = con,
    user_id = user_id
  )

  now <- .tm_now_utc()

  DBI::dbExecute(
    con,
    paste(
      "INSERT INTO user_experiment_roles (",
      "user_id, experiment_id, role_name,",
      "created_at_utc, updated_at_utc",
      ") VALUES (?, ?, ?, ?, ?)",
      "ON CONFLICT(user_id, experiment_id) DO UPDATE SET",
      "role_name = excluded.role_name,",
      "updated_at_utc = excluded.updated_at_utc"
    ),
    params = list(
      user_id,
      experiment_id,
      role_name,
      now,
      now
    )
  )

  invisible(TRUE)
}

.tm_set_active_experiment_id <- function(
  con,
  experiment_id,
  user_id = .tm_default_user_id()
) {
  if (
    length(experiment_id) != 1L ||
      is.na(experiment_id) ||
      !is.character(experiment_id) ||
      !nzchar(trimws(experiment_id))
  ) {
    stop(
      "`experiment_id` must be one non-empty character value.",
      call. = FALSE
    )
  }

  experiment_id <- trimws(experiment_id)

  .tm_ensure_user(
    con = con,
    user_id = user_id
  )

  experiment <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT experiment_id, status",
      "FROM experiments",
      "WHERE experiment_id = ?"
    ),
    params = list(experiment_id)
  )

  if (nrow(experiment) != 1L) {
    stop(
      "No experiment exists with ID `",
      experiment_id,
      "`.",
      call. = FALSE
    )
  }

  if (experiment$status[[1L]] != "active") {
    stop(
      "Only an experiment with status `active` can be selected. ",
      "`",
      experiment_id,
      "` has status `",
      experiment$status[[1L]],
      "`.",
      call. = FALSE
    )
  }

  if (!.tm_user_can_access_experiment(
    con = con,
    user_id = user_id,
    experiment_id = experiment_id,
    minimum_role = "viewer"
  )) {
    stop(
      "User `",
      user_id,
      "` does not have access to experiment `",
      experiment_id,
      "`.",
      call. = FALSE
    )
  }

  DBI::dbExecute(
    con,
    paste(
      "INSERT INTO user_settings (",
      "user_id, setting_key, setting_value, updated_at_utc",
      ") VALUES (?, 'active_experiment_id', ?, ?)",
      "ON CONFLICT(user_id, setting_key) DO UPDATE SET",
      "setting_value = excluded.setting_value,",
      "updated_at_utc = excluded.updated_at_utc"
    ),
    params = list(
      user_id,
      experiment_id,
      .tm_now_utc()
    )
  )

  invisible(experiment_id)
}

.tm_get_active_experiment_id <- function(
  con,
  user_id = .tm_default_user_id(),
  minimum_role = c("viewer", "editor", "owner")
) {
  minimum_role <- match.arg(minimum_role)

  setting <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT setting_value",
      "FROM user_settings",
      "WHERE user_id = ?",
      "AND setting_key = 'active_experiment_id'"
    ),
    params = list(user_id)
  )

  if (
    nrow(setting) != 1L ||
      is.na(setting$setting_value[[1L]]) ||
      !nzchar(trimws(setting$setting_value[[1L]]))
  ) {
    stop(
      "No active experiment is configured for user `",
      user_id,
      "`. Call `set_active_experiment()` first.",
      call. = FALSE
    )
  }

  experiment_id <- trimws(setting$setting_value[[1L]])

  if (!.tm_user_can_access_experiment(
    con = con,
    user_id = user_id,
    experiment_id = experiment_id,
    minimum_role = minimum_role
  )) {
    stop(
      "User `",
      user_id,
      "` does not have sufficient access to active experiment `",
      experiment_id,
      "`.",
      call. = FALSE
    )
  }

  experiment_id
}

.tm_get_active_experiment <- function(
  con,
  user_id = .tm_default_user_id()
) {
  experiment_id <- .tm_get_active_experiment_id(
    con = con,
    user_id = user_id,
    minimum_role = "viewer"
  )

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "experiment_id,",
      "experiment_name,",
      "description,",
      "status,",
      "study_start_date,",
      "inoculation_date,",
      "created_by_user_id,",
      "created_at_utc,",
      "updated_at_utc,",
      "closed_at_utc",
      "FROM experiments",
      "WHERE experiment_id = ?"
    ),
    params = list(experiment_id)
  )
}

.tm_list_experiments <- function(
  con,
  user_id = .tm_default_user_id()
) {
  DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "e.experiment_id,",
      "e.experiment_name,",
      "e.description,",
      "e.status,",
      "e.study_start_date,",
      "e.inoculation_date,",
      "r.role_name,",
      "e.created_at_utc,",
      "e.updated_at_utc,",
      "e.closed_at_utc",
      "FROM experiments AS e",
      "INNER JOIN user_experiment_roles AS r",
      "ON r.experiment_id = e.experiment_id",
      "WHERE r.user_id = ?",
      "ORDER BY",
      "CASE e.status",
      "WHEN 'active' THEN 1",
      "WHEN 'draft' THEN 2",
      "WHEN 'closed' THEN 3",
      "WHEN 'archived' THEN 4",
      "ELSE 5 END,",
      "e.created_at_utc DESC"
    ),
    params = list(user_id)
  )
}

# ---------------------------------------------------------------------------
# Experiment enrollment helpers
# ---------------------------------------------------------------------------

.tm_enroll_animals_in_experiment <- function(
  con,
  experiment_id,
  animal_ids = NULL,
  user_id = .tm_default_user_id(),
  enrollment_date = Sys.Date(),
  replace_existing = FALSE
) {
  if (!.tm_user_can_access_experiment(
    con = con,
    user_id = user_id,
    experiment_id = experiment_id,
    minimum_role = "editor"
  )) {
    stop(
      "User `",
      user_id,
      "` does not have editor access to experiment `",
      experiment_id,
      "`.",
      call. = FALSE
    )
  }

  enrollment_date <- as.Date(enrollment_date)

  if (length(enrollment_date) != 1L || is.na(enrollment_date)) {
    stop(
      "`enrollment_date` must be one valid date.",
      call. = FALSE
    )
  }

  # NULL means enroll all currently registered animals.
  if (is.null(animal_ids)) {
    animals <- DBI::dbGetQuery(
      con,
      paste(
        "SELECT animal_pk, animal_id, cage_id, status, killed_date, notes",
        "FROM animals",
        "ORDER BY animal_id"
      )
    )
  } else {
    animal_ids <- unique(as.character(animal_ids))

    if (
      length(animal_ids) == 0L ||
      any(is.na(animal_ids)) ||
      any(!nzchar(trimws(animal_ids)))
    ) {
      stop(
        "`animal_ids` must be NULL or a non-empty character vector.",
        call. = FALSE
      )
    }

    placeholders <- paste(
      rep("?", length(animal_ids)),
      collapse = ", "
    )

    animals <- DBI::dbGetQuery(
      con,
      paste0(
        "SELECT animal_pk, animal_id, cage_id, status, killed_date, notes ",
        "FROM animals ",
        "WHERE animal_id IN (", placeholders, ") ",
        "ORDER BY animal_id"
      ),
      params = as.list(animal_ids)
    )

    found_ids <- animals$animal_id
    missing_ids <- setdiff(animal_ids, found_ids)

    if (length(missing_ids) > 0L) {
      stop(
        "These animal IDs do not exist in `animals`: ",
        paste(missing_ids, collapse = ", "),
        call. = FALSE
      )
    }
  }

  if (nrow(animals) == 0L) {
    stop(
      "No animals are available to enroll.",
      call. = FALSE
    )
  }

  now <- .tm_now_utc()
  enrollment_date <- as.character(enrollment_date)

  result <- DBI::dbWithTransaction(con, {
    enrolled <- 0L
    skipped <- 0L

    for (i in seq_len(nrow(animals))) {
      animal <- animals[i, , drop = FALSE]

      existing <- DBI::dbGetQuery(
        con,
        paste(
          "SELECT experiment_animal_pk",
          "FROM experiment_animals",
          "WHERE experiment_id = ?",
          "AND animal_pk = ?"
        ),
        params = list(
          experiment_id,
          animal$animal_pk[[1L]]
        )
      )

      if (nrow(existing) > 0L) {
        if (!isTRUE(replace_existing)) {
          skipped <- skipped + 1L
          next
        }

        DBI::dbExecute(
          con,
          paste(
            "UPDATE experiment_animals",
            "SET cage_id = ?,",
            "status = ?,",
            "enrollment_date = ?,",
            "updated_at_utc = ?",
            "WHERE experiment_id = ?",
            "AND animal_pk = ?"
          ),
          params = list(
            animal$cage_id[[1L]],
            animal$status[[1L]],
            enrollment_date,
            now,
            experiment_id,
            animal$animal_pk[[1L]]
          )
        )

        enrolled <- enrolled + 1L
        next
      }

      DBI::dbExecute(
        con,
        paste(
          "INSERT INTO experiment_animals (",
          "experiment_id, animal_pk, cage_id, status, enrollment_date,",
          "notes, created_at_utc, updated_at_utc",
          ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          experiment_id,
          animal$animal_pk[[1L]],
          animal$cage_id[[1L]],
          animal$status[[1L]],
          enrollment_date,
          animal$notes[[1L]],
          now,
          now
        )
      )

      enrolled <- enrolled + 1L
    }

    list(
      experiment_id = experiment_id,
      enrolled = enrolled,
      skipped_existing = skipped
    )
  })

  invisible(result)
}

.tm_list_experiment_animals <- function(
  con,
  experiment_id,
  user_id = .tm_default_user_id()
) {
  if (!.tm_user_can_access_experiment(
    con = con,
    user_id = user_id,
    experiment_id = experiment_id,
    minimum_role = "viewer"
  )) {
    stop(
      "User `",
      user_id,
      "` does not have access to experiment `",
      experiment_id,
      "`.",
      call. = FALSE
    )
  }

  DBI::dbGetQuery(
    con,
    paste(
      "SELECT",
      "ea.experiment_animal_pk,",
      "ea.experiment_id,",
      "a.animal_pk,",
      "a.animal_id,",
      "ea.cage_id,",
      "ea.status,",
      "ea.enrollment_date,",
      "ea.removal_date,",
      "ea.removal_reason,",
      "ea.notes",
      "FROM experiment_animals AS ea",
      "INNER JOIN animals AS a",
      "ON a.animal_pk = ea.animal_pk",
      "WHERE ea.experiment_id = ?",
      "ORDER BY a.animal_id"
    ),
    params = list(experiment_id)
  )
}

# ---------------------------------------------------------------------------
# Drug-information import
# ---------------------------------------------------------------------------

.tm_import_drug_information <- function(
  target_con,
  source_db,
  include_lots = TRUE,
  confirm = FALSE,
  imported_by = Sys.info()[["user"]],
  notes = NULL
) {
  if (
    !is.character(source_db) ||
      length(source_db) != 1L ||
      is.na(source_db) ||
      !nzchar(source_db)
  ) {
    stop(
      "`source_db` must be one non-empty database path.",
      call. = FALSE
    )
  }

  if (!file.exists(source_db)) {
    stop(
      "Source database does not exist: ",
      source_db,
      call. = FALSE
    )
  }

  source_con <- .tm_connect(source_db)
  on.exit(DBI::dbDisconnect(source_con), add = TRUE)

  required_tables <- c(
    "project",
    "drugs",
    "drug_products",
    "drug_lots"
  )

  source_tables <- DBI::dbListTables(source_con)

  missing_tables <- setdiff(
    required_tables,
    source_tables
  )

  if (length(missing_tables) > 0L) {
    stop(
      "The source database is missing required table(s): ",
      paste(missing_tables, collapse = ", "),
      call. = FALSE
    )
  }

  source_project <- DBI::dbGetQuery(
    source_con,
    "SELECT title FROM project WHERE project_id = 1"
  )

  source_project_title <- if (nrow(source_project) == 0L) {
    NA_character_
  } else {
    source_project$title[[1L]]
  }

  source_drugs <- DBI::dbGetQuery(
    source_con,
    paste(
      "SELECT",
      "drug_pk, drug_name, drug, drug_type, target,",
      "description, notes, created_at_utc, updated_at_utc",
      "FROM drugs",
      "ORDER BY drug_pk"
    )
  )

  source_products <- DBI::dbGetQuery(
    source_con,
    paste(
      "SELECT",
      "drug_product_pk, drug_pk, company_name, catalog_number,",
      "product_name, stock_concentration, stock_concentration_unit,",
      "formulation, storage_conditions, notes,",
      "created_at_utc, updated_at_utc",
      "FROM drug_products",
      "ORDER BY drug_product_pk"
    )
  )

  source_lots <- data.frame()

  if (isTRUE(include_lots)) {
    source_lots <- DBI::dbGetQuery(
      source_con,
      paste(
        "SELECT",
        "drug_lot_pk, drug_product_pk, lot_number, received_date,",
        "expiration_date, reconstitution_date,",
        "reconstituted_concentration,",
        "reconstituted_concentration_unit, storage_location, notes,",
        "created_at_utc, updated_at_utc",
        "FROM drug_lots",
        "ORDER BY drug_lot_pk"
      )
    )
  }

  preview <- list(
    source_database = normalizePath(
      source_db,
      winslash = "/",
      mustWork = TRUE
    ),
    source_project_title = source_project_title,
    include_lots = include_lots,
    drugs_found = nrow(source_drugs),
    products_found = nrow(source_products),
    lots_found = nrow(source_lots)
  )

  if (!isTRUE(confirm)) {
    message(
      "Drug import preview only; no changes were made.\n",
      "  Source project: ",
      ifelse(is.na(source_project_title), "<unknown>", source_project_title),
      "\n  Drugs found: ", nrow(source_drugs),
      "\n  Products found: ", nrow(source_products),
      "\n  Lots found: ", nrow(source_lots),
      "\n\nRe-run with `confirm = TRUE` to import."
    )

    return(invisible(preview))
  }

  result <- DBI::dbWithTransaction(target_con, {
    drug_map <- integer(0)
    product_map <- integer(0)

    drugs_imported <- 0L
    products_imported <- 0L
    lots_imported <- 0L

    # -------------------------------------------------------------------------
    # Drugs
    # -------------------------------------------------------------------------

    if (nrow(source_drugs) > 0L) {
      for (i in seq_len(nrow(source_drugs))) {
        source_row <- source_drugs[i, , drop = FALSE]

        existing <- DBI::dbGetQuery(
          target_con,
          "SELECT drug_pk FROM drugs WHERE drug = ?",
          params = list(source_row$drug[[1L]])
        )

        if (nrow(existing) > 0L) {
          target_drug_pk <- existing$drug_pk[[1L]]
        } else {
          DBI::dbExecute(
            target_con,
            paste(
              "INSERT INTO drugs (",
              "drug_name, drug, drug_type, target, description, notes,",
              "created_at_utc, updated_at_utc",
              ") VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
            ),
            params = list(
              source_row$drug_name[[1L]],
              source_row$drug[[1L]],
              source_row$drug_type[[1L]],
              source_row$target[[1L]],
              source_row$description[[1L]],
              source_row$notes[[1L]],
              .tm_now_utc(),
              .tm_now_utc()
            )
          )

          target_drug_pk <- DBI::dbGetQuery(
            target_con,
            "SELECT last_insert_rowid() AS id"
          )$id[[1L]]

          drugs_imported <- drugs_imported + 1L
        }

        drug_map[[as.character(
          source_row$drug_pk[[1L]]
        )]] <- target_drug_pk
      }
    }

    # -------------------------------------------------------------------------
    # Drug products
    # -------------------------------------------------------------------------

    if (nrow(source_products) > 0L) {
      for (i in seq_len(nrow(source_products))) {
        source_row <- source_products[i, , drop = FALSE]

        source_drug_pk <- as.character(
          source_row$drug_pk[[1L]]
        )

        target_drug_pk <- drug_map[[source_drug_pk]]

        if (is.null(target_drug_pk)) {
          stop(
            "Could not map source drug_pk ",
            source_drug_pk,
            " while importing drug products.",
            call. = FALSE
          )
        }

        existing <- DBI::dbGetQuery(
          target_con,
          paste(
            "SELECT drug_product_pk",
            "FROM drug_products",
            "WHERE drug_pk = ?",
            "AND company_name = ?",
            "AND catalog_number = ?"
          ),
          params = list(
            target_drug_pk,
            source_row$company_name[[1L]],
            source_row$catalog_number[[1L]]
          )
        )

        if (nrow(existing) > 0L) {
          target_product_pk <- existing$drug_product_pk[[1L]]
        } else {
          DBI::dbExecute(
            target_con,
            paste(
              "INSERT INTO drug_products (",
              "drug_pk, company_name, catalog_number, product_name,",
              "stock_concentration, stock_concentration_unit, formulation,",
              "storage_conditions, notes, created_at_utc, updated_at_utc",
              ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
            ),
            params = list(
              target_drug_pk,
              source_row$company_name[[1L]],
              source_row$catalog_number[[1L]],
              source_row$product_name[[1L]],
              source_row$stock_concentration[[1L]],
              source_row$stock_concentration_unit[[1L]],
              source_row$formulation[[1L]],
              source_row$storage_conditions[[1L]],
              source_row$notes[[1L]],
              .tm_now_utc(),
              .tm_now_utc()
            )
          )

          target_product_pk <- DBI::dbGetQuery(
            target_con,
            "SELECT last_insert_rowid() AS id"
          )$id[[1L]]

          products_imported <- products_imported + 1L
        }

        product_map[[as.character(
          source_row$drug_product_pk[[1L]]
        )]] <- target_product_pk
      }
    }

    # -------------------------------------------------------------------------
    # Drug lots
    # -------------------------------------------------------------------------

    if (isTRUE(include_lots) && nrow(source_lots) > 0L) {
      for (i in seq_len(nrow(source_lots))) {
        source_row <- source_lots[i, , drop = FALSE]

        source_product_pk <- as.character(
          source_row$drug_product_pk[[1L]]
        )

        target_product_pk <- product_map[[source_product_pk]]

        if (is.null(target_product_pk)) {
          stop(
            "Could not map source drug_product_pk ",
            source_product_pk,
            " while importing drug lots.",
            call. = FALSE
          )
        }

        existing <- DBI::dbGetQuery(
          target_con,
          paste(
            "SELECT drug_lot_pk",
            "FROM drug_lots",
            "WHERE drug_product_pk = ?",
            "AND lot_number = ?"
          ),
          params = list(
            target_product_pk,
            source_row$lot_number[[1L]]
          )
        )

        if (nrow(existing) == 0L) {
          DBI::dbExecute(
            target_con,
            paste(
              "INSERT INTO drug_lots (",
              "drug_product_pk, lot_number, received_date, expiration_date,",
              "reconstitution_date, reconstituted_concentration,",
              "reconstituted_concentration_unit, storage_location, notes,",
              "created_at_utc, updated_at_utc",
              ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
            ),
            params = list(
              target_product_pk,
              source_row$lot_number[[1L]],
              source_row$received_date[[1L]],
              source_row$expiration_date[[1L]],
              source_row$reconstitution_date[[1L]],
              source_row$reconstituted_concentration[[1L]],
              source_row$reconstituted_concentration_unit[[1L]],
              source_row$storage_location[[1L]],
              source_row$notes[[1L]],
              .tm_now_utc(),
              .tm_now_utc()
            )
          )

          lots_imported <- lots_imported + 1L
        }
      }
    }

    # -------------------------------------------------------------------------
    # Import provenance
    # -------------------------------------------------------------------------

    DBI::dbExecute(
      target_con,
      paste(
        "INSERT INTO drug_imports (",
        "source_database_path, source_project_title, imported_at_utc,",
        "imported_by, include_lots, drugs_imported, products_imported,",
        "lots_imported, notes",
        ") VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
      ),
      params = list(
        normalizePath(
          source_db,
          winslash = "/",
          mustWork = TRUE
        ),
        source_project_title,
        .tm_now_utc(),
        imported_by,
        as.integer(include_lots),
        drugs_imported,
        products_imported,
        lots_imported,
        notes
      )
    )

    list(
      source_database = normalizePath(
        source_db,
        winslash = "/",
        mustWork = TRUE
      ),
      source_project_title = source_project_title,
      drugs_imported = drugs_imported,
      products_imported = products_imported,
      lots_imported = lots_imported
    )
  })

  message(
    "Drug information imported successfully.\n",
    "  New drugs: ", result$drugs_imported,
    "\n  New products: ", result$products_imported,
    "\n  New lots: ", result$lots_imported
  )

  invisible(result)
}

# ---------------------------------------------------------------------------
# R6 public methods
#
# Your R6 class must include this list in its `public = c(...)` definition.
# ---------------------------------------------------------------------------

.tm_experiment_public_methods <- list(

  create_experiment = function(
    experiment_id,
    experiment_name,
    description = NULL,
    study_start_date = NULL,
    inoculation_date = NULL,
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_initialize_schema(con)

    .tm_create_experiment(
      con = con,
      experiment_id = experiment_id,
      experiment_name = experiment_name,
      description = description,
      study_start_date = study_start_date,
      inoculation_date = inoculation_date,
      user_id = user_id
    )
  },

  set_active_experiment = function(
    experiment_id,
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_initialize_schema(con)

    .tm_set_active_experiment_id(
      con = con,
      experiment_id = experiment_id,
      user_id = user_id
    )
  },

  active_experiment_id = function(
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_get_active_experiment_id(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )
  },

  get_active_experiment = function(
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_get_active_experiment(
      con = con,
      user_id = user_id
    )
  },

  list_experiments = function(
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_list_experiments(
      con = con,
      user_id = user_id
    )
  },

  grant_experiment_access = function(
    experiment_id,
    user_id,
    role_name = c("viewer", "editor", "owner"),
    granted_by_user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    .tm_grant_experiment_access(
      con = con,
      experiment_id = experiment_id,
      user_id = user_id,
      role_name = role_name,
      granted_by_user_id = granted_by_user_id
    )
  },

  enroll_animals_in_active_experiment = function(
    animal_ids = NULL,
    user_id = .tm_default_user_id(),
    enrollment_date = Sys.Date(),
    replace_existing = FALSE
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_get_active_experiment_id(
      con = con,
      user_id = user_id,
      minimum_role = "editor"
    )

    .tm_enroll_animals_in_experiment(
      con = con,
      experiment_id = experiment_id,
      animal_ids = animal_ids,
      user_id = user_id,
      enrollment_date = enrollment_date,
      replace_existing = replace_existing
    )
  },

  list_active_experiment_animals = function(
    user_id = .tm_default_user_id()
  ) {
    con <- private$connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)

    experiment_id <- .tm_get_active_experiment_id(
      con = con,
      user_id = user_id,
      minimum_role = "viewer"
    )

    .tm_list_experiment_animals(
      con = con,
      experiment_id = experiment_id,
      user_id = user_id
    )
  }
)

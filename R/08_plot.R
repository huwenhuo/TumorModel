# R/08_plot.R
#
# Tumor plotting methods for TumorModelProject.
#
# The plotting accessors are expected to be scoped to the active experiment:
#   study$tumor_measurements()
#   study$animals()
#   study$treatment_assignments()
#   study$treatment_groups()
#
# Day 0 = study_start_date, ordinarily the experiment inoculation date.


# =========================================================================
# INTERNAL HELPERS
# =========================================================================

.tm_plot_require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      paste0(
        "Package `ggplot2` is required for plotting. ",
        "Install it with install.packages('ggplot2')."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.tm_plot_require_columns <- function(data, columns, data_name) {
  missing_columns <- setdiff(columns, names(data))

  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        data_name,
        " is missing required column(s): ",
        paste(missing_columns, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.tm_plot_validate_study_start_date <- function(study_start_date) {
  study_start_date <- as.Date(study_start_date)

  if (length(study_start_date) != 1L || is.na(study_start_date)) {
    stop(
      "`study_start_date` must be one valid YYYY-MM-DD date.",
      call. = FALSE
    )
  }

  study_start_date
}


.tm_plot_validate_cap <- function(cap_volume_mm3) {
  if (is.null(cap_volume_mm3)) {
    return(NULL)
  }

  cap_volume_mm3 <- as.numeric(cap_volume_mm3)

  if (
    length(cap_volume_mm3) != 1L ||
      is.na(cap_volume_mm3) ||
      !is.finite(cap_volume_mm3) ||
      cap_volume_mm3 <= 0
  ) {
    stop(
      "`cap_volume_mm3` must be one positive finite number or NULL.",
      call. = FALSE
    )
  }

  cap_volume_mm3
}


# -------------------------------------------------------------------------
# Prepare one row per tumor measurement for plotting.
#
# Supports either:
#   measurement_date / volume_mm3
# or:
#   observation_date / source_volume_mm3
#
# This makes the plot code compatible with the Excel importer.
# -------------------------------------------------------------------------
.tm_plot_measurement_data <- function(
  project,
  study_start_date,
  treatment_order = NULL,
  exclude_animal_ids = NULL,
  cap_volume_mm3 = NULL
) {
  study_start_date <- .tm_plot_validate_study_start_date(
    study_start_date
  )

  cap_volume_mm3 <- .tm_plot_validate_cap(
    cap_volume_mm3
  )

  measurements <- as.data.frame(
    project$tumor_measurements(),
    stringsAsFactors = FALSE
  )

  animals <- as.data.frame(
    project$animals(),
    stringsAsFactors = FALSE
  )

  assignments <- as.data.frame(
    project$treatment_assignments(),
    stringsAsFactors = FALSE
  )

  treatment_groups <- as.data.frame(
    project$treatment_groups(),
    stringsAsFactors = FALSE
  )

  if (nrow(measurements) == 0L) {
    return(
      data.frame(
        animal_id = character(0),
        animal_pk = integer(0),
        tumor_label = character(0),
        measurement_date = as.Date(character(0)),
        day = numeric(0),
        tumor_volume_mm3 = numeric(0),
        plot_volume_mm3 = numeric(0),
        treatment_group = character(0),
        treatment = character(0),
        plot_series = character(0),
        stringsAsFactors = FALSE
      )
    )
  }

  .tm_plot_require_columns(
    measurements,
    "animal_pk",
    "study$tumor_measurements()"
  )

  .tm_plot_require_columns(
    animals,
    c("animal_pk", "animal_id"),
    "study$animals()"
  )

    # -----------------------------------------------------------------------
  # Attach central animal metadata to each tumor measurement.
  #
  # The tumor-measurement records contain animal_pk. Match that key to
  # study$animals() so plots can use animal_id, cage_id, species, sex, etc.
  # -----------------------------------------------------------------------

  animal_index <- match(
    measurements$animal_pk,
    animals$animal_pk
  )

  if (any(is.na(animal_index))) {
    stop(
      "One or more tumor measurements could not be matched to an animal.",
      call. = FALSE
    )
  }

  # Animal ID is required for plotting.
  measurements$animal_id <- as.character(
    animals$animal_id[animal_index]
  )

  if (
    any(
      is.na(measurements$animal_id) |
        !nzchar(trimws(measurements$animal_id))
    )
  ) {
    stop(
      "One or more tumor measurements could not be matched to an animal ID.",
      call. = FALSE
    )
  }

  # Optional central-animal fields used for faceting and reporting.
  # Create each field even if it does not yet exist in the animals table.
  for (column_name in c("cage_id", "species", "strain", "sex")) {
    if (column_name %in% names(animals)) {
      measurements[[column_name]] <- as.character(
        animals[[column_name]][animal_index]
      )
    } else {
      measurements[[column_name]] <- NA_character_
    }
  }

  if ("age_weeks" %in% names(animals)) {
    measurements$age_weeks <- suppressWarnings(
      as.numeric(animals$age_weeks[animal_index])
    )
  } else {
    measurements$age_weeks <- NA_real_
  }

  # -----------------------------------------------------------------------
  # Standardize measurement date.
  # -----------------------------------------------------------------------

  if ("measurement_date" %in% names(measurements)) {
    measurements$measurement_date <- as.Date(
      measurements$measurement_date
    )
  } else if ("observation_date" %in% names(measurements)) {
    measurements$measurement_date <- as.Date(
      measurements$observation_date
    )
  } else {
    stop(
      paste0(
        "study$tumor_measurements() must contain either ",
        "`measurement_date` or `observation_date`."
      ),
      call. = FALSE
    )
  }

  if (any(is.na(measurements$measurement_date))) {
    stop(
      "One or more tumor measurements has an invalid measurement date.",
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Standardize tumor volume.
  #
  # The importer calculates source_volume_mm3 as:
  #   length * width^2 / 2
  # -----------------------------------------------------------------------

  if ("volume_mm3" %in% names(measurements)) {
    measurements$tumor_volume_mm3 <- as.numeric(
      measurements$volume_mm3
    )
  } else if ("source_volume_mm3" %in% names(measurements)) {
    measurements$tumor_volume_mm3 <- as.numeric(
      measurements$source_volume_mm3
    )
  } else if ("tumor_volume_mm3" %in% names(measurements)) {
    measurements$tumor_volume_mm3 <- as.numeric(
      measurements$tumor_volume_mm3
    )
  } else {
    stop(
      paste0(
        "study$tumor_measurements() must contain one of: ",
        "`volume_mm3`, `source_volume_mm3`, or `tumor_volume_mm3`."
      ),
      call. = FALSE
    )
  }

  # -----------------------------------------------------------------------
  # Standardize tumor label.
  # -----------------------------------------------------------------------

  if (!"tumor_label" %in% names(measurements)) {
    measurements$tumor_label <- NA_character_
  }

  if (!"tumor_pk" %in% names(measurements)) {
    measurements$tumor_pk <- NA_integer_
  }

  # -----------------------------------------------------------------------
  # Attach treatment-group and readable treatment description.
  #
  # Animals without a treatment assignment are labeled "Untreated".
  # This is preferable to using a pseudo-treatment value such as NONE.
  # -----------------------------------------------------------------------

  measurements$treatment_group <- NA_character_
  measurements$treatment <- "Untreated"

  if (
    nrow(assignments) > 0L &&
      "animal_id" %in% names(assignments) &&
      "treatment_group" %in% names(assignments)
  ) {
    # If duplicate assignments exist, retain the latest one per animal.
    if (!"treatment_start_date" %in% names(assignments)) {
      assignments$treatment_start_date <- NA_character_
    }

    if (!"assignment_date" %in% names(assignments)) {
      assignments$assignment_date <- NA_character_
    }

    assignments$treatment_start_date <- as.Date(
      assignments$treatment_start_date
    )

    assignments$assignment_date <- as.Date(
      assignments$assignment_date
    )

    assignments$.plot_order <- seq_len(nrow(assignments))

    assignments <- assignments[
      order(
        assignments$animal_id,
        assignments$treatment_start_date,
        assignments$assignment_date,
        assignments$.plot_order,
        decreasing = TRUE,
        na.last = TRUE
      ),
      ,
      drop = FALSE
    ]

    assignments <- assignments[
      !duplicated(assignments$animal_id),
      ,
      drop = FALSE
    ]

    assignment_index <- match(
      as.character(measurements$animal_id),
      as.character(assignments$animal_id)
    )

    measurements$treatment_group <- as.character(
      assignments$treatment_group[assignment_index]
    )

    # Match the assignment group name to the treatment-group description.
    if (
      nrow(treatment_groups) > 0L &&
        all(c("group_name", "description") %in% names(treatment_groups))
    ) {
      treatment_index <- match(
        measurements$treatment_group,
        treatment_groups$group_name
      )

      measurements$treatment <- as.character(
        treatment_groups$description[treatment_index]
      )
    } else {
      measurements$treatment <- measurements$treatment_group
    }

    # Groups with missing descriptions use the group name as a fallback.
    missing_treatment <- is.na(measurements$treatment) |
      !nzchar(measurements$treatment)

    measurements$treatment[missing_treatment] <- measurements$treatment_group[
      missing_treatment
    ]

    # Animals that truly have no treatment assignment remain Untreated.
    missing_group <- is.na(measurements$treatment_group) |
      !nzchar(measurements$treatment_group)

    measurements$treatment[missing_group] <- "Untreated"
  }

  # -----------------------------------------------------------------------
  # Day 0 = inoculation date / supplied study_start_date.
  # -----------------------------------------------------------------------

  measurements$day <- as.numeric(
    measurements$measurement_date - study_start_date
  )

  measurements$plot_volume_mm3 <- measurements$tumor_volume_mm3

  if (!is.null(cap_volume_mm3)) {
    measurements$plot_volume_mm3 <- pmin(
      measurements$plot_volume_mm3,
      cap_volume_mm3
    )
  }

  # One visual line per animal/tumor trajectory.
  measurements$plot_series <- paste(
    measurements$animal_id,
    measurements$tumor_pk,
    measurements$tumor_label,
    sep = "__"
  )

  # Optional animal exclusions.
  if (!is.null(exclude_animal_ids)) {
    measurements <- measurements[
      !as.character(measurements$animal_id) %in%
        as.character(exclude_animal_ids),
      ,
      drop = FALSE
    ]
  }

  # Remove rows that cannot be plotted.
  measurements <- measurements[
    !is.na(measurements$plot_volume_mm3),
    ,
    drop = FALSE
  ]

  # Optional display order for treatment groups.
  if (!is.null(treatment_order)) {
    treatment_order <- as.character(treatment_order)

    measurements$treatment <- factor(
      measurements$treatment,
      levels = treatment_order
    )

    measurements <- measurements[
      !is.na(measurements$treatment),
      ,
      drop = FALSE
    ]
  }

  measurements <- measurements[
    order(
      measurements$treatment,
      measurements$animal_id,
      measurements$tumor_label,
      measurements$measurement_date
    ),
    ,
    drop = FALSE
  ]

  rownames(measurements) <- NULL

  measurements
}


# -------------------------------------------------------------------------
# Summarize tumor measurements by treatment and measurement day.
# -------------------------------------------------------------------------
.tm_plot_group_summary_data <- function(
  measurement_data,
  treatment_order = NULL
) {
  if (nrow(measurement_data) == 0L) {
    return(
      data.frame(
        treatment = character(0),
        day = numeric(0),
        n = integer(0),
        mean = numeric(0),
        median = numeric(0),
        sd = numeric(0),
        sem = numeric(0),
        stringsAsFactors = FALSE
      )
    )
  }

  split_index <- interaction(
    measurement_data$treatment,
    measurement_data$day,
    drop = TRUE,
    lex.order = TRUE
  )

  summary_list <- lapply(
    split(measurement_data, split_index),
    function(x) {
      values <- x$plot_volume_mm3
      values <- values[!is.na(values)]

      standard_deviation <- if (length(values) > 1L) {
        stats::sd(values)
      } else {
        0
      }

      data.frame(
        treatment = as.character(x$treatment[[1L]]),
        day = x$day[[1L]],
        n = length(values),
        mean = mean(values),
        median = stats::median(values),
        sd = standard_deviation,
        sem = standard_deviation / sqrt(length(values)),
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, summary_list)

  if (!is.null(treatment_order)) {
    out$treatment <- factor(
      out$treatment,
      levels = as.character(treatment_order)
    )
  }

  out <- out[
    order(out$treatment, out$day),
    ,
    drop = FALSE
  ]

  rownames(out) <- NULL

  out
}

.tm_plot_locf_data <- function(
  measurement_data,
  schedule_days = NULL,
  end_day = NULL,
  carry_forward_animal_ids = NULL
) {
  if (nrow(measurement_data) == 0L) {
    measurement_data$is_carried_forward <- logical(0)
    measurement_data$source_day <- numeric(0)
    return(measurement_data)
  }

  if (is.null(schedule_days)) {
    schedule_days <- sort(unique(measurement_data$day))
  } else {
    schedule_days <- sort(unique(as.numeric(schedule_days)))
  }

  if (
    length(schedule_days) == 0L ||
      any(is.na(schedule_days)) ||
      any(!is.finite(schedule_days))
  ) {
    stop(
      "`schedule_days` must be NULL or a non-empty vector of numeric study days.",
      call. = FALSE
    )
  }

  if (is.null(end_day)) {
    end_day <- max(schedule_days)
  }

  end_day <- as.numeric(end_day)

  if (
    length(end_day) != 1L ||
      is.na(end_day) ||
      !is.finite(end_day)
  ) {
    stop(
      "`end_day` must be one finite numeric study day.",
      call. = FALSE
    )
  }

  schedule_days <- schedule_days[
    schedule_days <= end_day
  ]

  if (length(schedule_days) == 0L) {
    stop(
      "No scheduled measurement days occur on or before `end_day`.",
      call. = FALSE
    )
  }

  # Default behavior: carry forward every animal's final observation.
  if (is.null(carry_forward_animal_ids)) {
    carry_forward_animal_ids <- unique(
      as.character(measurement_data$animal_id)
    )
  } else {
    carry_forward_animal_ids <- as.character(
      carry_forward_animal_ids
    )
  }

  locf_list <- lapply(
    split(measurement_data, measurement_data$plot_series),
    function(x) {
      x <- x[
        order(x$day, x$measurement_date),
        ,
        drop = FALSE
      ]

      # For duplicate values on one day, retain the final recorded value.
      x <- x[
        !duplicated(x$day, fromLast = TRUE),
        ,
        drop = FALSE
      ]

      x <- x[
        x$day <= end_day,
        ,
        drop = FALSE
      ]

      if (nrow(x) == 0L) {
        return(NULL)
      }

      animal_id <- as.character(x$animal_id[[1L]])

      # Never impute values before the tumor's first observation.
      target_days <- schedule_days[
        schedule_days >= min(x$day)
      ]

      # If this animal was not selected for LOCF, retain only observed data.
      if (!animal_id %in% carry_forward_animal_ids) {
        out <- x[
          x$day %in% target_days,
          ,
          drop = FALSE
        ]

        out$is_carried_forward <- FALSE
        out$source_day <- out$day

        return(out)
      }

      # For every target day, use the latest available actual measurement.
      source_index <- vapply(
        target_days,
        function(target_day) {
          eligible <- which(x$day <= target_day)

          if (length(eligible) == 0L) {
            return(NA_integer_)
          }

          utils::tail(eligible, 1L)
        },
        integer(1)
      )

      keep <- !is.na(source_index)

      target_days <- target_days[keep]
      source_index <- source_index[keep]

      out <- x[source_index, , drop = FALSE]

      out$source_day <- x$day[source_index]
      out$is_carried_forward <- target_days > out$source_day
      out$day <- target_days

      rownames(out) <- NULL
      out
    }
  )

  locf_list <- Filter(Negate(is.null), locf_list)

  if (length(locf_list) == 0L) {
    measurement_data[0, "is_carried_forward"] <- logical(0)
    measurement_data[0, "source_day"] <- numeric(0)
    return(measurement_data[0, , drop = FALSE])
  }

  out <- do.call(rbind, locf_list)

  out <- out[
    order(out$treatment, out$plot_series, out$day),
    ,
    drop = FALSE
  ]

  rownames(out) <- NULL

  out
}

# =========================================================================
# SURVIVAL / KAPLAN-MEIER HELPERS
# =========================================================================

.tm_survival_attach_treatment <- function(
  animals,
  assignments,
  treatment_groups
) {
  animals$treatment_group <- NA_character_
  animals$treatment <- "Untreated"

  if (
    nrow(assignments) == 0L ||
      !"animal_id" %in% names(assignments) ||
      !"treatment_group" %in% names(assignments)
  ) {
    return(animals)
  }

  # If an animal has more than one assignment, use the latest assignment.
  if (!"treatment_start_date" %in% names(assignments)) {
    assignments$treatment_start_date <- NA_character_
  }

  if (!"assignment_date" %in% names(assignments)) {
    assignments$assignment_date <- NA_character_
  }

  assignments$treatment_start_date <- suppressWarnings(
    as.Date(assignments$treatment_start_date)
  )

  assignments$assignment_date <- suppressWarnings(
    as.Date(assignments$assignment_date)
  )

  assignments$.survival_assignment_order <- seq_len(nrow(assignments))

  assignments <- assignments[
    order(
      assignments$animal_id,
      assignments$treatment_start_date,
      assignments$assignment_date,
      assignments$.survival_assignment_order,
      decreasing = TRUE,
      na.last = TRUE
    ),
    ,
    drop = FALSE
  ]

  assignments <- assignments[
    !duplicated(assignments$animal_id),
    ,
    drop = FALSE
  ]

  assignment_index <- match(
    as.character(animals$animal_id),
    as.character(assignments$animal_id)
  )

  animals$treatment_group <- as.character(
    assignments$treatment_group[assignment_index]
  )

  # Obtain human-readable treatment descriptions from group metadata.
  if (
    nrow(treatment_groups) > 0L &&
      all(c("group_name", "description") %in% names(treatment_groups))
  ) {
    group_index <- match(
      animals$treatment_group,
      treatment_groups$group_name
    )

    animals$treatment <- as.character(
      treatment_groups$description[group_index]
    )
  } else {
    animals$treatment <- animals$treatment_group
  }

  # If a treatment group has no description, display its group code.
  missing_treatment <- !is.na(animals$treatment_group) &
    (
      is.na(animals$treatment) |
        !nzchar(animals$treatment)
    )

  animals$treatment[missing_treatment] <- animals$treatment_group[
    missing_treatment
  ]

  # Animals that are not assigned to a real treatment remain Untreated.
  no_assignment <- is.na(animals$treatment_group) |
    !nzchar(animals$treatment_group)

  animals$treatment[no_assignment] <- "Untreated"

  animals
}


# -------------------------------------------------------------------------
# Prepare one row per animal for survival analysis.
#
# Event:
#   killed_date is present and occurs on/before censor_date.
#
# Censored:
#   no killed_date is recorded, or killed_date occurs after censor_date.
# -------------------------------------------------------------------------
.tm_data_survival <- function(
  project,
  study_start_date,
  censor_date,
  treatment_order = NULL,
  exclude_animal_ids = NULL
) {
  study_start_date <- .tm_plot_validate_study_start_date(
    study_start_date
  )

  censor_date <- .tm_plot_validate_study_start_date(
    censor_date
  )

  if (censor_date < study_start_date) {
    stop(
      "`censor_date` cannot occur before `study_start_date`.",
      call. = FALSE
    )
  }

  animals <- as.data.frame(
    project$animals(),
    stringsAsFactors = FALSE
  )

  assignments <- as.data.frame(
    project$treatment_assignments(),
    stringsAsFactors = FALSE
  )

  treatment_groups <- as.data.frame(
    project$treatment_groups(),
    stringsAsFactors = FALSE
  )

  .tm_plot_require_columns(
    animals,
    c("animal_pk", "animal_id", "killed_date"),
    "study$animals()"
  )

  # One row per active-experiment enrollment/animal.
  animals <- animals[
    !duplicated(animals$animal_pk),
    ,
    drop = FALSE
  ]

  animals$killed_date <- suppressWarnings(
    as.Date(animals$killed_date)
  )

  animals <- .tm_survival_attach_treatment(
    animals = animals,
    assignments = assignments,
    treatment_groups = treatment_groups
  )

  if (!is.null(exclude_animal_ids)) {
    animals <- animals[
      !as.character(animals$animal_id) %in%
        as.character(exclude_animal_ids),
      ,
      drop = FALSE
    ]
  }

  if (!is.null(treatment_order)) {
    treatment_order <- as.character(treatment_order)

    # Permit legacy NONE in an explicitly supplied ordering vector.
    treatment_order[
      toupper(treatment_order) == "NONE"
    ] <- "Untreated"

    treatment_order <- unique(treatment_order)

    animals$treatment <- factor(
      animals$treatment,
      levels = treatment_order
    )

    animals <- animals[
      !is.na(animals$treatment),
      ,
      drop = FALSE
    ]
  }

  if (nrow(animals) == 0L) {
    return(data.frame(
      animal_id = character(0),
      animal_pk = integer(0),
      treatment_group = character(0),
      treatment = character(0),
      killed_date = as.Date(character(0)),
      event_date = as.Date(character(0)),
      event = integer(0),
      survival_day = integer(0),
      status = character(0),
      stringsAsFactors = FALSE
    ))
  }

  # A death/euthanasia after the censor date is not yet an observed event.
  animals$event <- as.integer(
    !is.na(animals$killed_date) &
      animals$killed_date <= censor_date
  )

  animals$event_date <- censor_date

  animals$event_date[animals$event == 1L] <- animals$killed_date[
    animals$event == 1L
  ]

  animals$survival_day <- as.integer(
    animals$event_date - study_start_date
  )

  if (any(animals$survival_day < 0L, na.rm = TRUE)) {
    bad_ids <- animals$animal_id[
      animals$survival_day < 0L
    ]

    stop(
      paste0(
        "At least one killed date is earlier than `study_start_date`. ",
        "Affected animal ID(s): ",
        paste(bad_ids, collapse = ", "),
        "."
      ),
      call. = FALSE
    )
  }

  animals$status <- ifelse(
    animals$event == 1L,
    "Killed/euthanized",
    "Censored alive"
  )

  animals <- animals[
    order(
      animals$treatment,
      animals$survival_day,
      animals$animal_id
    ),
    ,
    drop = FALSE
  ]

  rownames(animals) <- NULL

  animals[
    ,
    c(
      "animal_id",
      "animal_pk",
      "treatment_group",
      "treatment",
      "killed_date",
      "event_date",
      "event",
      "survival_day",
      "status"
    ),
    drop = FALSE
  ]
}


# -------------------------------------------------------------------------
# Create Kaplan-Meier step-function data without requiring survival::survfit.
# -------------------------------------------------------------------------
.tm_data_kaplan_meier <- function(
  survival_data,
  censor_day
) {
  if (nrow(survival_data) == 0L) {
    return(data.frame(
      treatment = character(0),
      day = numeric(0),
      survival = numeric(0),
      n_risk = integer(0),
      n_event = integer(0),
      stringsAsFactors = FALSE
    ))
  }

  by_treatment <- split(
    survival_data,
    survival_data$treatment,
    drop = TRUE
  )

  km_list <- lapply(
    by_treatment,
    function(x) {
      x <- x[
        order(x$survival_day, x$event),
        ,
        drop = FALSE
      ]

      event_days <- sort(unique(
        x$survival_day[x$event == 1L]
      ))

      out <- data.frame(
        treatment = as.character(x$treatment[[1L]]),
        day = 0,
        survival = 1,
        n_risk = nrow(x),
        n_event = 0L,
        stringsAsFactors = FALSE
      )

      current_survival <- 1

      for (event_day in event_days) {
        n_risk <- sum(x$survival_day >= event_day)

        n_event <- sum(
          x$survival_day == event_day &
            x$event == 1L
        )

        current_survival <- current_survival * (
          1 - n_event / n_risk
        )

        out <- rbind(
          out,
          data.frame(
            treatment = as.character(x$treatment[[1L]]),
            day = event_day,
            survival = current_survival,
            n_risk = n_risk,
            n_event = n_event,
            stringsAsFactors = FALSE
          )
        )
      }

      # Extend the curve to the administrative censor date.
      if (utils::tail(out$day, 1L) < censor_day) {
        out <- rbind(
          out,
          data.frame(
            treatment = as.character(x$treatment[[1L]]),
            day = censor_day,
            survival = utils::tail(out$survival, 1L),
            n_risk = NA_integer_,
            n_event = 0L,
            stringsAsFactors = FALSE
          )
        )
      }

      out
    }
  )

  out <- do.call(rbind, km_list)
  rownames(out) <- NULL

  out
}


# -------------------------------------------------------------------------
# Draw Kaplan-Meier survival curves.
# -------------------------------------------------------------------------
.tm_plot_survival <- function(
  km_data,
  survival_data,
  censor_day,
  show_censor_marks = TRUE,
  show_end_labels = TRUE,
  end_label_offset_days = NULL
) {
  .tm_plot_require_ggplot2()

  if (nrow(survival_data) == 0L) {
    stop(
      "There are no animals available for the survival plot.",
      call. = FALSE
    )
  }

  if (is.null(end_label_offset_days)) {
    end_label_offset_days <- max(1, censor_day * 0.04)
  }

  end_label_offset_days <- as.numeric(end_label_offset_days)

  if (
    length(end_label_offset_days) != 1L ||
      is.na(end_label_offset_days) ||
      !is.finite(end_label_offset_days) ||
      end_label_offset_days < 0
  ) {
    stop(
      "`end_label_offset_days` must be one non-negative finite number.",
      call. = FALSE
    )
  }

  # Censor marks are placed at the survival probability immediately before
  # or at the administrative censor date.
  censor_data <- survival_data[
    survival_data$event == 0L,
    ,
    drop = FALSE
  ]

  if (nrow(censor_data) > 0L) {
    censor_data$survival <- vapply(
      seq_len(nrow(censor_data)),
      function(i) {
        curve <- km_data[
          as.character(km_data$treatment) ==
            as.character(censor_data$treatment[[i]]) &
            km_data$day <= censor_data$survival_day[[i]],
          ,
          drop = FALSE
        ]

        utils::tail(curve$survival, 1L)
      },
      numeric(1)
    )
  }

  p <- ggplot2::ggplot(
    km_data,
    ggplot2::aes(
      x = day,
      y = survival,
      color = treatment,
      group = treatment
    )
  ) +
    ggplot2::geom_step(
      direction = "hv",
      linewidth = 0.95,
      na.rm = TRUE
    ) +
    ggplot2::scale_y_continuous(
      name = "Survival probability",
      limits = c(0, 1),
      breaks = seq(0, 1, by = 0.25),
      labels = function(x) paste0(round(x * 100), "%"),
      expand = ggplot2::expansion(mult = c(0, 0.03))
    ) +
    ggplot2::labs(
      title = "Kaplan–Meier Time to Euthanasia by Treatment",
      subtitle = paste0(
        "Event = euthanasia/killing; + = right-censored at Day ",
        censor_day
      ),
      x = "Study day after inoculation",
      color = "Treatment"
    ) +
    ggplot2::theme_classic()

  if (isTRUE(show_censor_marks) && nrow(censor_data) > 0L) {
    p <- p +
      ggplot2::geom_point(
        data = censor_data,
        ggplot2::aes(
          x = survival_day,
          y = survival,
          color = treatment
        ),
        inherit.aes = FALSE,
        shape = 3,
        size = 3.2,
        stroke = 0.9,
        na.rm = TRUE
      )
  }

  if (isTRUE(show_end_labels)) {
    groups <- split(
      survival_data,
      as.character(survival_data$treatment),
      drop = TRUE
    )

    label_data <- do.call(
      rbind,
      lapply(
        groups,
        function(x) {
          treatment_name <- as.character(x$treatment[[1L]])

          curve <- km_data[
            as.character(km_data$treatment) == treatment_name,
            ,
            drop = FALSE
          ]

          curve <- curve[order(curve$day), , drop = FALSE]

          group_code <- unique(
            as.character(x$treatment_group[
              !is.na(x$treatment_group) &
                nzchar(x$treatment_group)
            ])
          )

          # Unassigned animals have no treatment group code.
          label <- if (length(group_code) == 1L) {
            group_code
          } else {
            treatment_name
          }

          data.frame(
            treatment = treatment_name,
            label = label,
            day = utils::tail(curve$day, 1L),
            survival = utils::tail(curve$survival, 1L),
            stringsAsFactors = FALSE
          )
        }
      )
    )

    label_data$label_day <- label_data$day + end_label_offset_days

    p <- p +
      ggplot2::geom_text(
        data = label_data,
        ggplot2::aes(
          x = label_day,
          y = survival,
          label = label,
          color = treatment
        ),
        inherit.aes = FALSE,
        hjust = 0,
        vjust = 0.5,
        fontface = "bold",
        size = 3.4,
        show.legend = FALSE,
        na.rm = TRUE
      )
  }

  axis_end <- censor_day

  if (isTRUE(show_end_labels)) {
    axis_end <- censor_day + end_label_offset_days * 4
  }

  axis_days <- sort(unique(c(
    0,
    km_data$day,
    censor_day
  )))

  p <- p +
    ggplot2::scale_x_continuous(
      limits = c(0, axis_end),
      breaks = axis_days,
      labels = paste0("Day ", axis_days),
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::coord_cartesian(
      clip = if (isTRUE(show_end_labels)) "off" else "on"
    ) +
    ggplot2::theme(
      legend.position = if (isTRUE(show_end_labels)) "none" else "right",
      plot.margin = ggplot2::margin(
        t = 10,
        r = if (isTRUE(show_end_labels)) 60 else 12,
        b = 10,
        l = 10
      )
    )

  attr(p, "km_data") <- km_data
  attr(p, "survival_data") <- survival_data

  p
}
        
# =========================================================================
# PUBLIC R6 METHODS
# =========================================================================

#' TumorModelProject plotting methods
#'
#' Internal R6 public-method list incorporated into `TumorModelProject` in
#' `R/90_tumormodel_project.R`. These methods are documented in the project
#' API and are not standalone exported functions.
#'
#' @noRd
.tm_plot_public_methods <- list(

  #' Return individual tumor-measurement data prepared for plotting
  data_tumor_plot = function(
    study_start_date,
    treatment_order = NULL,
    exclude_animal_ids = NULL,
    cap_volume_mm3 = NULL
  ) {
    .tm_plot_measurement_data(
      project = self,
      study_start_date = study_start_date,
      treatment_order = treatment_order,
      exclude_animal_ids = exclude_animal_ids,
      cap_volume_mm3 = cap_volume_mm3
    )
  },


  #' Plot Individual Tumor Growth Trajectories
  #'
  #' Plots each tumor trajectory from the active experiment. By default, tumor
  #' trajectories are faceted by treatment. Set `facet_by` to another column
  #' returned by data_tumor_plot(), such as `"animal_id"`, `"cage_id"`,
  #' `"sex"`, or `"species"`.
  #'
  #' @param study_start_date Study Day 0 date, in YYYY-MM-DD format.
  #' @param treatment_order Optional character vector defining treatment display
  #'   order.
  #' @param exclude_animal_ids Optional animal IDs to exclude from the plot.
  #' @param cap_volume_mm3 Optional tumor-volume cap used for display and shown
  #'   as a horizontal dashed reference line. Set to `NULL` to omit the line.
  #' @param ncol Number of columns in the facet layout.
  #' @param facet_by One column name from the tumor plot data by which to facet.
  #'   Defaults to `"treatment"`. Common choices include `"treatment"`,
  #'   `"animal_id"`, `"cage_id"`, `"sex"`, and `"species"`.
  #' @param show_points Logical; if `TRUE`, display individual measurements.
  #' @param show_animal_ids Logical; if `TRUE`, label the final measurement of
  #'   each tumor trajectory with its animal ID.
  #' @param free_y Logical; if `TRUE`, permit independent y-axis ranges across
  #'   facets. Defaults to `FALSE`.
  #'
  #' @return A ggplot object. The underlying plotting data are available in
  #'   `attr(plot, "plot_data")`.
  #'
  #' @examples
  #' \dontrun{
  #' study$plot_individual_tumors(
  #'   study_start_date = "2026-10-14",
  #'   facet_by = "treatment"
  #' )
  #'
  #' study$plot_individual_tumors(
  #'   study_start_date = "2026-10-14",
  #'   facet_by = "animal_id",
  #'   ncol = 5,
  #'   free_y = TRUE
  #' )
  #' }
  plot_individual_tumors = function(
    study_start_date,
    treatment_order = NULL,
    exclude_animal_ids = NULL,
    cap_volume_mm3 = 1000,
    ncol = 4,
    facet_by = "treatment",
    show_points = TRUE,
    show_animal_ids = TRUE,
    free_y = FALSE
  ) {
    .tm_plot_require_ggplot2()
  
    # -----------------------------------------------------------------------
    # Validate facet selection and facet layout.
    # -----------------------------------------------------------------------
    if (
      !is.character(facet_by) ||
        length(facet_by) != 1L ||
        is.na(facet_by) ||
        !nzchar(trimws(facet_by))
    ) {
      stop(
        "`facet_by` must be one non-empty column name.",
        call. = FALSE
      )
    }
  
    facet_by <- trimws(facet_by)
  
    if (
      length(ncol) != 1L ||
        is.na(ncol) ||
        !is.finite(ncol) ||
        ncol < 1 ||
        ncol != as.integer(ncol)
    ) {
      stop(
        "`ncol` must be one positive integer.",
        call. = FALSE
      )
    }
  
    ncol <- as.integer(ncol)
  
    if (!is.logical(free_y) || length(free_y) != 1L || is.na(free_y)) {
      stop(
        "`free_y` must be either TRUE or FALSE.",
        call. = FALSE
      )
    }
  
    if (
      !is.logical(show_points) ||
        length(show_points) != 1L ||
        is.na(show_points)
    ) {
      stop(
        "`show_points` must be either TRUE or FALSE.",
        call. = FALSE
      )
    }
  
    if (
      !is.logical(show_animal_ids) ||
        length(show_animal_ids) != 1L ||
        is.na(show_animal_ids)
    ) {
      stop(
        "`show_animal_ids` must be either TRUE or FALSE.",
        call. = FALSE
      )
    }
  
    # -----------------------------------------------------------------------
    # Obtain active-experiment tumor data.
    # -----------------------------------------------------------------------
    dat <- self$data_tumor_plot(
      study_start_date = study_start_date,
      treatment_order = treatment_order,
      exclude_animal_ids = exclude_animal_ids,
      cap_volume_mm3 = cap_volume_mm3
    )
  
    if (nrow(dat) == 0L) {
      stop(
        "There are no tumor measurements available after filtering.",
        call. = FALSE
      )
    }
  
    if (!facet_by %in% names(dat)) {
      stop(
        paste0(
          "`facet_by` must name a column in the tumor plot data. `",
          facet_by,
          "` is not available. Available columns: ",
          paste(names(dat), collapse = ", "),
          "."
        ),
        call. = FALSE
      )
    }
  
    # Prevent accidentally creating a single empty/NA facet.
    if (all(is.na(dat[[facet_by]]))) {
      stop(
        paste0(
          "`facet_by = \"", facet_by,
          "\"` cannot be used because that column contains only missing values."
        ),
        call. = FALSE
      )
    }
  
    facet_scales <- if (isTRUE(free_y)) "free_y" else "fixed"
  
    # Construct formula safely after confirming the requested field is an
    # actual column in `dat`.
    facet_formula <- stats::reformulate(
      response = NULL,
      termlabels = facet_by
    )
  
    # -----------------------------------------------------------------------
    # Base individual-trajectory plot.
    # -----------------------------------------------------------------------
    p <- ggplot2::ggplot(
      dat,
      ggplot2::aes(
        x = day,
        y = plot_volume_mm3,
        group = plot_series
      )
    ) +
      ggplot2::geom_line(
        color = "#466277",
        linewidth = 0.7,
        alpha = 0.80,
        na.rm = TRUE
      ) +
      ggplot2::facet_wrap(
        facets = facet_formula,
        ncol = ncol,
        scales = facet_scales
      ) +
      ggplot2::labs(
        title = "Individual Tumor Growth Curves",
        subtitle = paste0("Faceted by ", gsub("_", " ", facet_by)),
        x = "Study day after inoculation",
        y = expression("Tumor volume (mm"^3 * ")")
      ) +
      ggplot2::theme_classic()
  
    if (isTRUE(show_points)) {
      p <- p +
        ggplot2::geom_point(
          color = "#243B4A",
          size = 1.8,
          alpha = 0.85,
          na.rm = TRUE
        )
    }
  
    # Label the final observed measurement from each tumor trajectory.
    if (isTRUE(show_animal_ids)) {
      label_data <- dat[
        order(
          dat$plot_series,
          dat$day,
          dat$measurement_date
        ),
        ,
        drop = FALSE
      ]
  
      label_data <- label_data[
        !duplicated(label_data$plot_series, fromLast = TRUE),
        ,
        drop = FALSE
      ]
  
      p <- p +
        ggplot2::geom_text(
          data = label_data,
          ggplot2::aes(
            x = day,
            y = plot_volume_mm3,
            label = animal_id
          ),
          inherit.aes = FALSE,
          hjust = -0.12,
          vjust = 0.4,
          size = 3,
          color = "#243B4A",
          na.rm = TRUE
        )
    }
  
    if (!is.null(cap_volume_mm3)) {
      cap_volume_mm3 <- suppressWarnings(as.numeric(cap_volume_mm3))
  
      if (
        length(cap_volume_mm3) != 1L ||
          is.na(cap_volume_mm3) ||
          !is.finite(cap_volume_mm3) ||
          cap_volume_mm3 <= 0
      ) {
        stop(
          "`cap_volume_mm3` must be NULL or one positive finite number.",
          call. = FALSE
        )
      }
  
      p <- p +
        ggplot2::geom_hline(
          yintercept = cap_volume_mm3,
          linetype = "dashed",
          linewidth = 0.4,
          color = "grey50"
        )
    }
  
    # Use actual measurement days as x-axis ticks so labels align with
    # available measurement timepoints.
    measurement_days <- sort(unique(
      dat$day[!is.na(dat$day) & is.finite(dat$day)]
    ))
  
    p <- p +
      ggplot2::scale_x_continuous(
        name = "Study day after inoculation",
        breaks = measurement_days,
        labels = paste0("Day ", measurement_days),
        expand = ggplot2::expansion(
          mult = c(0.02, 0.10)
        )
      ) +
      ggplot2::coord_cartesian(
        clip = "off"
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(
          t = 10,
          r = 35,
          b = 10,
          l = 10
        )
      )
  
    attr(p, "plot_data") <- dat
    attr(p, "study_start_date") <- as.Date(study_start_date)
    attr(p, "facet_by") <- facet_by
  
    p
  },

  #' Plot Group-Level Tumor Growth Curves
  #'
  #' Summarizes tumor volumes in the active experiment by treatment and study
  #' day. The plot can display either the mean or median tumor volume, with
  #' optional SEM or SD error bars.
  #'
  #' @param study_start_date Study Day 0 date, in YYYY-MM-DD format.
  #' @param treatment_order Optional character vector defining treatment display
  #'   order.
  #' @param exclude_animal_ids Optional animal IDs to exclude from the plot.
  #' @param cap_volume_mm3 Optional tumor-volume cap used for display and shown
  #'   as a horizontal dashed reference line. Set to `NULL` to omit the line.
  #' @param summary Summary statistic: `"mean"` or `"median"`.
  #' @param error_bar Error-bar type: `"sem"`, `"sd"`, or `"none"`.
  #' @param show_individuals Logical; if `TRUE`, faint individual tumor
  #'   trajectories are drawn behind the group summary curves.
  #'
  #' @return A ggplot object. Group summary data are available in
  #'   `attr(plot, "plot_data")`; individual data are available in
  #'   `attr(plot, "individual_data")`.
  #' @examples
  #' \dontrun{
  #' study$plot_tumor_groups(
  #'   study_start_date = "2026-10-14",
  #'   summary = "mean",
  #'   error_bar = "sem"
  #' )
  #' }
  plot_tumor_groups = function(
    study_start_date,
    treatment_order = NULL,
    exclude_animal_ids = NULL,
    cap_volume_mm3 = 1000,
    summary = c("mean", "median"),
    error_bar = c("sem", "sd", "none"),
    show_individuals = FALSE
  ) {
    .tm_plot_require_ggplot2()

    summary <- match.arg(summary)
    error_bar <- match.arg(error_bar)

    individual_data <- self$data_tumor_plot(
      study_start_date = study_start_date,
      treatment_order = treatment_order,
      exclude_animal_ids = exclude_animal_ids,
      cap_volume_mm3 = cap_volume_mm3
    )

    if (nrow(individual_data) == 0L) {
      stop(
        "There are no tumor measurements available after filtering.",
        call. = FALSE
      )
    }

    summary_data <- .tm_plot_group_summary_data(
      measurement_data = individual_data,
      treatment_order = treatment_order
    )

    if (identical(summary, "mean")) {
      summary_data$center <- summary_data$mean
      subtitle_text <- "Mean tumor volume"
    } else {
      summary_data$center <- summary_data$median
      subtitle_text <- "Median tumor volume"
    }

    if (identical(error_bar, "sem")) {
      summary_data$error <- summary_data$sem
      subtitle_text <- paste0(subtitle_text, " ± SEM")
    } else if (identical(error_bar, "sd")) {
      summary_data$error <- summary_data$sd
      subtitle_text <- paste0(subtitle_text, " ± SD")
    } else {
      summary_data$error <- 0
    }

    p <- ggplot2::ggplot(
      summary_data,
      ggplot2::aes(
        x = day,
        y = center,
        color = treatment,
        group = treatment
      )
    )

    if (isTRUE(show_individuals)) {
      p <- p +
        ggplot2::geom_line(
          data = individual_data,
          ggplot2::aes(
            x = day,
            y = plot_volume_mm3,
            color = treatment,
            group = plot_series
          ),
          inherit.aes = FALSE,
          alpha = 0.18,
          linewidth = 0.4,
          na.rm = TRUE
        )
    }

    p <- p +
      ggplot2::geom_line(
        linewidth = 0.95,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        size = 2.2,
        na.rm = TRUE
      ) +
      ggplot2::labs(
        title = "Tumor Growth by Treatment",
        subtitle = subtitle_text,
        x = "Study day after inoculation",
        y = expression("Tumor volume (mm"^3 * ")"),
        color = "Treatment"
      ) +
      ggplot2::theme_classic()

    if (!identical(error_bar, "none")) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = pmax(0, center - error),
            ymax = center + error
          ),
          width = 0.25,
          linewidth = 0.5,
          na.rm = TRUE
        )
    }

    if (!is.null(cap_volume_mm3)) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = cap_volume_mm3,
          linetype = "dashed",
          linewidth = 0.4,
          color = "grey50"
        ) +
        ggplot2::coord_cartesian(
          ylim = c(0, cap_volume_mm3 * 1.05)
        )
    }

    attr(p, "plot_data") <- summary_data
    attr(p, "individual_data") <- individual_data
    attr(p, "study_start_date") <- as.Date(study_start_date)

    p
  },

  #' Plot Group-Level Tumor Growth Curves Using LOCF
  #'
  #' Creates a sensitivity plot using last observation carried forward (LOCF).
  #' For selected animals, the final observed tumor-volume measurement is
  #' carried forward to scheduled study days through `end_day`.
  #'
  #' This should generally be used as a supplementary sensitivity analysis,
  #' rather than as a replacement for observed tumor-volume data.
  #'
  #' @param study_start_date Study Day 0 date, in YYYY-MM-DD format.
  #' @param treatment_order Optional character vector defining treatment display
  #'   order.
  #' @param exclude_animal_ids Optional animal IDs to exclude from the plot.
  #' @param cap_volume_mm3 Optional tumor-volume cap used for display and shown
  #'   as a horizontal dashed reference line. Set to `NULL` to omit the line.
  #' @param summary Summary statistic: `"mean"` or `"median"`.
  #' @param error_bar Error-bar type: `"sem"`, `"sd"`, or `"none"`.
  #' @param show_individuals Logical; if `TRUE`, individual observed and LOCF
  #'   trajectories are shown behind the group summary curves.
  #' @param schedule_days Optional numeric vector of study days for LOCF.
  #'   Defaults to the observed measurement days.
  #' @param end_day Final study day for LOCF. Defaults to the latest scheduled
  #'   study day.
  #' @param carry_forward_animal_ids Optional animal IDs for which final
  #'   observed values should be carried forward. Defaults to all animals.
  #'
  #' @return A ggplot object. Group summary data are available in
  #'   `attr(plot, "plot_data")`; LOCF individual-level data are available in
  #'   `attr(plot, "individual_locf_data")`.
  #' @examples
  #' \dontrun{
  #' study$plot_tumor_groups_locf(
  #'   study_start_date = "2026-10-14",
  #'   schedule_days = c(0, 3, 7, 10, 14, 17, 21),
  #'   end_day = 21
  #' )
  #' }
  plot_tumor_groups_locf = function(
    study_start_date,
    treatment_order = NULL,
    exclude_animal_ids = NULL,
    cap_volume_mm3 = 1000,
    summary = c("mean", "median"),
    error_bar = c("sem", "sd", "none"),
    show_individuals = FALSE,
    schedule_days = NULL,
    end_day = NULL,
    carry_forward_animal_ids = NULL
  ) {
    .tm_plot_require_ggplot2()

    summary <- match.arg(summary)
    error_bar <- match.arg(error_bar)

    observed_data <- self$data_tumor_plot(
      study_start_date = study_start_date,
      treatment_order = treatment_order,
      exclude_animal_ids = exclude_animal_ids,
      cap_volume_mm3 = cap_volume_mm3
    )

    if (nrow(observed_data) == 0L) {
      stop(
        "There are no tumor measurements available after filtering.",
        call. = FALSE
      )
    }

    locf_data <- .tm_plot_locf_data(
      measurement_data = observed_data,
      schedule_days = schedule_days,
      end_day = end_day,
      carry_forward_animal_ids = carry_forward_animal_ids
    )

    summary_data <- .tm_plot_group_summary_data(
      measurement_data = locf_data,
      treatment_order = treatment_order
    )

    if (identical(summary, "mean")) {
      summary_data$center <- summary_data$mean
      summary_label <- "Mean"
    } else {
      summary_data$center <- summary_data$median
      summary_label <- "Median"
    }

    if (identical(error_bar, "sem")) {
      summary_data$error <- summary_data$sem
      error_label <- "SEM"
    } else if (identical(error_bar, "sd")) {
      summary_data$error <- summary_data$sd
      error_label <- "SD"
    } else {
      summary_data$error <- 0
      error_label <- NULL
    }

    p <- ggplot2::ggplot(
      summary_data,
      ggplot2::aes(
        x = day,
        y = center,
        color = treatment,
        group = treatment
      )
    )

    if (isTRUE(show_individuals)) {

      # A trajectory can contain both observed and LOCF values.
      # Split its drawing group by status because ggplot cannot draw a
      # single line that changes linetype partway through.
      locf_data$.line_segment <- interaction(
        locf_data$plot_series,
        locf_data$is_carried_forward,
        drop = TRUE
      )

      p <- p +
        ggplot2::geom_line(
          data = locf_data,
          ggplot2::aes(
            x = day,
            y = plot_volume_mm3,
            color = treatment,
            group = .line_segment,
            linetype = is_carried_forward
          ),
          inherit.aes = FALSE,
          alpha = 0.25,
          linewidth = 0.4,
          na.rm = TRUE
        ) +
        ggplot2::scale_linetype_manual(
          name = "Individual value",
          values = c(
            `FALSE` = "solid",
            `TRUE` = "dashed"
          ),
          labels = c(
            `FALSE` = "Observed",
            `TRUE` = "LOCF carried forward"
          )
        )
    }

    p <- p +
      ggplot2::geom_line(
        linewidth = 0.95,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(
        size = 2.2,
        na.rm = TRUE
      ) +
      ggplot2::labs(
        title = "Tumor Growth by Treatment (LOCF)",
        subtitle = paste0(
          summary_label,
          " tumor volume",
          if (!is.null(error_label)) {
            paste0(" ± ", error_label)
          } else {
            ""
          }
        ),
        caption = paste(
          "LOCF sensitivity plot:",
          "the last observed tumor volume is carried forward after the final measurement."
        ),
        x = "Study day after inoculation",
        y = expression("Tumor volume (mm"^3 * ")"),
        color = "Treatment"
      ) +
      ggplot2::theme_classic()

    if (!identical(error_bar, "none")) {
      p <- p +
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = pmax(0, center - error),
            ymax = center + error
          ),
          width = 0.25,
          linewidth = 0.5,
          na.rm = TRUE
        )
    }

    if (!is.null(cap_volume_mm3)) {
      p <- p +
        ggplot2::geom_hline(
          yintercept = cap_volume_mm3,
          linetype = "dashed",
          linewidth = 0.4,
          color = "grey50"
        ) +
        ggplot2::coord_cartesian(
          ylim = c(0, cap_volume_mm3 * 1.05)
        )
    }

    # Align axis tick marks with observed/scheduled LOCF data points.
    plot_days <- sort(unique(locf_data$day))

    p <- p +
      ggplot2::scale_x_continuous(
        breaks = plot_days,
        labels = paste0("Day ", plot_days),
        expand = ggplot2::expansion(mult = c(0.02, 0.03))
      )

    attr(p, "plot_data") <- summary_data
    attr(p, "individual_locf_data") <- locf_data
    attr(p, "study_start_date") <- as.Date(study_start_date)

    p
  }, 

  #' Return Active-Experiment Survival Data
  #'
  #' Creates one row per animal enrolled in the active experiment.
  #' Animals with a killed date on or before `censor_date` are events;
  #' all others are right-censored at `censor_date`.
  #'
  #' @param study_start_date Day 0 of the study, usually inoculation date.
  #' @param censor_date Final follow-up date for this analysis.
  #' @param treatment_order Optional treatment display/analysis order.
  #' @param exclude_animal_ids Optional animal IDs to omit entirely.
  #'
  #' @return A data frame with one row per animal and event/censor status.
  data_survival = function(
    study_start_date,
    censor_date,
    treatment_order = NULL,
    exclude_animal_ids = NULL
  ) {
    .tm_data_survival(
      project = self,
      study_start_date = study_start_date,
      censor_date = censor_date,
      treatment_order = treatment_order,
      exclude_animal_ids = exclude_animal_ids
    )
  },


  #' Plot Kaplan-Meier Time to Euthanasia by Treatment
  #'
  #' Generates a Kaplan-Meier plot for animals in the active experiment.
  #' A recorded killed date on or before `censor_date` is treated as an
  #' event. Animals alive at the censor date are right-censored and shown
  #' with plus-sign censor marks by default.
  #'
  #' @param study_start_date Day 0 of the study, usually inoculation date.
  #' @param censor_date Final follow-up date for the survival analysis.
  #' @param treatment_order Optional treatment display order.
  #' @param exclude_animal_ids Optional animal IDs to exclude entirely.
  #' @param show_censor_marks Show a plus sign for censored animals.
  #' @param show_end_labels Label each curve at its right-side endpoint.
  #' @param end_label_offset_days Horizontal spacing for endpoint labels.
  #'
  #' @return A ggplot object.
  plot_survival = function(
    study_start_date,
    censor_date,
    treatment_order = NULL,
    exclude_animal_ids = NULL,
    show_censor_marks = TRUE,
    show_end_labels = TRUE,
    end_label_offset_days = NULL
  ) {
    survival_data <- self$data_survival(
      study_start_date = study_start_date,
      censor_date = censor_date,
      treatment_order = treatment_order,
      exclude_animal_ids = exclude_animal_ids
    )

    if (nrow(survival_data) == 0L) {
      stop(
        "There are no animals available for the survival plot.",
        call. = FALSE
      )
    }

    censor_day <- as.integer(
      .tm_plot_validate_study_start_date(censor_date) -
        .tm_plot_validate_study_start_date(study_start_date)
    )

    km_data <- .tm_data_kaplan_meier(
      survival_data = survival_data,
      censor_day = censor_day
    )

    p <- .tm_plot_survival(
      km_data = km_data,
      survival_data = survival_data,
      censor_day = censor_day,
      show_censor_marks = show_censor_marks,
      show_end_labels = show_end_labels,
      end_label_offset_days = end_label_offset_days
    )

    attr(p, "study_start_date") <- as.Date(study_start_date)
    attr(p, "censor_date") <- as.Date(censor_date)

    p
  }

)
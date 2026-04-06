required_packages <- c("shiny", "bslib", "ggplot2", "DT", "readxl")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages. Run:\n",
      "install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      "))"
    ),
    call. = FALSE
  )
}

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(readxl)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

all_model_choices <- c(
  "4PL",
  "5PL",
  "3PL (Hill fixed = 1)",
  "3PL (Bottom = 0)",
  "3PL (Top = 100)"
)

sample_dataset <- function() {
  data.frame(
    compound = c(
      rep("Compound A", 24),
      rep("Compound B", 24)
    ),
    concentration = c(
      rep(c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30), each = 3),
      rep(c(0.01, 0.03, 0.1, 0.3, 1, 3, 10, 30), each = 3)
    ),
    response = c(
      99, 101, 98, 97, 95, 96, 90, 91, 88, 76, 73, 75,
      50, 48, 52, 22, 19, 20, 8, 7, 9, 6, 5, 6,
      101, 100, 99, 99, 98, 100, 96, 95, 97, 90, 88, 91,
      76, 74, 75, 47, 44, 46, 18, 19, 16, 9, 8, 10
    ),
    replicate = rep(c("R1", "R2", "R3"), 16),
    stringsAsFactors = FALSE
  )
}

read_input_data <- function(path, filename, sheet = NULL) {
  ext <- tolower(tools::file_ext(filename))

  if (ext %in% c("csv")) {
    return(utils::read.csv(path, check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (ext %in% c("tsv", "txt")) {
    return(utils::read.delim(path, check.names = FALSE, stringsAsFactors = FALSE))
  }

  if (ext %in% c("xls", "xlsx")) {
    return(as.data.frame(readxl::read_excel(path, sheet = sheet %||% 1)))
  }

  stop("Unsupported file type. Use CSV, TSV, TXT, XLS, or XLSX.")
}

available_sheets <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))
  if (ext %in% c("xls", "xlsx")) {
    return(readxl::excel_sheets(path))
  }
  character()
}

guess_column <- function(data, patterns, fallback = NULL) {
  nm <- names(data)
  if (!length(nm)) {
    return(fallback %||% "")
  }

  lower_nm <- tolower(nm)
  for (pattern in patterns) {
    hits <- grep(pattern, lower_nm, value = FALSE)
    if (length(hits) > 0) {
      return(nm[hits[1]])
    }
  }

  fallback %||% nm[1]
}

normalize_vector <- function(x, mode) {
  if (mode == "Raw values") {
    return(x)
  }

  x_min <- min(x, na.rm = TRUE)
  x_max <- max(x, na.rm = TRUE)
  span <- x_max - x_min

  if (!is.finite(span) || span <= 0) {
    return(rep(50, length(x)))
  }

  scaled <- (x - x_min) / span * 100

  if (mode == "Normalize 100 to 0 (max to min)") {
    return(100 - scaled)
  }

  scaled
}

transform_response_vector <- function(x, mode) {
  if (identical(mode, "As entered")) {
    return(x)
  }

  if (identical(mode, "Invert as 100 - response")) {
    return(100 - x)
  }

  if (identical(mode, "Mirror around min/max")) {
    x_range <- range(x, na.rm = TRUE)
    return(sum(x_range) - x)
  }

  x
}

parse_numeric_values <- function(text) {
  cleaned <- trimws(text %||% "")
  if (!nzchar(cleaned)) {
    return(NULL)
  }

  parts <- unlist(strsplit(cleaned, "[,;\\s]+"))
  parts <- parts[nzchar(parts)]
  values <- suppressWarnings(as.numeric(parts))

  if (!length(values) || any(!is.finite(values))) {
    return(NULL)
  }

  values
}

parse_axis_limits <- function(text) {
  values <- parse_numeric_values(text)
  if (is.null(values) || length(values) != 2 || values[1] >= values[2]) {
    return(NULL)
  }
  values
}

default_y_axis_settings <- function(prepared, fit_data, explicit_breaks, explicit_limits, normalization, response_transform) {
  finite_values <- c(
    prepared$raw$response,
    prepared$summary$response,
    fit_data$curves$response,
    fit_data$fitted$observed,
    fit_data$fitted$fitted
  )
  finite_values <- finite_values[is.finite(finite_values)]

  limits <- explicit_limits
  if (is.null(limits) && length(finite_values)) {
    looks_percent_like <- min(finite_values, na.rm = TRUE) >= -5 && max(finite_values, na.rm = TRUE) <= 105
    if (!identical(normalization, "Raw values") ||
        !identical(response_transform, "As entered") ||
        looks_percent_like) {
      limits <- c(0, 100)
    }
  }

  if (is.null(explicit_breaks)) {
    break_source <- if (!is.null(limits)) {
      limits
    } else if (length(finite_values)) {
      range(finite_values, na.rm = TRUE)
    } else {
      c(0, 100)
    }
    breaks <- sort(unique(c(pretty(break_source, n = 5), 50, 100)))
  } else {
    breaks <- explicit_breaks
  }

  if (!is.null(limits)) {
    breaks <- breaks[breaks >= limits[1] & breaks <= limits[2]]
  }

  list(
    breaks = breaks,
    limits = limits
  )
}

format_axis_labels <- function(x) {
  format(signif(x, 4), trim = TRUE, scientific = FALSE)
}

format_signif_text <- function(x, digits = 4) {
  if (!is.finite(x)) {
    return(NA_character_)
  }
  format(signif(x, digits), trim = TRUE, scientific = FALSE)
}

join_messages <- function(...) {
  parts <- c(...)
  parts <- parts[nzchar(parts) & !is.na(parts)]
  paste(parts, collapse = " | ")
}

compute_summary <- function(df) {
  mean_df <- aggregate(response ~ group + dose, df, mean)
  sd_df <- aggregate(
    response ~ group + dose,
    df,
    function(x) if (length(x) > 1) stats::sd(x) else NA_real_
  )
  n_df <- aggregate(response ~ group + dose, df, length)

  merged <- merge(mean_df, sd_df, by = c("group", "dose"), suffixes = c("", "_sd"))
  merged <- merge(merged, n_df, by = c("group", "dose"))
  names(merged) <- c("group", "dose", "response", "response_sd", "n")
  merged[order(merged$group, merged$dose), ]
}

group_diagnostics <- function(df) {
  split_groups <- split(df, df$group)
  out <- data.frame(
    group = names(split_groups),
    n_points = vapply(split_groups, nrow, integer(1)),
    distinct_doses = vapply(split_groups, function(piece) length(unique(piece$dose)), integer(1)),
    stringsAsFactors = FALSE
  )
  out$can_fit <- out$distinct_doses >= 4
  out[order(out$group), ]
}

format_problem_groups <- function(diagnostics_df, max_groups = 4) {
  if (!nrow(diagnostics_df)) {
    return("")
  }

  labels <- sprintf("%s (%s doses)", diagnostics_df$group, diagnostics_df$distinct_doses)
  if (length(labels) > max_groups) {
    labels <- c(labels[seq_len(max_groups)], sprintf("and %s more", length(diagnostics_df$group) - max_groups))
  }

  paste(labels, collapse = ", ")
}

prepare_dataset <- function(data, dose_col, response_col, group_col = NULL, normalization = "Raw values", response_transform = "As entered") {
  df <- data
  selected_group <- group_col

  df$dose <- suppressWarnings(as.numeric(df[[dose_col]]))
  df$response <- suppressWarnings(as.numeric(df[[response_col]]))
  df$group <- if (is.null(selected_group) || identical(selected_group, "None")) {
    "Series 1"
  } else {
    as.character(df[[selected_group]])
  }

  original_n <- nrow(df)
  df <- df[is.finite(df$dose) & is.finite(df$response) & nzchar(df$group), c("group", "dose", "response")]
  removed_non_numeric <- original_n - nrow(df)

  removed_non_positive <- sum(df$dose <= 0, na.rm = TRUE)
  df <- df[df$dose > 0, , drop = FALSE]

  if (nrow(df) == 0) {
    stop("No valid rows left after filtering. Dose values must be numeric and greater than zero.")
  }

  split_groups <- split(df, df$group)
  normalized_groups <- lapply(split_groups, function(piece) {
    piece$response <- normalize_vector(piece$response, normalization)
    piece$response <- transform_response_vector(piece$response, response_transform)
    piece
  })
  df <- do.call(rbind, normalized_groups)
  rownames(df) <- NULL

  summary_df <- compute_summary(df)
  diagnostics_df <- group_diagnostics(df)
  problem_groups <- diagnostics_df[!diagnostics_df$can_fit, , drop = FALSE]

  notes <- c(
    sprintf("Rows loaded: %s", original_n),
    sprintf("Removed non-numeric rows: %s", removed_non_numeric),
    sprintf("Removed rows with dose <= 0: %s", removed_non_positive),
    sprintf("Groups detected: %s", length(unique(df$group))),
    sprintf("Groups fit-ready (>=4 distinct doses): %s", sum(diagnostics_df$can_fit)),
    sprintf("Response transform: %s", response_transform)
  )

  if (nrow(problem_groups) > 0) {
    notes <- c(
      notes,
      paste0(
        "Groups that cannot be fit yet: ",
        format_problem_groups(problem_groups),
        ". If this looks wrong, change the Group/compound column to the compound or sample name, or set it to None."
      )
    )
  }

  list(
    raw = df[order(df$group, df$dose), ],
    summary = summary_df,
    diagnostics = diagnostics_df,
    notes = notes
  )
}

predict_curve <- function(dose, params, model, direction) {
  bottom <- params$bottom
  top <- params$top
  ic50_param <- params$ic50_param
  hill <- params$hill
  asymmetry <- params$asymmetry %||% 1

  ratio <- if (identical(direction, "Decreasing")) {
    (dose / ic50_param)^hill
  } else {
    (ic50_param / dose)^hill
  }

  denom <- 1 + ratio
  if (identical(model, "5PL")) {
    denom <- denom^asymmetry
  }

  bottom + (top - bottom) / denom
}

decode_parameters <- function(theta, model) {
  index <- 1

  if (identical(model, "4PL")) {
    bottom <- theta[index]
    index <- index + 1
    top <- bottom + exp(theta[index])
    index <- index + 1
    ic50_param <- exp(theta[index])
    index <- index + 1
    hill <- exp(theta[index])
    list(
      bottom = bottom,
      top = top,
      ic50_param = ic50_param,
      hill = hill,
      asymmetry = 1
    )
  } else if (identical(model, "5PL")) {
    bottom <- theta[index]
    index <- index + 1
    top <- bottom + exp(theta[index])
    index <- index + 1
    ic50_param <- exp(theta[index])
    index <- index + 1
    hill <- exp(theta[index])
    index <- index + 1
    asymmetry <- exp(theta[index])
    list(
      bottom = bottom,
      top = top,
      ic50_param = ic50_param,
      hill = hill,
      asymmetry = asymmetry
    )
  } else if (identical(model, "3PL (Hill fixed = 1)")) {
    bottom <- theta[index]
    index <- index + 1
    top <- bottom + exp(theta[index])
    index <- index + 1
    ic50_param <- exp(theta[index])
    list(
      bottom = bottom,
      top = top,
      ic50_param = ic50_param,
      hill = 1,
      asymmetry = 1
    )
  } else if (identical(model, "3PL (Bottom = 0)")) {
    top <- exp(theta[index])
    index <- index + 1
    ic50_param <- exp(theta[index])
    index <- index + 1
    hill <- exp(theta[index])
    list(
      bottom = 0,
      top = top,
      ic50_param = ic50_param,
      hill = hill,
      asymmetry = 1
    )
  } else if (identical(model, "3PL (Top = 100)")) {
    bottom <- theta[index]
    index <- index + 1
    ic50_param <- exp(theta[index])
    index <- index + 1
    hill <- exp(theta[index])
    list(
      bottom = bottom,
      top = 100,
      ic50_param = ic50_param,
      hill = hill,
      asymmetry = 1
    )
  } else {
    stop("Unknown model selected.")
  }
}

encode_parameters <- function(bottom, top, ic50_param, hill, asymmetry, model) {
  safe_bottom <- if (is.finite(bottom)) bottom else 0
  safe_top <- if (is.finite(top)) top else 100
  safe_ic50 <- max(ic50_param, .Machine$double.eps)
  safe_hill <- max(hill, 0.05)
  safe_asymmetry <- max(asymmetry, 0.2)

  if (identical(model, "4PL")) {
    return(c(
      safe_bottom,
      log(max(safe_top - safe_bottom, 1e-6)),
      log(safe_ic50),
      log(safe_hill)
    ))
  }

  if (identical(model, "5PL")) {
    return(c(
      safe_bottom,
      log(max(safe_top - safe_bottom, 1e-6)),
      log(safe_ic50),
      log(safe_hill),
      log(safe_asymmetry)
    ))
  }

  if (identical(model, "3PL (Hill fixed = 1)")) {
    return(c(
      safe_bottom,
      log(max(safe_top - safe_bottom, 1e-6)),
      log(safe_ic50)
    ))
  }

  if (identical(model, "3PL (Bottom = 0)")) {
    return(c(
      log(max(safe_top, 1e-6)),
      log(safe_ic50),
      log(safe_hill)
    ))
  }

  if (identical(model, "3PL (Top = 100)")) {
    capped_bottom <- min(safe_bottom, 99.999)
    return(c(
      capped_bottom,
      log(safe_ic50),
      log(safe_hill)
    ))
  }

  stop("Unknown model selected.")
}

compute_half_max_ic50 <- function(params, model, direction, observed_dose) {
  target_response <- params$bottom + 0.5 * (params$top - params$bottom)
  lower <- log10(max(min(observed_dose, na.rm = TRUE) / 1000, .Machine$double.eps))
  upper <- log10(max(observed_dose, na.rm = TRUE) * 1000)

  response_gap <- function(log_dose) {
    predict_curve(10^log_dose, params, model, direction) - target_response
  }

  grid <- seq(lower, upper, length.out = 500)
  values <- vapply(grid, response_gap, numeric(1))
  change_points <- which(diff(sign(values)) != 0)

  if (!length(change_points)) {
    return(NA_real_)
  }

  bracket <- c(grid[change_points[1]], grid[change_points[1] + 1])
  root <- try(stats::uniroot(response_gap, bracket)$root, silent = TRUE)

  if (inherits(root, "try-error")) {
    return(NA_real_)
  }

  10^root
}

empty_uncertainty <- function() {
  list(
    ic50_sd = NA_real_,
    ic50_sem = NA_real_,
    ic50_ci95_low = NA_real_,
    ic50_ci95_high = NA_real_,
    ic50_boot_n = 0
  )
}

bootstrap_group_data <- function(raw_group_df) {
  split_by_dose <- split(raw_group_df, raw_group_df$dose)
  sampled <- lapply(split_by_dose, function(piece) {
    idx <- sample.int(nrow(piece), size = nrow(piece), replace = TRUE)
    piece[idx, , drop = FALSE]
  })
  out <- do.call(rbind, sampled)
  rownames(out) <- NULL
  out
}

estimate_ic50_uncertainty <- function(raw_group_df, group_name, fit_to, model, direction, weighting, curve_points, bootstrap_iterations, progress_step = NULL) {
  if (bootstrap_iterations < 2 || length(unique(raw_group_df$dose)) < 4) {
    return(empty_uncertainty())
  }

  boot_ic50 <- rep(NA_real_, bootstrap_iterations)

  for (i in seq_len(bootstrap_iterations)) {
    sampled_raw <- bootstrap_group_data(raw_group_df)
    sampled_fit_df <- if (identical(fit_to, "Group means")) compute_summary(sampled_raw) else sampled_raw

    fit_attempt <- try(
      fit_single_group(
        df = sampled_fit_df,
        group_name = group_name,
        model = model,
        direction = direction,
        weighting = weighting,
        curve_points = curve_points
      ),
      silent = TRUE
    )

    if (!inherits(fit_attempt, "try-error")) {
      boot_ic50[i] <- fit_attempt$result_row$ic50[1]
    }

    if (!is.null(progress_step)) {
      progress_step(sprintf("Bootstrap IC50 uncertainty: %s (%s/%s)", group_name, i, bootstrap_iterations))
    }
  }

  boot_ic50 <- boot_ic50[is.finite(boot_ic50)]
  if (length(boot_ic50) < 2) {
    return(empty_uncertainty())
  }

  list(
    ic50_sd = stats::sd(boot_ic50),
    ic50_sem = stats::sd(boot_ic50) / sqrt(length(boot_ic50)),
    ic50_ci95_low = as.numeric(stats::quantile(boot_ic50, probs = 0.025, na.rm = TRUE, names = FALSE)),
    ic50_ci95_high = as.numeric(stats::quantile(boot_ic50, probs = 0.975, na.rm = TRUE, names = FALSE)),
    ic50_boot_n = length(boot_ic50)
  )
}

format_ic50_report <- function(ic50_value, uncertainty_method, uncertainty_values) {
  if (!is.finite(ic50_value)) {
    return(NA_character_)
  }

  center <- format_signif_text(ic50_value)

  if (identical(uncertainty_method, "None")) {
    return(center)
  }

  if (identical(uncertainty_method, "\u00b1 SD") && is.finite(uncertainty_values$ic50_sd)) {
    return(sprintf("%s \u00b1 %s", center, format_signif_text(uncertainty_values$ic50_sd, 3)))
  }

  if (identical(uncertainty_method, "\u00b1 SEM") && is.finite(uncertainty_values$ic50_sem)) {
    return(sprintf("%s \u00b1 %s", center, format_signif_text(uncertainty_values$ic50_sem, 3)))
  }

  if (identical(uncertainty_method, "95% CI") &&
      is.finite(uncertainty_values$ic50_ci95_low) &&
      is.finite(uncertainty_values$ic50_ci95_high)) {
    return(sprintf(
      "%s (95%% CI %s to %s)",
      center,
      format_signif_text(uncertainty_values$ic50_ci95_low, 4),
      format_signif_text(uncertainty_values$ic50_ci95_high, 4)
    ))
  }

  center
}

detect_direction_mismatch <- function(x, y, selected_direction) {
  log_x <- log10(x)
  corr <- suppressWarnings(stats::cor(log_x, y, method = "spearman", use = "complete.obs"))
  expected_direction <- if (is.finite(corr) && corr < 0) "Decreasing" else "Increasing"
  mismatch <- is.finite(corr) && !identical(expected_direction, selected_direction)

  list(
    spearman = corr,
    expected_direction = expected_direction,
    mismatch = mismatch
  )
}

assess_ic50_reliability <- function(df, params, ic50_value, direction) {
  observed_min <- min(df$response, na.rm = TRUE)
  observed_max <- max(df$response, na.rm = TRUE)
  min_dose <- min(df$dose, na.rm = TRUE)
  max_dose <- max(df$dose, na.rm = TRUE)
  crosses_50_observed <- observed_min <= 50 && observed_max >= 50

  interpretation <- if (is.finite(ic50_value)) format_signif_text(ic50_value) else NA_character_
  reliability <- "Reliable"
  warning_text <- ""

  if (!crosses_50_observed) {
    reliability <- "Observed range does not cross 50%"
    if (identical(direction, "Increasing") && observed_max < 50) {
      interpretation <- paste0("50% not reached (> ", format_signif_text(max_dose), ")")
      warning_text <- "Observed response never reaches 50%; IC50 is above the highest tested concentration."
    } else if (identical(direction, "Decreasing") && observed_min > 50) {
      interpretation <- paste0("50% not reached (< ", format_signif_text(min_dose), ")")
      warning_text <- "Observed response never drops to 50%; IC50 is below the lowest tested concentration."
    } else {
      warning_text <- "Observed data do not span the 50% response level."
    }
  }

  if (is.finite(ic50_value) && (ic50_value < min_dose || ic50_value > max_dose)) {
    if (identical(reliability, "Reliable")) {
      reliability <- "IC50 outside tested range"
      interpretation <- paste0("Extrapolated (", format_signif_text(ic50_value), ")")
    }
    warning_text <- join_messages(
      warning_text,
      "Fitted IC50 is outside the tested concentration range and should be treated as extrapolated."
    )
  }

  observed_span <- observed_max - observed_min
  fitted_span <- params$top - params$bottom
  if (!is.finite(fitted_span) || fitted_span <= 0) {
    reliability <- "Unreliable fit"
    warning_text <- join_messages(warning_text, "The fitted curve collapsed to a flat or invalid response range.")
  }

  if (is.finite(params$bottom) && params$bottom < observed_min - 25) {
    reliability <- "Unreliable fit"
    warning_text <- join_messages(warning_text, "The fitted bottom is far below the observed data.")
  }

  if (is.finite(params$top) && params$top > observed_max + max(25, 0.35 * max(observed_span, 1))) {
    reliability <- "Unreliable fit"
    warning_text <- join_messages(warning_text, "The fitted top is far above the observed data.")
  }

  if (identical(reliability, "Unreliable fit") && !grepl("not reached|Extrapolated", interpretation %||% "", ignore.case = TRUE)) {
    interpretation <- "Review fit before reporting"
  }

  list(
    observed_min_response = observed_min,
    observed_max_response = observed_max,
    min_tested_concentration = min_dose,
    max_tested_concentration = max_dose,
    crosses_50_observed = crosses_50_observed,
    reliability = reliability,
    warning_text = warning_text,
    interpretation = interpretation
  )
}

fit_single_group <- function(df, group_name, model, direction, weighting, curve_points) {
  if (identical(direction, "Auto-detect")) {
    direction_check <- detect_direction_mismatch(df$dose, df$response, "Increasing")
    candidate_directions <- unique(c(direction_check$expected_direction, "Increasing", "Decreasing"))
    fit_attempts <- lapply(candidate_directions, function(candidate_direction) {
      try(
        fit_single_group(df, group_name, model, candidate_direction, weighting, curve_points),
        silent = TRUE
      )
    })

    valid_attempts <- Filter(function(x) !inherits(x, "try-error"), fit_attempts)
    if (!length(valid_attempts)) {
      stop("Curve fit failed for this group.")
    }

    attempt_scores <- vapply(valid_attempts, function(x) {
      r_sq <- x$result_row$r_squared[1]
      if (is.finite(r_sq)) r_sq else -Inf
    }, numeric(1))
    best_fit <- valid_attempts[[which.max(attempt_scores)]]
    chosen_direction <- best_fit$result_row$direction[1]

    if (!identical(chosen_direction, direction_check$expected_direction)) {
      best_fit$result_row$fit_warning <- join_messages(
        best_fit$result_row$fit_warning[1],
        sprintf(
          "Auto-detect selected %s because it fit better than %s for this group.",
          chosen_direction,
          direction_check$expected_direction
        )
      )
    }

    return(best_fit)
  }

  x <- df$dose
  y <- df$response
  if (length(unique(x)) < 4) {
    stop(sprintf("Group '%s' has only %s distinct dose values; at least 4 are required.", group_name, length(unique(x))))
  }

  ordered <- order(x)
  edge_n <- max(1, floor(length(x) * 0.2))
  low_slice <- y[ordered][seq_len(edge_n)]
  high_slice <- y[ordered][(length(y) - edge_n + 1):length(y)]

  if (identical(direction, "Decreasing")) {
    start_top <- mean(low_slice, na.rm = TRUE)
    start_bottom <- mean(high_slice, na.rm = TRUE)
  } else {
    start_top <- mean(high_slice, na.rm = TRUE)
    start_bottom <- mean(low_slice, na.rm = TRUE)
  }

  if (!is.finite(start_top) || !is.finite(start_bottom) || start_top <= start_bottom) {
    start_bottom <- min(y, na.rm = TRUE)
    start_top <- max(y, na.rm = TRUE)
  }

  dose_candidates <- unique(as.numeric(stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)))
  hill_candidates <- if (identical(model, "3PL (Hill fixed = 1)")) 1 else c(0.5, 1, 1.5, 2)
  asymmetry_candidates <- if (identical(model, "5PL")) c(0.8, 1, 1.2) else 1

  weights <- rep(1, length(y))
  if (identical(weighting, "1 / SD^2 from means") && "response_sd" %in% names(df)) {
    sd_values <- df$response_sd
    weights <- ifelse(is.finite(sd_values) & sd_values > 0, 1 / (sd_values^2), 1)
  }

  objective <- function(theta) {
    params <- decode_parameters(theta, model)

    if (!is.finite(params$bottom) || !is.finite(params$top) || params$top <= params$bottom) {
      return(1e15)
    }

    predictions <- predict_curve(x, params, model, direction)
    if (any(!is.finite(predictions))) {
      return(1e15)
    }

    sum(weights * (y - predictions)^2)
  }

  best_fit <- NULL
  best_value <- Inf

  for (dose_start in dose_candidates) {
    for (hill_start in hill_candidates) {
      for (asymmetry_start in asymmetry_candidates) {
        theta0 <- encode_parameters(
          bottom = start_bottom,
          top = start_top,
          ic50_param = dose_start,
          hill = hill_start,
          asymmetry = asymmetry_start,
          model = model
        )

        fit_attempt <- try(
          stats::optim(
            par = theta0,
            fn = objective,
            method = "BFGS",
            control = list(maxit = 2000, reltol = 1e-10)
          ),
          silent = TRUE
        )

        if (!inherits(fit_attempt, "try-error") && is.finite(fit_attempt$value) && fit_attempt$value < best_value) {
          best_fit <- fit_attempt
          best_value <- fit_attempt$value
        }
      }
    }
  }

  if (is.null(best_fit)) {
    stop("Curve fit failed for this group.")
  }

  params <- decode_parameters(best_fit$par, model)
  fitted_values <- predict_curve(x, params, model, direction)
  grid <- exp(seq(log(min(x)), log(max(x)), length.out = curve_points))
  curve_values <- predict_curve(grid, params, model, direction)
  sse <- sum((y - fitted_values)^2)
  sst <- sum((y - mean(y))^2)
  r_squared <- if (sst > 0) 1 - sse / sst else NA_real_
  half_max_ic50 <- compute_half_max_ic50(params, model, direction, x)
  direction_check <- detect_direction_mismatch(x, y, direction)
  reliability <- assess_ic50_reliability(df, params, half_max_ic50, direction)
  fit_warning <- reliability$warning_text

  if (isTRUE(direction_check$mismatch)) {
    fit_warning <- join_messages(
      fit_warning,
      sprintf(
        "Observed trend looks %s (Spearman %.2f), but the selected direction is %s.",
        tolower(direction_check$expected_direction),
        direction_check$spearman,
        direction
      )
    )
  }

  list(
    result_row = data.frame(
      group = group_name,
      model = model,
      direction = direction,
      ic50 = half_max_ic50,
      midpoint_parameter = params$ic50_param,
      bottom = params$bottom,
      top = params$top,
      hill_slope = if (identical(direction, "Decreasing")) -params$hill else params$hill,
      asymmetry = params$asymmetry,
      r_squared = r_squared,
      n_points = nrow(df),
      observed_min_response = reliability$observed_min_response,
      observed_max_response = reliability$observed_max_response,
      min_tested_concentration = reliability$min_tested_concentration,
      max_tested_concentration = reliability$max_tested_concentration,
      crosses_50_observed = reliability$crosses_50_observed,
      direction_spearman = direction_check$spearman,
      suggested_direction = direction_check$expected_direction,
      fit_reliability = reliability$reliability,
      fit_warning = fit_warning,
      ic50_interpretation = reliability$interpretation,
      ic50_sd = NA_real_,
      ic50_sem = NA_real_,
      ic50_ci95_low = NA_real_,
      ic50_ci95_high = NA_real_,
      ic50_boot_n = 0,
      ic50_reported = NA_character_,
      fit_status = "OK",
      stringsAsFactors = FALSE
    ),
    curve = data.frame(
      group = group_name,
      dose = grid,
      response = curve_values,
      stringsAsFactors = FALSE
    ),
    fitted = data.frame(
      group = group_name,
      dose = x,
      observed = y,
      fitted = fitted_values,
      stringsAsFactors = FALSE
    )
  )
}

failed_fit_row <- function(group_name, model, direction, n_points, status_text) {
  data.frame(
    group = group_name,
    model = model,
    direction = direction,
    ic50 = NA_real_,
    midpoint_parameter = NA_real_,
    bottom = NA_real_,
    top = NA_real_,
    hill_slope = NA_real_,
    asymmetry = NA_real_,
    r_squared = NA_real_,
    n_points = n_points,
    observed_min_response = NA_real_,
    observed_max_response = NA_real_,
    min_tested_concentration = NA_real_,
    max_tested_concentration = NA_real_,
    crosses_50_observed = NA,
    direction_spearman = NA_real_,
    suggested_direction = NA_character_,
    fit_reliability = "Not fit",
    fit_warning = status_text,
    ic50_interpretation = NA_character_,
    ic50_sd = NA_real_,
    ic50_sem = NA_real_,
    ic50_ci95_low = NA_real_,
    ic50_ci95_high = NA_real_,
    ic50_boot_n = 0,
    ic50_reported = NA_character_,
    fit_status = status_text,
    stringsAsFactors = FALSE
  )
}

reporting_status_label <- function(fit_status, fit_reliability) {
  if (!identical(fit_status, "OK")) {
    return("No fit available")
  }

  if (identical(fit_reliability, "Reliable")) {
    return("Report numeric IC50")
  }

  if (identical(fit_reliability, "Observed range does not cross 50%")) {
    return("Do not report numeric IC50 (50% not reached)")
  }

  if (identical(fit_reliability, "IC50 outside tested range")) {
    return("Do not report numeric IC50 (extrapolated)")
  }

  "Review fit before reporting"
}

fit_dataset <- function(prepared, fit_to, model, direction, weighting, curve_points, uncertainty_method = "None", bootstrap_iterations = 200, progress_callback = NULL) {
  fit_source <- if (identical(fit_to, "Group means")) prepared$summary else prepared$raw
  split_groups <- split(fit_source, fit_source$group)
  diagnostics_df <- group_diagnostics(fit_source)
  fits <- vector("list", length(split_groups))
  names(fits) <- names(split_groups)
  total_steps <- length(split_groups) * (1 + if (identical(uncertainty_method, "None")) 0 else bootstrap_iterations)
  current_step <- 0

  step_progress <- function(detail_text) {
    current_step <<- current_step + 1
    if (!is.null(progress_callback)) {
      progress_callback(detail_text, current_step, max(total_steps, 1))
    }
  }

  for (group_name in names(split_groups)) {
    group_df <- split_groups[[group_name]]
    distinct_doses <- length(unique(group_df$dose))

    if (distinct_doses < 4) {
      fits[[group_name]] <- list(
        result_row = failed_fit_row(
          group_name = group_name,
          model = model,
          direction = direction,
          n_points = nrow(group_df),
          status_text = sprintf("Need at least 4 distinct doses (found %s)", distinct_doses)
        ),
        curve = data.frame(group = character(0), dose = numeric(0), response = numeric(0)),
        fitted = data.frame(group = character(0), dose = numeric(0), observed = numeric(0), fitted = numeric(0))
      )
      step_progress(sprintf("Skipping %s: not enough dose levels", group_name))
      next
    }

    fits[[group_name]] <- tryCatch(
      fit_single_group(
        df = group_df,
        group_name = group_name,
        model = model,
        direction = direction,
        weighting = weighting,
        curve_points = curve_points
      ),
      error = function(e) {
        list(
          result_row = failed_fit_row(
            group_name = group_name,
            model = model,
            direction = direction,
            n_points = nrow(group_df),
            status_text = conditionMessage(e)
          ),
          curve = data.frame(group = character(0), dose = numeric(0), response = numeric(0)),
          fitted = data.frame(group = character(0), dose = numeric(0), observed = numeric(0), fitted = numeric(0))
        )
      }
    )

    step_progress(sprintf("Curve fit completed for %s", group_name))

    if (fits[[group_name]]$result_row$fit_status[1] == "OK" && !identical(uncertainty_method, "None")) {
      raw_group_df <- prepared$raw[prepared$raw$group == group_name, , drop = FALSE]
      uncertainty_values <- estimate_ic50_uncertainty(
        raw_group_df = raw_group_df,
        group_name = group_name,
        fit_to = fit_to,
        model = model,
        direction = direction,
        weighting = weighting,
        curve_points = curve_points,
        bootstrap_iterations = bootstrap_iterations,
        progress_step = step_progress
      )

      fits[[group_name]]$result_row$ic50_sd <- uncertainty_values$ic50_sd
      fits[[group_name]]$result_row$ic50_sem <- uncertainty_values$ic50_sem
      fits[[group_name]]$result_row$ic50_ci95_low <- uncertainty_values$ic50_ci95_low
      fits[[group_name]]$result_row$ic50_ci95_high <- uncertainty_values$ic50_ci95_high
      fits[[group_name]]$result_row$ic50_boot_n <- uncertainty_values$ic50_boot_n
      fits[[group_name]]$result_row$ic50_reported <- format_ic50_report(
        ic50_value = fits[[group_name]]$result_row$ic50[1],
        uncertainty_method = uncertainty_method,
        uncertainty_values = uncertainty_values
      )
    } else if (fits[[group_name]]$result_row$fit_status[1] == "OK") {
      fits[[group_name]]$result_row$ic50_reported <- format_ic50_report(
        ic50_value = fits[[group_name]]$result_row$ic50[1],
        uncertainty_method = uncertainty_method,
        uncertainty_values = empty_uncertainty()
      )
    }

    if (fits[[group_name]]$result_row$fit_status[1] == "OK" &&
        fits[[group_name]]$result_row$fit_reliability[1] != "Reliable" &&
        nzchar(fits[[group_name]]$result_row$ic50_interpretation[1])) {
      fits[[group_name]]$result_row$ic50_reported <- fits[[group_name]]$result_row$ic50_interpretation[1]
    }
  }

  results_df <- do.call(rbind, lapply(fits, `[[`, "result_row"))
  curves_df <- do.call(rbind, lapply(fits, `[[`, "curve"))
  fitted_df <- do.call(rbind, lapply(fits, `[[`, "fitted"))
  results_df$reporting_status <- vapply(
    seq_len(nrow(results_df)),
    function(i) reporting_status_label(results_df$fit_status[i], results_df$fit_reliability[i]),
    character(1)
  )
  has_fit <- any(results_df$fit_status == "OK")
  invalid_groups <- diagnostics_df[!diagnostics_df$can_fit, , drop = FALSE]
  fit_messages <- character()

  if (nrow(invalid_groups) > 0) {
    fit_messages <- c(
      fit_messages,
      paste0(
        "Some groups were skipped because they do not have at least four distinct doses: ",
        format_problem_groups(invalid_groups),
        "."
      )
    )
  }

  if (!has_fit) {
    fit_messages <- c(
      fit_messages,
      "No IC50 curve could be fit with the current grouping. Try selecting the compound/sample column as Group, or set Group/compound to None if your file contains only one series."
    )
  }

  warning_mask <- results_df$fit_status == "OK" & !is.na(results_df$fit_warning) & nzchar(results_df$fit_warning)
  mismatch_groups <- results_df[warning_mask, , drop = FALSE]
  if (nrow(mismatch_groups) > 0) {
    fit_messages <- c(
      fit_messages,
      paste0(
        "Fit warnings detected for: ",
        paste(mismatch_groups$group, collapse = ", "),
        ". Check the fit_warning and fit_reliability columns in the results table."
      )
    )
  }

  if (!identical(uncertainty_method, "None")) {
    fit_messages <- c(
      fit_messages,
      paste0("IC50 uncertainty reported as ", uncertainty_method, " using bootstrap resampling (n = ", bootstrap_iterations, ").")
    )
  }

  list(
    results = results_df,
    curves = curves_df,
    fitted = fitted_df,
    has_fit = has_fit,
    diagnostics = diagnostics_df,
    message = paste(fit_messages, collapse = " ")
  )
}

model_parameter_count <- function(model) {
  switch(
    model,
    "4PL" = 4L,
    "5PL" = 5L,
    "3PL (Hill fixed = 1)" = 3L,
    "3PL (Bottom = 0)" = 3L,
    "3PL (Top = 100)" = 3L,
    NA_integer_
  )
}

compare_models <- function(prepared, fit_to, direction, weighting, curve_points, progress_callback = NULL) {
  model_fits <- vector("list", length(all_model_choices))
  names(model_fits) <- all_model_choices

  per_model_steps <- length(unique((if (identical(fit_to, "Group means")) prepared$summary else prepared$raw)$group))
  total_steps <- max(length(all_model_choices) * max(per_model_steps, 1), 1)

  for (i in seq_along(all_model_choices)) {
    model_name <- all_model_choices[i]
    model_fits[[model_name]] <- fit_dataset(
      prepared = prepared,
      fit_to = fit_to,
      model = model_name,
      direction = direction,
      weighting = weighting,
      curve_points = curve_points,
      uncertainty_method = "None",
      bootstrap_iterations = 20,
      progress_callback = function(detail_text, current_step, total_step_count) {
        if (!is.null(progress_callback)) {
          overall_step <- (i - 1) * max(total_step_count, 1) + current_step
          progress_callback(
            sprintf("Comparing models: %s | %s", model_name, detail_text),
            overall_step,
            total_steps
          )
        }
      }
    )
  }

  summary_df <- do.call(rbind, lapply(all_model_choices, function(model_name) {
    result_df <- model_fits[[model_name]]$results
    ok_mask <- result_df$fit_status == "OK"
    reliable_mask <- ok_mask & result_df$reporting_status == "Report numeric IC50"
    not_reached_mask <- ok_mask & result_df$reporting_status == "Do not report numeric IC50 (50% not reached)"
    extrapolated_mask <- ok_mask & result_df$reporting_status == "Do not report numeric IC50 (extrapolated)"
    review_mask <- ok_mask & result_df$reporting_status == "Review fit before reporting"
    r_sq_values <- result_df$r_squared[ok_mask & is.finite(result_df$r_squared)]

    data.frame(
      model = model_name,
      parameters = model_parameter_count(model_name),
      groups_fit = sum(ok_mask),
      numeric_ic50_reportable = sum(reliable_mask),
      not_reaching_50 = sum(not_reached_mask),
      extrapolated = sum(extrapolated_mask),
      review_before_reporting = sum(review_mask),
      median_r_squared = if (length(r_sq_values)) stats::median(r_sq_values) else NA_real_,
      mean_r_squared = if (length(r_sq_values)) mean(r_sq_values) else NA_real_,
      recommendation = "",
      stringsAsFactors = FALSE
    )
  }))

  rank_df <- summary_df
  rank_df$median_r_squared_rank <- ifelse(is.finite(rank_df$median_r_squared), rank_df$median_r_squared, -Inf)
  recommended_index <- order(
    -rank_df$numeric_ic50_reportable,
    -rank_df$groups_fit,
    rank_df$review_before_reporting,
    -rank_df$median_r_squared_rank,
    rank_df$parameters
  )[1]
  recommended_model <- summary_df$model[recommended_index]
  summary_df$recommendation[summary_df$model == recommended_model] <- "Suggested"
  summary_df <- summary_df[order(
    -summary_df$numeric_ic50_reportable,
    -summary_df$groups_fit,
    summary_df$review_before_reporting,
    -ifelse(is.finite(summary_df$median_r_squared), summary_df$median_r_squared, -Inf),
    summary_df$parameters
  ), , drop = FALSE]

  recommended_row <- summary_df[summary_df$model == recommended_model, , drop = FALSE]
  recommendation_text <- sprintf(
    "Suggested model: %s. This no-bootstrap comparison prioritizes reportable numeric IC50 values first, then successful fits, then median R-squared.",
    recommended_model
  )

  if (nrow(recommended_row) == 1) {
    recommendation_text <- sprintf(
      "%s It gave %s reportable numeric IC50 value(s) across %s fitted group(s), with median R-squared %s.",
      recommendation_text,
      recommended_row$numeric_ic50_reportable[1],
      recommended_row$groups_fit[1],
      format_signif_text(recommended_row$median_r_squared[1], 3)
    )
  }

  list(
    summary = summary_df,
    recommended_model = recommended_model,
    recommendation_text = recommendation_text,
    model_fits = model_fits
  )
}

equation_label <- function(model, direction) {
  shape <- switch(
    model,
    "4PL" = "bottom + (top - bottom) / (1 + ratio)",
    "5PL" = "bottom + (top - bottom) / (1 + ratio)^asymmetry",
    "3PL (Hill fixed = 1)" = "bottom + (top - bottom) / (1 + ratio), with Hill fixed to 1",
    "3PL (Bottom = 0)" = "0 + top / (1 + ratio)",
    "3PL (Top = 100)" = "bottom + (100 - bottom) / (1 + ratio)"
  )

  ratio_label <- if (identical(direction, "Auto-detect")) {
    "ratio uses the increasing or decreasing form chosen automatically for each group"
  } else if (identical(direction, "Decreasing")) {
    "ratio = (dose / midpoint)^Hill"
  } else {
    "ratio = (midpoint / dose)^Hill"
  }

  paste(shape, ratio_label)
}

direction_summary_label <- function(results_df, requested_direction) {
  if (!identical(requested_direction, "Auto-detect")) {
    return(requested_direction)
  }

  actual_directions <- unique(stats::na.omit(results_df$direction))
  if (!length(actual_directions)) {
    return("Auto-detect")
  }

  if (length(actual_directions) == 1) {
    return(paste("Auto ->", actual_directions))
  }

  paste0("Auto (mixed: ", paste(actual_directions, collapse = ", "), ")")
}

plot_palette <- function(n) {
  base <- c("#0f766e", "#b45309", "#be123c", "#1d4ed8", "#4d7c0f", "#7c3aed")
  rep(base, length.out = n)
}

publication_palette <- function(n, palette_name) {
  values <- switch(
    palette_name,
    "Bright contrast" = c("#1f3cff", "#ff2020", "#00c83c", "#c227ff", "#ff8c00", "#111111", "#996515", "#0f1e9c", "#7a0c64"),
    "Colorblind safe" = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
    "Nature muted" = c("#3c5488", "#00a087", "#e64b35", "#4dbbd5", "#f39b7f", "#8491b4", "#91d1c2", "#dc0000"),
    "Black and gray" = c("#000000", "#4d4d4d", "#6b6b6b", "#858585", "#a3a3a3", "#bdbdbd", "#d4d4d4"),
    "Earth tones" = c("#6c584c", "#a98467", "#adc178", "#4f772d", "#bc6c25", "#7f5539", "#386641", "#dda15e"),
    "Viridis" = grDevices::hcl.colors(max(n, 3), "viridis"),
    c("#0f766e", "#b45309", "#be123c", "#1d4ed8", "#4d7c0f", "#7c3aed")
  )
  rep(values, length.out = n)
}

publication_shapes <- function(n) {
  rep(c(16, 15, 17, 25, 18, 19, 0, 1, 2, 5), length.out = n)
}

resolve_plot_fill <- function(background_fill) {
  switch(
    background_fill,
    "Transparent" = "transparent",
    "Warm paper" = "#f7f4ef",
    "#ffffff"
  )
}

resolve_panel_fill <- function(background_fill) {
  switch(
    background_fill,
    "Transparent" = "transparent",
    "Warm paper" = "#fffdfa",
    "#ffffff"
  )
}

publication_theme <- function(style_name, base_size, legend_position, background_fill) {
  panel_fill <- resolve_panel_fill(background_fill)
  plot_fill <- resolve_plot_fill(background_fill)

  base_theme <- switch(
    style_name,
    "Minimal clean" = theme_minimal(base_size = base_size),
    "Classic axes" = theme_classic(base_size = base_size),
    "Publication boxed" = theme_classic(base_size = base_size),
    theme_bw(base_size = base_size)
  )

  themed <- base_theme +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 6, colour = "#111827"),
      plot.subtitle = element_text(colour = "#4b5563", size = base_size),
      legend.position = tolower(legend_position),
      legend.title = element_text(face = "bold"),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = panel_fill, colour = NA),
      plot.background = element_rect(fill = plot_fill, colour = NA),
      strip.background = element_rect(fill = panel_fill, colour = "#111111"),
      strip.text = element_text(face = "bold")
    )

  if (identical(style_name, "Publication boxed")) {
    themed <- themed +
      theme(
        panel.border = element_rect(colour = "#111111", fill = NA, size = 1),
        axis.line = element_line(colour = "#111111", size = 0.8),
        axis.ticks = element_line(colour = "#111111", size = 0.8),
        axis.ticks.length = grid::unit(0.22, "cm")
      )
  } else if (identical(style_name, "Classic axes")) {
    themed <- themed +
      theme(
        axis.line = element_line(colour = "#111111", size = 0.8),
        axis.ticks = element_line(colour = "#111111", size = 0.8)
      )
  } else if (identical(style_name, "Framed clean")) {
    themed <- themed +
      theme(
        panel.border = element_rect(colour = "#111111", fill = NA, size = 0.9),
        panel.grid.major = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "#111111", size = 0.8)
      )
  } else {
    themed <- themed +
      theme(
        panel.grid.major.x = element_line(colour = "#e5e7eb"),
        panel.grid.major.y = element_line(colour = "#e5e7eb")
      )
  }

  if (identical(tolower(legend_position), "none")) {
    themed <- themed + theme(legend.position = "none")
  }

  themed
}

plot_export_device <- function(format_name) {
  switch(
    format_name,
    "TIFF" = "tiff",
    "PDF" = grDevices::pdf,
    "SVG" = grDevices::svg,
    "png"
  )
}

plot_export_extension <- function(format_name) {
  tolower(format_name)
}

plot_preset_values <- function(preset_name) {
  switch(
    preset_name,
    "Journal inhibitor" = list(
      plot_style = "Publication boxed",
      palette_name = "Bright contrast",
      legend_position = "Right",
      background_fill = "White",
      use_group_shapes = TRUE,
      show_half_max_line = TRUE,
      base_font_size = 18,
      point_size = 3.8,
      line_width = 1.2
    ),
    "High-contrast figure" = list(
      plot_style = "Framed clean",
      palette_name = "Bright contrast",
      legend_position = "Right",
      background_fill = "White",
      use_group_shapes = TRUE,
      show_half_max_line = TRUE,
      base_font_size = 16,
      point_size = 3.5,
      line_width = 1.1
    ),
    "Minimal figure" = list(
      plot_style = "Minimal clean",
      palette_name = "Nature muted",
      legend_position = "Top",
      background_fill = "White",
      use_group_shapes = FALSE,
      show_half_max_line = FALSE,
      base_font_size = 15,
      point_size = 3.2,
      line_width = 1
    ),
    "Monochrome" = list(
      plot_style = "Classic axes",
      palette_name = "Black and gray",
      legend_position = "Right",
      background_fill = "White",
      use_group_shapes = TRUE,
      show_half_max_line = TRUE,
      base_font_size = 16,
      point_size = 3.5,
      line_width = 1.1
    ),
    NULL
  )
}

build_plot <- function(prepared, fit_data, input) {
  groups <- unique(prepared$raw$group)
  palette_values <- publication_palette(length(groups), input$palette_name)
  names(palette_values) <- groups
  shape_values <- publication_shapes(length(groups))
  names(shape_values) <- groups

  x_breaks <- parse_numeric_values(input$x_breaks)
  y_breaks <- parse_numeric_values(input$y_breaks)
  x_limits <- parse_axis_limits(input$x_limits)
  y_limits <- parse_axis_limits(input$y_limits)
  y_axis_settings <- default_y_axis_settings(
    prepared = prepared,
    fit_data = fit_data,
    explicit_breaks = y_breaks,
    explicit_limits = y_limits,
    normalization = input$normalization,
    response_transform = input$response_transform
  )
  y_breaks <- y_axis_settings$breaks
  y_limits <- y_axis_settings$limits

  x_label <- if (nzchar(trimws(input$x_axis_title))) trimws(input$x_axis_title) else (input$dose_col %||% "Dose")
  y_label <- if (nzchar(trimws(input$y_axis_title))) {
    trimws(input$y_axis_title)
  } else if (identical(input$response_transform, "Invert as 100 - response")) {
    "Inhibition (%)"
  } else if (identical(input$normalization, "Raw values")) {
    "Response"
  } else {
    "Response (%)"
  }

  plot_title <- if (nzchar(trimws(input$plot_title))) {
    trimws(input$plot_title)
  } else {
    "Dose-response curve fit"
  }
  plot_title <- if (isTRUE(input$show_plot_title)) plot_title else NULL
  plot_subtitle <- if (isTRUE(input$show_plot_subtitle)) {
    paste(
      "Model:",
      input$model_equation,
      "| Fit:",
      input$fit_to,
      "| Direction:",
      direction_summary_label(fit_data$results, input$direction)
    )
  } else {
    NULL
  }

  p <- ggplot()

  if (isTRUE(input$show_raw_points)) {
    if (isTRUE(input$use_group_shapes)) {
      p <- p + geom_point(
        data = prepared$raw,
        aes(x = dose, y = response, color = group, shape = group),
        alpha = 0.45,
        size = input$point_size * 0.8
      )
    } else {
      p <- p + geom_point(
        data = prepared$raw,
        aes(x = dose, y = response, color = group),
        alpha = 0.45,
        size = input$point_size * 0.8
      )
    }
  }

  if (isTRUE(input$show_errorbars)) {
    errorbar_df <- prepared$summary[is.finite(prepared$summary$response_sd), ]
    if (nrow(errorbar_df) > 0) {
      p <- p + geom_errorbar(
        data = errorbar_df,
        aes(
          x = dose,
          y = response,
          ymin = response - response_sd,
          ymax = response + response_sd,
          color = group
        ),
        width = 0.04,
        alpha = 0.65,
        size = max(0.3, input$line_width * 0.7)
      )
    }
  }

  if (isTRUE(input$use_group_shapes)) {
    p <- p +
      geom_point(
        data = prepared$summary,
        aes(x = dose, y = response, color = group, shape = group),
        size = input$point_size,
        stroke = 0.8
      )
  } else {
    p <- p +
      geom_point(
        data = prepared$summary,
        aes(x = dose, y = response, color = group),
        size = input$point_size,
        stroke = 0.8
      )
  }

  if (nrow(fit_data$curves) > 0) {
    p <- p +
      geom_line(
        data = fit_data$curves,
        aes(x = dose, y = response, color = group),
        size = input$line_width
      )
  }

  if (isTRUE(input$show_ic50_guides)) {
    ic50_df <- fit_data$results[is.finite(fit_data$results$ic50), c("group", "ic50")]
    if (nrow(ic50_df) > 0) {
      p <- p + geom_vline(
        data = ic50_df,
        aes(xintercept = ic50, color = group),
        linetype = "dashed",
        alpha = 0.5
      )
    }
  }

  if (isTRUE(input$show_half_max_line)) {
    p <- p + geom_hline(yintercept = 50, linetype = "dashed", size = 0.8, colour = "#9ca3af")
  }

  p <- p +
    scale_color_manual(values = palette_values) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_label,
      y = y_label,
      color = "Series"
    ) +
    publication_theme(
      style_name = input$plot_style,
      base_size = input$base_font_size,
      legend_position = input$legend_position,
      background_fill = input$background_fill
    )

  if (isTRUE(input$use_group_shapes)) {
    p <- p + scale_shape_manual(values = shape_values)
  }

  if (isTRUE(input$use_log10_axis)) {
    p <- p + scale_x_log10(
      breaks = if (is.null(x_breaks)) waiver() else x_breaks,
      labels = if (is.null(x_breaks)) waiver() else format_axis_labels,
      limits = x_limits
    )
  } else {
    p <- p + scale_x_continuous(
      breaks = if (is.null(x_breaks)) waiver() else x_breaks,
      labels = if (is.null(x_breaks)) waiver() else format_axis_labels,
      limits = x_limits
    )
  }

  p <- p + scale_y_continuous(
    breaks = if (is.null(y_breaks)) waiver() else y_breaks,
    labels = if (is.null(y_breaks)) waiver() else format_axis_labels
  )

  if (!is.null(y_limits)) {
    p <- p + coord_cartesian(ylim = y_limits)
  }

  if (isTRUE(input$facet_by_group) && length(groups) > 1) {
    p <- p + facet_wrap(~group)
  }

  p
}

app_theme <- bs_theme(
  version = 5,
  bg = "#f7f4ef",
  fg = "#1f2933",
  primary = "#0f766e",
  secondary = "#8b5e34",
  success = "#4d7c0f",
  info = "#1d4ed8",
  warning = "#b45309",
  danger = "#be123c"
)

ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Trebuchet MS', 'Segoe UI', Tahoma, sans-serif;
        background: linear-gradient(180deg, #f4efe6 0%, #f7f4ef 100%);
      }
      h1, h2, h3, h4, .control-label, .nav-tabs > li > a {
        font-family: Georgia, 'Palatino Linotype', serif;
      }
      .app-shell {
        background: rgba(255, 253, 250, 0.82);
        border: 1px solid #eadfca;
        border-radius: 20px;
        box-shadow: 0 20px 50px rgba(76, 56, 36, 0.08);
        padding: 20px 24px 10px 24px;
        margin-bottom: 20px;
        backdrop-filter: blur(4px);
      }
      .app-title {
        font-size: 34px;
        font-weight: 700;
        color: #102a43;
        margin-bottom: 6px;
      }
      .app-subtitle {
        color: #52606d;
        font-size: 16px;
        margin-bottom: 18px;
      }
      .sidebar-panel {
        background: linear-gradient(180deg, #fffdfa 0%, #f6efe3 100%);
        border-radius: 18px;
        padding: 18px;
        border: 1px solid #eadfca;
      }
      .note-block {
        background: #fff9ef;
        border-left: 5px solid #b45309;
        border-radius: 12px;
        padding: 12px 14px;
        margin-bottom: 14px;
      }
      .well {
        background: #fffdfa;
        border: 1px solid #eadfca;
        border-radius: 14px;
      }
      .nav-tabs {
        border-bottom: 1px solid #eadfca;
      }
      .nav-tabs > li > a {
        color: #52606d;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        color: #0f766e;
        background: #fffdfa;
        border: 1px solid #eadfca;
        border-bottom-color: transparent;
      }
      .btn-primary {
        border-radius: 999px;
        font-weight: 700;
      }
      .form-control, .selectize-input {
        border-radius: 12px !important;
        border-color: #d8c8ad;
      }
      .shiny-download-link {
        margin-right: 10px;
      }
    "))
  ),
  div(
    class = "app-shell",
    div(class = "app-title", "Dose-Response IC50 Lab"),
    div(
      class = "app-subtitle",
      "Load dose-response data, fit multiple sigmoid equations, calculate IC50 values, and export publication-ready plots in an open workflow."
    ),
    sidebarLayout(
      sidebarPanel(
        class = "sidebar-panel",
        width = 4,
        fileInput("data_file", "Upload data", accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")),
        actionButton("load_example", "Use example dataset", class = "btn-primary"),
        br(), br(),
        uiOutput("sheet_ui"),
        uiOutput("mapping_ui"),
        actionButton("run_analysis", "Run analysis", class = "btn-primary"),
        helpText("Change settings, then click Run analysis to refresh IC50 values and plots."),
        hr(),
        h4("Pre-processing"),
        selectInput(
          "normalization",
          "Response scaling",
          choices = c(
            "Raw values",
            "Normalize 0 to 100 (min to max)",
            "Normalize 100 to 0 (max to min)"
          ),
          selected = "Raw values"
        ),
        selectInput(
          "fit_to",
          "Fit curve using",
          choices = c("Group means", "All observations"),
          selected = "Group means"
        ),
        selectInput(
          "response_transform",
          "Response transform",
          choices = c("As entered", "Invert as 100 - response", "Mirror around min/max"),
          selected = "As entered"
        ),
        selectInput(
          "direction",
          "Curve direction",
          choices = c("Auto-detect", "Increasing", "Decreasing"),
          selected = "Auto-detect"
        ),
        hr(),
        h4("Equation"),
        selectInput(
          "model_equation",
          "Model",
          choices = all_model_choices,
          selected = "4PL"
        ),
        checkboxInput("compare_models", "Compare all models first (no bootstrap)", value = FALSE),
        helpText("Use this before bootstrap to see which equation gives the most reportable fits for your dataset."),
        selectInput(
          "weighting",
          "Weighting",
          choices = c("None", "1 / SD^2 from means"),
          selected = "None"
        ),
        selectInput(
          "ic50_uncertainty",
          "IC50 uncertainty",
          choices = c("95% CI", "\u00b1 SD", "\u00b1 SEM", "None"),
          selected = "None"
        ),
        numericInput("bootstrap_iterations", "Bootstrap iterations", value = 50, min = 20, max = 2000, step = 10),
        helpText("Use 100 to 200 bootstrap iterations for publication-oriented IC50 uncertainty."),
        numericInput("curve_points", "Curve resolution", value = 250, min = 100, max = 1000, step = 50),
        hr(),
        h4("Plot"),
        textInput("plot_title", "Plot title", value = "Dose-response curve fit"),
        checkboxInput("show_plot_title", "Show title", value = TRUE),
        checkboxInput("show_plot_subtitle", "Show subtitle", value = TRUE),
        selectInput(
          "plot_preset",
          "Plot preset",
          choices = c("Custom", "Journal inhibitor", "High-contrast figure", "Minimal figure", "Monochrome"),
          selected = "Journal inhibitor"
        ),
        checkboxInput("use_log10_axis", "Use log10 concentration axis", value = TRUE),
        checkboxInput("show_ic50_guides", "Show IC50 guide lines", value = TRUE),
        checkboxInput("show_half_max_line", "Show 50% reference line", value = TRUE),
        tags$details(
          class = "well",
          tags$summary("Plot styling options"),
          br(),
          textInput("x_axis_title", "X-axis title", value = ""),
          textInput("y_axis_title", "Y-axis title", value = ""),
          selectInput(
            "plot_style",
            "Plot style",
            choices = c("Publication boxed", "Framed clean", "Classic axes", "Minimal clean"),
            selected = "Publication boxed"
          ),
          selectInput(
            "palette_name",
            "Palette",
            choices = c("Bright contrast", "Colorblind safe", "Nature muted", "Black and gray", "Earth tones", "Viridis"),
            selected = "Bright contrast"
          ),
          selectInput(
            "legend_position",
            "Legend position",
            choices = c("Right", "Top", "Bottom", "Left", "None"),
            selected = "Right"
          ),
          selectInput(
            "background_fill",
            "Background",
            choices = c("White", "Warm paper", "Transparent"),
            selected = "White"
          ),
          checkboxInput("use_group_shapes", "Use different point shapes by group", value = TRUE),
          checkboxInput("show_raw_points", "Show raw points", value = TRUE),
          checkboxInput("show_errorbars", "Show SD error bars", value = TRUE),
          checkboxInput("facet_by_group", "Facet by group", value = FALSE),
          numericInput("base_font_size", "Base font size", value = 16, min = 8, max = 30, step = 1),
          numericInput("point_size", "Point size", value = 3.5, min = 1, max = 8, step = 0.25),
          numericInput("line_width", "Curve line width", value = 1.1, min = 0.4, max = 3, step = 0.1)
        ),
        tags$details(
          class = "well",
          tags$summary("Axis breaks and limits"),
          br(),
          textInput("x_breaks", "Custom x breaks", value = "", placeholder = "e.g. 1.56, 6.25, 25, 100, 400"),
          textInput("y_breaks", "Custom y breaks", value = "", placeholder = "e.g. 0, 25, 50, 75, 100"),
          textInput("x_limits", "X-axis limits", value = "", placeholder = "e.g. 0.5, 400"),
          textInput("y_limits", "Y-axis limits", value = "", placeholder = "e.g. 0, 100")
        ),
        tags$details(
          class = "well",
          tags$summary("Export settings"),
          br(),
          textInput("export_filename", "Export file name", value = "dose_response_curve"),
          selectInput("export_format", "Export format", choices = c("PNG", "TIFF", "PDF", "SVG"), selected = "PNG"),
          selectInput("export_units", "Export units", choices = c("in", "cm", "mm"), selected = "in"),
          numericInput("export_width", "Export width", value = 7, min = 3, max = 20, step = 0.5),
          numericInput("export_height", "Export height", value = 5.5, min = 3, max = 20, step = 0.5),
          numericInput("export_dpi", "Export DPI", value = 600, min = 72, max = 1200, step = 50)
        )
      ),
      mainPanel(
        width = 8,
        div(class = "note-block", textOutput("data_notes")),
        tabsetPanel(
          tabPanel(
            "Curve Plot",
            br(),
            plotOutput("dose_plot", height = "620px"),
            br(),
            downloadButton("download_plot", "Download plot file"),
            downloadButton("download_results", "Download fit table CSV")
          ),
          tabPanel(
            "Fit Results",
            br(),
            h4("Equation used"),
            textOutput("equation_used"),
            br(),
            DTOutput("fit_results_table")
          ),
          tabPanel(
            "Model Comparison",
            br(),
            uiOutput("model_comparison_summary"),
            br(),
            uiOutput("apply_suggested_model_ui"),
            br(),
            DTOutput("model_comparison_table")
          ),
          tabPanel(
            "Data Preview",
            br(),
            h4("Imported data"),
            DTOutput("raw_data_table"),
            br(),
            h4("Prepared summary"),
            DTOutput("summary_data_table")
          ),
          tabPanel(
            "Format Guide",
            br(),
            h4("Quick start"),
            tags$p("1. Upload your file or use the example dataset."),
            tags$p("2. Map the concentration/dose, response, and optional group columns."),
            tags$p("3. Choose response transform and model."),
            tags$p("4. Click Run analysis."),
            tags$p("5. Open 'Plot styling options' only when you want to polish the figure."),
            tags$p("6. Leave IC50 uncertainty set to None for speed. Turn on bootstrap only for final reporting."),
            br(),
            tags$p("Recommended columns: one numeric concentration or dose column, one numeric response column, and an optional grouping column such as compound, sample, or treatment."),
            tags$p("Supported files: CSV, TSV, TXT, XLS, XLSX."),
            tags$p("Important: concentration values must be greater than zero if you want a log-scale x axis or a standard IC50 fit."),
            tags$p("Units are not fixed by the app. You can use nM, uM, mg/mL, or any other unit as long as the x column is numeric, then write the exact unit in the x-axis title."),
            tags$p("For enzyme inhibition-style figures like the example you shared, use Response transform = 'Invert as 100 - response', Curve direction = 'Increasing', a 0 to 100 y-axis, and custom x breaks if you want specific concentration labels."),
            tags$p("For most datasets, leave Curve direction on Auto-detect so the app picks the better increasing or decreasing fit for each group."),
            tags$p("If you want a fixed-slope three-parameter dose-response fit, try '3PL (Hill fixed = 1)'."),
            tags$p("You can turn on 'Compare all models first (no bootstrap)' to screen the equations quickly, then use the suggested model for your final bootstrap uncertainty run."),
            tags$p("Recommended IC50 reporting: 95% CI is usually the best publication choice because it communicates uncertainty around the fitted parameter. SD is better for spread across repeats, and SEM is the least informative on its own."),
            tags$p("Practical speed guide: keep bootstrap off during exploration. If you need uncertainty, 50 iterations is a quick preview and 100 to 200 is a better publication-oriented starting point."),
            tags$p("If you choose 5PL, the app reports the half-max concentration solved from the fitted curve. That can differ slightly from the internal midpoint parameter when asymmetry is not 1."),
            h4("Example layout"),
            DTOutput("example_table")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  source_mode <- reactiveVal("example")

  observeEvent(input$data_file, {
    source_mode("file")
  })

  observeEvent(input$load_example, {
    source_mode("example")
  })

  observeEvent(input$plot_preset, {
    preset_values <- plot_preset_values(input$plot_preset)
    if (is.null(preset_values)) {
      return()
    }

    updateSelectInput(session, "plot_style", selected = preset_values$plot_style)
    updateSelectInput(session, "palette_name", selected = preset_values$palette_name)
    updateSelectInput(session, "legend_position", selected = preset_values$legend_position)
    updateSelectInput(session, "background_fill", selected = preset_values$background_fill)
    updateCheckboxInput(session, "use_group_shapes", value = preset_values$use_group_shapes)
    updateCheckboxInput(session, "show_half_max_line", value = preset_values$show_half_max_line)
    updateNumericInput(session, "base_font_size", value = preset_values$base_font_size)
    updateNumericInput(session, "point_size", value = preset_values$point_size)
    updateNumericInput(session, "line_width", value = preset_values$line_width)
  }, ignoreInit = FALSE)

  current_data <- reactive({
    if (identical(source_mode(), "example")) {
      return(sample_dataset())
    }

    req(input$data_file)
    read_input_data(
      path = input$data_file$datapath,
      filename = input$data_file$name,
      sheet = input$sheet_name %||% NULL
    )
  })

  current_sheets <- reactive({
    if (!identical(source_mode(), "file") || is.null(input$data_file)) {
      return(character())
    }

    available_sheets(input$data_file$datapath, input$data_file$name)
  })

  output$sheet_ui <- renderUI({
    sheets <- current_sheets()
    if (!length(sheets)) {
      return(NULL)
    }

    selectInput("sheet_name", "Excel sheet", choices = sheets, selected = sheets[1])
  })

  output$mapping_ui <- renderUI({
    df <- current_data()
    nm <- names(df)
    req(length(nm) > 0)

    tagList(
      h4("Column mapping"),
      selectInput(
        "dose_col",
        "Concentration or dose column",
        choices = nm,
        selected = guess_column(df, c("dose", "conc", "concentration", "um", "nm"), nm[1])
      ),
      selectInput(
        "response_col",
        "Response column",
        choices = nm,
        selected = guess_column(df, c("response", "signal", "viability", "inhibition", "activity", "effect"), nm[min(2, length(nm))])
      ),
      selectInput(
        "group_col",
        "Group or compound column",
        choices = c("None", nm),
        selected = if ("compound" %in% nm) "compound" else if ("group" %in% nm) "group" else "None"
      )
    )
  })

  analysis_result <- eventReactive(input$run_analysis, {
    req(input$dose_col, input$response_col)

    withProgress(message = "Running dose-response analysis...", value = 0, {
      setProgress(value = 0.03, detail = "Preparing dataset")
      prepared <- prepare_dataset(
        data = current_data(),
        dose_col = input$dose_col,
        response_col = input$response_col,
        group_col = input$group_col,
        normalization = input$normalization,
        response_transform = input$response_transform
      )

      comparison <- NULL
      if (isTRUE(input$compare_models)) {
        comparison_end <- if (identical(input$ic50_uncertainty, "None")) 0.97 else 0.55
        comparison <- compare_models(
          prepared = prepared,
          fit_to = input$fit_to,
          direction = input$direction,
          weighting = input$weighting,
          curve_points = input$curve_points,
          progress_callback = function(detail_text, current_step, total_steps) {
            setProgress(
              value = 0.05 + (comparison_end - 0.05) * current_step / max(total_steps, 1),
              detail = detail_text
            )
          }
        )
      }

      fit <- NULL
      if (isTRUE(input$compare_models) &&
          identical(input$ic50_uncertainty, "None") &&
          !is.null(comparison) &&
          input$model_equation %in% names(comparison$model_fits)) {
        setProgress(value = 0.99, detail = sprintf("Using %s fit from model comparison", input$model_equation))
        fit <- comparison$model_fits[[input$model_equation]]
      } else {
        fit_start <- if (isTRUE(input$compare_models)) 0.58 else 0.05
        fit <- fit_dataset(
          prepared = prepared,
          fit_to = input$fit_to,
          model = input$model_equation,
          direction = input$direction,
          weighting = input$weighting,
          curve_points = input$curve_points,
          uncertainty_method = input$ic50_uncertainty,
          bootstrap_iterations = input$bootstrap_iterations,
          progress_callback = function(detail_text, current_step, total_steps) {
            setProgress(
              value = fit_start + (1 - fit_start) * current_step / max(total_steps, 1),
              detail = detail_text
            )
          }
        )
      }

      list(prepared = prepared, fit = fit, comparison = comparison)
    })
  }, ignoreNULL = FALSE)

  output$data_notes <- renderText({
    comparison_note <- NULL
    if (!is.null(analysis_result()$comparison)) {
      comparison_note <- analysis_result()$comparison$recommendation_text
    }

    note_parts <- c(analysis_result()$prepared$notes, analysis_result()$fit$message, comparison_note)
    note_parts <- note_parts[nzchar(note_parts)]
    paste(note_parts, collapse = " | ")
  })

  output$equation_used <- renderText({
    equation_label(input$model_equation, input$direction)
  })

  output$dose_plot <- renderPlot({
    build_plot(analysis_result()$prepared, analysis_result()$fit, input)
  }, res = 130)

  output$fit_results_table <- renderDT({
    result_df <- analysis_result()$fit$results
    numeric_cols <- vapply(result_df, is.numeric, logical(1))
    result_df[numeric_cols] <- lapply(result_df[numeric_cols], function(x) round(x, 4))

    display_order <- c(
      "group",
      "model",
      "direction",
      "ic50_reported",
      "reporting_status",
      "fit_reliability",
      "suggested_direction",
      "r_squared",
      "n_points",
      "fit_status"
    )
    display_cols <- intersect(display_order, names(result_df))
    hidden_cols <- setdiff(names(result_df), display_cols)
    visible_df <- result_df[, c(display_cols, hidden_cols), drop = FALSE]

    dt <- datatable(
      visible_df,
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel"),
        pageLength = 8,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = match(hidden_cols, names(visible_df)) - 1)
        )
      )
    )

    dt <- formatStyle(
      dt,
      "reporting_status",
      target = "row",
      backgroundColor = styleEqual(
        c(
          "Report numeric IC50",
          "Do not report numeric IC50 (50% not reached)",
          "Do not report numeric IC50 (extrapolated)",
          "Review fit before reporting",
          "No fit available"
        ),
        c("#eefbf3", "#fff7db", "#fff0d6", "#fdecec", "#f3f4f6")
      )
    )
    dt <- formatStyle(dt, "reporting_status", fontWeight = "700")
    dt
  })

  output$model_comparison_summary <- renderUI({
    comparison <- analysis_result()$comparison

    if (is.null(comparison)) {
      return(tags$p("Turn on 'Compare all models first (no bootstrap)' and click Run analysis to benchmark the equations before choosing one for bootstrap uncertainty."))
    }

    tagList(
      tags$p(comparison$recommendation_text),
      tags$p("Bootstrap is not used in this comparison. If you want uncertainty, apply or choose the suggested model and run the analysis again with IC50 uncertainty enabled.")
    )
  })

  output$apply_suggested_model_ui <- renderUI({
    comparison <- analysis_result()$comparison
    if (is.null(comparison) || !nzchar(comparison$recommended_model %||% "")) {
      return(NULL)
    }

    actionButton(
      "apply_suggested_model",
      sprintf("Use suggested model (%s)", comparison$recommended_model),
      class = "btn-primary"
    )
  })

  observeEvent(input$apply_suggested_model, {
    comparison <- analysis_result()$comparison
    req(comparison$recommended_model)
    updateSelectInput(session, "model_equation", selected = comparison$recommended_model)
  })

  output$model_comparison_table <- renderDT({
    comparison <- analysis_result()$comparison

    if (is.null(comparison)) {
      return(datatable(
        data.frame(note = "Model comparison not run yet."),
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE)
      ))
    }

    compare_df <- comparison$summary
    numeric_cols <- vapply(compare_df, is.numeric, logical(1))
    compare_df[numeric_cols] <- lapply(compare_df[numeric_cols], function(x) round(x, 4))

    dt <- datatable(
      compare_df,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        scrollX = TRUE
      )
    )

    dt <- formatStyle(
      dt,
      "recommendation",
      target = "row",
      backgroundColor = styleEqual("Suggested", "#eefbf3")
    )
    dt <- formatStyle(dt, "recommendation", fontWeight = "700")
    dt
  })

  output$raw_data_table <- renderDT({
    datatable(
      current_data(),
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  output$summary_data_table <- renderDT({
    datatable(
      analysis_result()$prepared$summary,
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE)
    )
  })

  output$example_table <- renderDT({
    datatable(
      sample_dataset(),
      rownames = FALSE,
      options = list(pageLength = 8, scrollX = TRUE, searching = FALSE, lengthChange = FALSE)
    )
  })

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("dose_response_ic50_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(analysis_result()$fit$results, file, row.names = FALSE)
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() {
      base_name <- trimws(input$export_filename)
      if (!nzchar(base_name)) {
        base_name <- paste0("dose_response_curve_", Sys.Date())
      }
      paste0(base_name, ".", plot_export_extension(input$export_format))
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = build_plot(analysis_result()$prepared, analysis_result()$fit, input),
        device = plot_export_device(input$export_format),
        width = input$export_width,
        height = input$export_height,
        units = input$export_units,
        dpi = input$export_dpi,
        bg = resolve_plot_fill(input$background_fill)
      )
    }
  )
}

shinyApp(ui, server)

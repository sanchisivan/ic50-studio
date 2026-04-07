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
manual_control_normalization <- "Normalize using manual 0% and 100% controls"

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

normalize_vector <- function(x, mode, control_100 = NA_real_, control_0 = NA_real_) {
  if (mode == "Raw values") {
    return(x)
  }

  if (identical(mode, manual_control_normalization)) {
    if (!is.finite(control_100) || !is.finite(control_0) || isTRUE(all.equal(control_100, control_0))) {
      stop("Manual normalization requires numeric 100% and 0% control values that are different.")
    }
    return((x - control_0) / (control_100 - control_0) * 100)
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

curve_grid_values <- function(x, curve_points, use_log10_axis, extend_curve_toward_zero = FALSE, extra_log_decades = 1) {
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)

  if (!isTRUE(extend_curve_toward_zero)) {
    lower_bound <- min_x
  } else if (isTRUE(use_log10_axis)) {
    lower_bound <- max(min_x / (10^max(extra_log_decades, 0)), .Machine$double.eps)
  } else {
    lower_bound <- 0
  }

  if (isTRUE(use_log10_axis)) {
    return(exp(seq(log(lower_bound), log(max_x), length.out = curve_points)))
  }

  seq(lower_bound, max_x, length.out = curve_points)
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

format_decimal_text <- function(x, decimals = 2) {
  if (!is.finite(x)) {
    return(NA_character_)
  }
  formatC(x, format = "f", digits = decimals)
}

potency_metric_label <- function(potency_metric = "IC50") {
  if (!nzchar(trimws(potency_metric %||% ""))) {
    return("IC50")
  }
  trimws(potency_metric)
}

potency_metric_lower <- function(potency_metric = "IC50") {
  tolower(potency_metric_label(potency_metric))
}

potency_target_response <- function(params, potency_metric = "IC50") {
  if (identical(potency_metric_label(potency_metric), "EC50")) {
    return(params$bottom + 0.5 * (params$top - params$bottom))
  }
  50
}

potency_target_reason <- function(potency_metric = "IC50") {
  if (identical(potency_metric_label(potency_metric), "EC50")) {
    return("Half-max effect not reached")
  }
  "50% not reached"
}

potency_target_phrase <- function(potency_metric = "IC50") {
  if (identical(potency_metric_label(potency_metric), "EC50")) {
    return("half-max effect")
  }
  "50%"
}

rename_potency_column_names <- function(column_names, potency_metric = "IC50") {
  lower_label <- potency_metric_lower(potency_metric)
  renamed <- gsub("^ic50", lower_label, column_names)
  renamed[column_names == "midpoint_parameter"] <- paste0(lower_label, "_parameter")
  renamed
}

join_messages <- function(...) {
  parts <- c(...)
  parts <- parts[nzchar(parts) & !is.na(parts)]
  paste(parts, collapse = " | ")
}

join_reason_flags <- function(...) {
  parts <- unique(c(...))
  parts <- parts[nzchar(parts) & !is.na(parts)]
  paste(parts, collapse = "; ")
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

prepare_dataset <- function(data, dose_col, response_col, group_col = NULL, normalization = "Raw values", response_transform = "As entered", control_100 = NA_real_, control_0 = NA_real_) {
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
    piece$response <- normalize_vector(piece$response, normalization, control_100 = control_100, control_0 = control_0)
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
    sprintf("Normalization: %s", normalization),
    sprintf("Response transform: %s", response_transform)
  )

  if (identical(normalization, manual_control_normalization)) {
    notes <- c(
      notes,
      sprintf(
        "Manual control normalization: 100%% control = %s, 0%% control = %s. Formula used: 100 * (Y - control_0) / (control_100 - control_0).",
        format_decimal_text(control_100, 4),
        format_decimal_text(control_0, 4)
      )
    )
  }

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

is_numericish_column <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }

  values <- trimws(as.character(x))
  values <- values[!is.na(values) & nzchar(values)]
  if (!length(values)) {
    return(FALSE)
  }

  numeric_values <- suppressWarnings(as.numeric(values))
  all(is.finite(numeric_values))
}

ordered_levels_from_values <- function(x) {
  values <- trimws(as.character(x))
  values <- values[!is.na(values) & nzchar(values)]

  if (!length(values)) {
    return(character())
  }

  if (is_numericish_column(values)) {
    numeric_values <- suppressWarnings(as.numeric(values))
    return(unique(values[order(numeric_values, values)]))
  }

  unique(values)
}

guess_bioassay_x_column <- function(data, plot_type = "Bar plot", dose_col = NULL, group_col = NULL) {
  nm <- names(data)
  if (!length(nm)) {
    return("")
  }

  numeric_cols <- nm[vapply(data, is_numericish_column, logical(1))]
  discrete_cols <- setdiff(nm, numeric_cols)

  candidates <- if (identical(plot_type, "Line plot")) {
    c(
      dose_col,
      guess_column(data, c("time", "hour", "day", "dose", "conc", "concentration"), NULL),
      numeric_cols[1],
      nm[1]
    )
  } else {
    c(
      if (!is.null(group_col) && !identical(group_col, "None")) group_col,
      guess_column(data, c("group", "compound", "sample", "treatment", "peptide", "variant", "name"), NULL),
      discrete_cols[1],
      dose_col,
      nm[1]
    )
  }

  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates) & candidates %in% nm])
  candidates[1] %||% nm[1]
}

guess_bioassay_series_column <- function(data, plot_type = "Bar plot", x_col = NULL, dose_col = NULL, group_col = NULL) {
  nm <- names(data)
  if (!length(nm)) {
    return("None")
  }

  numeric_cols <- nm[vapply(data, is_numericish_column, logical(1))]
  discrete_cols <- setdiff(nm, numeric_cols)

  candidates <- if (identical(plot_type, "Line plot")) {
    c(
      if (!is.null(group_col) && !identical(group_col, "None") && !identical(group_col, x_col)) group_col,
      guess_column(data, c("group", "compound", "sample", "treatment", "peptide", "variant"), NULL),
      discrete_cols[1]
    )
  } else {
    c(
      if (!is.null(dose_col) && !identical(dose_col, x_col)) dose_col,
      if (!is.null(group_col) && !identical(group_col, "None") && !identical(group_col, x_col)) group_col,
      guess_column(data, c("dose", "conc", "concentration", "time", "hour", "day"), NULL),
      discrete_cols[1]
    )
  }

  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates) & candidates %in% nm])
  candidates[1] %||% "None"
}

summarize_bioassay_dataset <- function(raw_df) {
  split_rows <- split(
    raw_df,
    interaction(raw_df$facet, raw_df$series, raw_df$x, drop = TRUE, lex.order = TRUE)
  )

  summary_list <- lapply(split_rows, function(piece) {
    values <- piece$y
    sd_value <- if (length(values) > 1) stats::sd(values) else NA_real_
    sem_value <- if (length(values) > 1) sd_value / sqrt(length(values)) else NA_real_
    ci95_value <- if (length(values) > 1) stats::qt(0.975, df = length(values) - 1) * sem_value else NA_real_
    label_values <- unique(piece$label[!is.na(piece$label) & nzchar(piece$label)])

    data.frame(
      facet = as.character(piece$facet[1]),
      series = as.character(piece$series[1]),
      x = as.character(piece$x[1]),
      x_order = piece$x_order[1],
      x_numeric = if (all(is.na(piece$x_numeric))) NA_real_ else piece$x_numeric[1],
      n = length(values),
      mean = mean(values),
      median = stats::median(values),
      sd = sd_value,
      sem = sem_value,
      ci95 = ci95_value,
      q1 = as.numeric(stats::quantile(values, probs = 0.25, names = FALSE, na.rm = TRUE)),
      q3 = as.numeric(stats::quantile(values, probs = 0.75, names = FALSE, na.rm = TRUE)),
      min = min(values),
      max = max(values),
      label = if (length(label_values)) paste(label_values, collapse = ", ") else NA_character_,
      stringsAsFactors = FALSE
    )
  })

  summary_df <- do.call(rbind, summary_list)
  summary_df$x <- factor(summary_df$x, levels = levels(raw_df$x))
  summary_df$series <- factor(summary_df$series, levels = levels(raw_df$series))
  summary_df$facet <- factor(summary_df$facet, levels = levels(raw_df$facet))
  summary_df[order(summary_df$facet, summary_df$series, summary_df$x_order), , drop = FALSE]
}

prepare_bioassay_dataset <- function(data, x_col, y_col, series_col = NULL, facet_col = NULL, label_col = NULL) {
  if (!x_col %in% names(data) || !y_col %in% names(data)) {
    stop("Choose both x and y columns for the other-plot module.")
  }

  x_source <- data[[x_col]]
  y_source <- suppressWarnings(as.numeric(data[[y_col]]))
  x_is_numeric <- is_numericish_column(x_source)
  x_numeric <- if (x_is_numeric) suppressWarnings(as.numeric(as.character(x_source))) else rep(NA_real_, length(x_source))
  x_values <- trimws(as.character(x_source))

  series_values <- if (is.null(series_col) || identical(series_col, "None")) {
    rep("All series", nrow(data))
  } else {
    trimws(as.character(data[[series_col]]))
  }

  facet_values <- if (is.null(facet_col) || identical(facet_col, "None")) {
    rep("All data", nrow(data))
  } else {
    trimws(as.character(data[[facet_col]]))
  }

  label_values <- if (is.null(label_col) || identical(label_col, "None")) {
    rep(NA_character_, nrow(data))
  } else {
    trimws(as.character(data[[label_col]]))
  }

  keep <- is.finite(y_source) &
    !is.na(x_values) & nzchar(x_values) &
    !is.na(series_values) & nzchar(series_values) &
    !is.na(facet_values) & nzchar(facet_values)

  if (x_is_numeric) {
    keep <- keep & is.finite(x_numeric)
  }

  removed_rows <- sum(!keep)

  raw_df <- data.frame(
    x = x_values[keep],
    x_numeric = x_numeric[keep],
    y = y_source[keep],
    series = series_values[keep],
    facet = facet_values[keep],
    label = label_values[keep],
    stringsAsFactors = FALSE
  )

  if (!nrow(raw_df)) {
    stop("No plottable rows remain after filtering. Check the other-plot x/y mappings and make sure the y column is numeric.")
  }

  raw_df$x <- factor(raw_df$x, levels = ordered_levels_from_values(raw_df$x))
  raw_df$series <- factor(raw_df$series, levels = ordered_levels_from_values(raw_df$series))
  raw_df$facet <- factor(raw_df$facet, levels = ordered_levels_from_values(raw_df$facet))
  raw_df$x_order <- as.integer(raw_df$x)
  raw_df <- raw_df[order(raw_df$facet, raw_df$series, raw_df$x_order), , drop = FALSE]
  rownames(raw_df) <- NULL

  notes <- c(
    sprintf("Other-plot rows retained: %s", nrow(raw_df)),
    sprintf("Rows removed from other plots: %s", removed_rows),
    sprintf("Other-plot x-axis column: %s", x_col),
    sprintf("Other-plot y-axis column: %s", y_col)
  )

  if (!is.null(series_col) && !identical(series_col, "None")) {
    notes <- c(notes, sprintf("Series column: %s", series_col))
  }

  if (!is.null(facet_col) && !identical(facet_col, "None")) {
    notes <- c(notes, sprintf("Facet column: %s", facet_col))
  }

  if (!is.null(label_col) && !identical(label_col, "None")) {
    notes <- c(notes, sprintf("Annotation column: %s", label_col))
  }

  list(
    raw = raw_df,
    summary = summarize_bioassay_dataset(raw_df),
    x_is_numeric = x_is_numeric,
    x_label = x_col,
    y_label = y_col,
    series_label = if (!is.null(series_col) && !identical(series_col, "None")) series_col else NULL,
    facet_label = if (!is.null(facet_col) && !identical(facet_col, "None")) facet_col else NULL,
    label_label = if (!is.null(label_col) && !identical(label_col, "None")) label_col else NULL,
    notes = notes
  )
}

apply_bioassay_summary_preferences <- function(summary_df, statistic = "Mean", error_bars = "SEM") {
  if (!nrow(summary_df)) {
    return(summary_df)
  }

  summary_df$summary_y <- if (identical(statistic, "Median")) summary_df$median else summary_df$mean
  summary_df$ymin <- NA_real_
  summary_df$ymax <- NA_real_

  if (identical(error_bars, "IQR")) {
    summary_df$ymin <- summary_df$q1
    summary_df$ymax <- summary_df$q3
    return(summary_df)
  }

  spread <- switch(
    error_bars,
    "SD" = summary_df$sd,
    "SEM" = summary_df$sem,
    "95% CI" = summary_df$ci95,
    rep(NA_real_, nrow(summary_df))
  )

  finite_spread <- is.finite(spread)
  summary_df$ymin[finite_spread] <- summary_df$summary_y[finite_spread] - spread[finite_spread]
  summary_df$ymax[finite_spread] <- summary_df$summary_y[finite_spread] + spread[finite_spread]
  summary_df
}

row_max_finite <- function(...) {
  values <- cbind(...)
  apply(values, 1, function(piece) {
    finite_values <- piece[is.finite(piece)]
    if (!length(finite_values)) {
      return(NA_real_)
    }
    max(finite_values)
  })
}

bar_key_from_components <- function(facet, series, x) {
  paste(as.character(facet), as.character(series), as.character(x), sep = "\r")
}

generate_letter_codes <- function(n) {
  if (n <= 0) {
    return(character())
  }

  alphabet <- letters
  codes <- character(n)

  for (i in seq_len(n)) {
    value <- i
    code <- ""

    while (value > 0) {
      remainder <- (value - 1) %% length(alphabet)
      code <- paste0(alphabet[remainder + 1], code)
      value <- (value - 1) %/% length(alphabet)
    }

    codes[i] <- code
  }

  codes
}

absorb_letter_matrix <- function(letter_matrix) {
  if (!length(letter_matrix)) {
    return(letter_matrix)
  }

  keep_cols <- colSums(letter_matrix) > 0
  letter_matrix <- letter_matrix[, keep_cols, drop = FALSE]

  if (!ncol(letter_matrix)) {
    return(letter_matrix)
  }

  duplicate_cols <- duplicated(as.data.frame(t(letter_matrix)))
  if (any(duplicate_cols)) {
    letter_matrix <- letter_matrix[, !duplicate_cols, drop = FALSE]
  }

  repeat {
    removed_any <- FALSE

    if (ncol(letter_matrix) < 2) {
      break
    }

    for (i in seq_len(ncol(letter_matrix))) {
      if (removed_any) {
        break
      }

      for (j in seq_len(ncol(letter_matrix))) {
        if (i == j) {
          next
        }

        subset_check <- all(!letter_matrix[, i] | letter_matrix[, j])
        strictly_smaller <- any(letter_matrix[, j] & !letter_matrix[, i])

        if (subset_check && strictly_smaller) {
          letter_matrix <- letter_matrix[, -i, drop = FALSE]
          removed_any <- TRUE
          break
        }
      }
    }

    if (!removed_any) {
      break
    }
  }

  letter_matrix
}

compact_letters_from_significance <- function(significant_matrix, group_scores = NULL) {
  group_names <- rownames(significant_matrix)
  n_groups <- length(group_names)

  if (!n_groups) {
    return(setNames(character(), character()))
  }

  if (n_groups == 1) {
    return(setNames("a", group_names))
  }

  letter_matrix <- matrix(
    TRUE,
    nrow = n_groups,
    ncol = 1,
    dimnames = list(group_names, NULL)
  )

  sig_pairs <- which(significant_matrix & upper.tri(significant_matrix), arr.ind = TRUE)

  if (nrow(sig_pairs) > 0) {
    for (pair_index in seq_len(nrow(sig_pairs))) {
      i <- sig_pairs[pair_index, "row"]
      j <- sig_pairs[pair_index, "col"]
      split_cols <- which(letter_matrix[i, ] & letter_matrix[j, ])

      if (!length(split_cols)) {
        next
      }

      for (col_index in rev(split_cols)) {
        col_i <- letter_matrix[, col_index]
        col_j <- letter_matrix[, col_index]
        col_i[i] <- FALSE
        col_j[j] <- FALSE
        letter_matrix[, col_index] <- col_i
        letter_matrix <- cbind(letter_matrix, col_j)
      }

      letter_matrix <- absorb_letter_matrix(letter_matrix)
    }
  }

  if (!is.null(group_scores) && ncol(letter_matrix) > 1) {
    col_scores <- vapply(seq_len(ncol(letter_matrix)), function(idx) {
      included_groups <- names(group_scores)[letter_matrix[, idx]]
      if (!length(included_groups)) {
        return(-Inf)
      }
      max(group_scores[included_groups], na.rm = TRUE)
    }, numeric(1))

    letter_matrix <- letter_matrix[, order(col_scores, decreasing = TRUE), drop = FALSE]
  }

  letter_codes <- generate_letter_codes(ncol(letter_matrix))
  group_letters <- vapply(seq_len(nrow(letter_matrix)), function(idx) {
    paste0(letter_codes[letter_matrix[idx, ]], collapse = "")
  }, character(1))

  group_letters[group_letters == ""] <- NA_character_
  setNames(group_letters, group_names)
}

symmetrize_pairwise_p <- function(lower_matrix, group_levels) {
  p_matrix <- matrix(
    NA_real_,
    nrow = length(group_levels),
    ncol = length(group_levels),
    dimnames = list(group_levels, group_levels)
  )
  diag(p_matrix) <- 1

  if (length(lower_matrix)) {
    for (row_name in rownames(lower_matrix)) {
      for (col_name in colnames(lower_matrix)) {
        p_value <- lower_matrix[row_name, col_name]
        if (!is.finite(p_value)) {
          next
        }
        p_matrix[row_name, col_name] <- p_value
        p_matrix[col_name, row_name] <- p_value
      }
    }
  }

  p_matrix
}

compute_pairwise_p_matrix <- function(values, groups, method_name) {
  group_factor <- factor(groups)
  group_levels <- levels(group_factor)

  if (length(group_levels) < 2) {
    stop("At least two groups are required.")
  }

  if (identical(method_name, "ANOVA + Tukey HSD")) {
    if (length(values) <= length(group_levels)) {
      stop("ANOVA + Tukey HSD needs at least one residual degree of freedom.")
    }

    fit <- stats::aov(values ~ group_factor)
    tukey <- stats::TukeyHSD(fit)
    tukey_table <- tukey$group_factor

    if (is.null(tukey_table) || !nrow(tukey_table)) {
      stop("Tukey HSD did not return pairwise comparisons.")
    }

    p_matrix <- matrix(
      NA_real_,
      nrow = length(group_levels),
      ncol = length(group_levels),
      dimnames = list(group_levels, group_levels)
    )
    diag(p_matrix) <- 1

    for (comparison_name in rownames(tukey_table)) {
      comparison_parts <- strsplit(comparison_name, "-", fixed = TRUE)[[1]]
      if (length(comparison_parts) != 2) {
        next
      }

      group_high <- comparison_parts[1]
      group_low <- comparison_parts[2]
      p_value <- tukey_table[comparison_name, "p adj"]

      p_matrix[group_high, group_low] <- p_value
      p_matrix[group_low, group_high] <- p_value
    }

    return(p_matrix)
  }

  if (identical(method_name, "Kruskal + pairwise Wilcoxon")) {
    pairwise <- suppressWarnings(
      stats::pairwise.wilcox.test(
        x = values,
        g = group_factor,
        p.adjust.method = "holm",
        exact = FALSE
      )
    )

    if (is.null(pairwise$p.value)) {
      stop("Pairwise Wilcoxon did not return a p-value matrix.")
    }

    return(symmetrize_pairwise_p(pairwise$p.value, group_levels))
  }

  stop("Unknown bar-plot letter method.")
}

compute_auto_bioassay_letters <- function(prepared, method_name, scope_name, alpha = 0.05) {
  raw_df <- prepared$raw
  summary_df <- prepared$summary

  if (!nrow(raw_df) || !nrow(summary_df)) {
    return(list(labels = setNames(character(), character()), notes = "Automatic letters could not be computed because no rows were available."))
  }

  work_df <- raw_df
  work_df$bar_key <- bar_key_from_components(work_df$facet, work_df$series, work_df$x)

  if (identical(scope_name, "Across all bars in each facet")) {
    work_df$family_id <- as.character(work_df$facet)
    work_df$group_value <- work_df$bar_key
  } else if (identical(scope_name, "Within each series in each facet")) {
    work_df$family_id <- paste(as.character(work_df$facet), as.character(work_df$series), sep = "\r")
    work_df$group_value <- as.character(work_df$x)
  } else if (identical(scope_name, "Within each x group in each facet")) {
    work_df$family_id <- paste(as.character(work_df$facet), as.character(work_df$x), sep = "\r")
    work_df$group_value <- as.character(work_df$series)
  } else {
    stop("Unknown bar-plot comparison scope.")
  }

  label_map <- setNames(rep(NA_character_, nrow(summary_df)), bar_key_from_components(summary_df$facet, summary_df$series, summary_df$x))
  family_pieces <- split(work_df, work_df$family_id)
  analyzed_families <- 0
  skipped_messages <- character()

  for (family_name in names(family_pieces)) {
    piece <- family_pieces[[family_name]]
    group_values <- unique(as.character(piece$group_value))

    if (length(group_values) < 2) {
      skipped_messages <- c(skipped_messages, sprintf("Skipped one comparison family because it had fewer than two groups."))
      next
    }

    group_lookup <- setNames(paste0("g", seq_along(group_values)), group_values)
    reverse_lookup <- setNames(names(group_lookup), group_lookup)
    piece$group_id <- factor(group_lookup[as.character(piece$group_value)], levels = unname(group_lookup))

    p_matrix <- try(
      compute_pairwise_p_matrix(piece$y, piece$group_id, method_name),
      silent = TRUE
    )

    if (inherits(p_matrix, "try-error")) {
      skipped_messages <- c(
        skipped_messages,
        sprintf("Skipped one comparison family because %s.", conditionMessage(attr(p_matrix, "condition") %||% simpleError("the test failed")))
      )
      next
    }

    off_diagonal <- p_matrix[row(p_matrix) != col(p_matrix)]
    if (!length(off_diagonal) || !any(is.finite(off_diagonal))) {
      skipped_messages <- c(skipped_messages, "Skipped one comparison family because no pairwise p-values were available.")
      next
    }

    if (any(is.na(off_diagonal))) {
      skipped_messages <- c(skipped_messages, "Skipped one comparison family because some pairwise comparisons returned missing p-values.")
      next
    }

    group_scores <- tapply(piece$y, piece$group_id, mean, na.rm = TRUE)
    significant_matrix <- p_matrix < alpha
    diag(significant_matrix) <- FALSE
    letters_by_id <- compact_letters_from_significance(significant_matrix, group_scores = group_scores)
    letters_by_group <- setNames(letters_by_id[names(reverse_lookup)], reverse_lookup)

    piece_summary <- summary_df[bar_key_from_components(summary_df$facet, summary_df$series, summary_df$x) %in% unique(piece$bar_key), , drop = FALSE]
    piece_keys <- bar_key_from_components(piece_summary$facet, piece_summary$series, piece_summary$x)

    if (identical(scope_name, "Across all bars in each facet")) {
      label_map[piece_keys] <- letters_by_group[piece_keys]
    } else if (identical(scope_name, "Within each series in each facet")) {
      label_map[piece_keys] <- letters_by_group[as.character(piece_summary$x)]
    } else {
      label_map[piece_keys] <- letters_by_group[as.character(piece_summary$series)]
    }

    analyzed_families <- analyzed_families + 1
  }

  note_parts <- c(
    sprintf(
      "Automatic letters: %s family/families analyzed using %s at alpha = %s (%s).",
      analyzed_families,
      method_name,
      format_decimal_text(alpha, 3),
      scope_name
    ),
    unique(skipped_messages)
  )

  list(
    labels = label_map,
    notes = note_parts[nzchar(note_parts)]
  )
}

resolve_bioassay_label_data <- function(prepared, summary_df, input) {
  output_df <- summary_df
  output_df$display_label <- NA_character_
  output_df$label_source <- "None"
  notes <- character()
  label_mode <- input$bioassay_label_mode %||% "None"

  if (identical(label_mode, "Annotation column")) {
    if (is.null(prepared$label_label)) {
      notes <- c(notes, "Annotation labels are enabled, but no annotation column is selected.")
    } else {
      output_df$display_label <- output_df$label
      output_df$label_source[!is.na(output_df$display_label) & nzchar(output_df$display_label)] <- "Annotation column"
    }
  } else if (identical(label_mode, "Auto significance letters (bar plot)")) {
    if (!identical(input$bioassay_plot_type, "Bar plot")) {
      notes <- c(notes, "Automatic significance letters are currently available only for bar plots.")
    } else {
      auto_letters <- compute_auto_bioassay_letters(
        prepared = prepared,
        method_name = input$bioassay_letters_method %||% "ANOVA + Tukey HSD",
        scope_name = input$bioassay_letters_scope %||% "Across all bars in each facet",
        alpha = input$bioassay_letters_alpha %||% 0.05
      )

      bar_keys <- bar_key_from_components(output_df$facet, output_df$series, output_df$x)
      output_df$display_label <- auto_letters$labels[bar_keys]
      output_df$label_source[!is.na(output_df$display_label) & nzchar(output_df$display_label)] <- "Auto significance letters"
      notes <- c(notes, auto_letters$notes)
    }
  }

  label_rows <- output_df[!is.na(output_df$display_label) & nzchar(output_df$display_label), , drop = FALSE]

  list(
    summary = output_df,
    label_rows = label_rows,
    notes = notes[nzchar(notes)]
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

compute_half_max_ic50 <- function(params, model, direction, observed_dose, potency_metric = "IC50") {
  target_response <- potency_target_response(params, potency_metric)
  search_dose <- c(observed_dose, params$ic50_param)
  search_dose <- search_dose[is.finite(search_dose) & search_dose > 0]
  lower <- log10(max(min(search_dose, na.rm = TRUE) / 1000, .Machine$double.eps))
  upper <- log10(max(search_dose, na.rm = TRUE) * 1000)

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

estimate_ic50_uncertainty <- function(raw_group_df, group_name, fit_to, model, direction, weighting, curve_points, use_log10_axis, extend_curve_toward_zero, extra_log_decades, bootstrap_iterations, ic50_decimals = 2, potency_metric = "IC50", progress_step = NULL) {
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
        curve_points = curve_points,
        use_log10_axis = use_log10_axis,
        extend_curve_toward_zero = extend_curve_toward_zero,
        extra_log_decades = extra_log_decades,
        ic50_decimals = ic50_decimals,
        potency_metric = potency_metric
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

format_ic50_report <- function(ic50_value, uncertainty_method, uncertainty_values, decimals = 2) {
  if (!is.finite(ic50_value)) {
    return(NA_character_)
  }

  center <- format_decimal_text(ic50_value, decimals)

  if (identical(uncertainty_method, "None")) {
    return(center)
  }

  if (identical(uncertainty_method, "\u00b1 SD") && is.finite(uncertainty_values$ic50_sd)) {
    return(sprintf("%s \u00b1 %s", center, format_decimal_text(uncertainty_values$ic50_sd, decimals)))
  }

  if (identical(uncertainty_method, "\u00b1 SEM") && is.finite(uncertainty_values$ic50_sem)) {
    return(sprintf("%s \u00b1 %s", center, format_decimal_text(uncertainty_values$ic50_sem, decimals)))
  }

  if (identical(uncertainty_method, "95% CI") &&
      is.finite(uncertainty_values$ic50_ci95_low) &&
      is.finite(uncertainty_values$ic50_ci95_high)) {
    return(sprintf(
      "%s (95%% CI %s to %s)",
      center,
      format_decimal_text(uncertainty_values$ic50_ci95_low, decimals),
      format_decimal_text(uncertainty_values$ic50_ci95_high, decimals)
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

assess_ic50_reliability <- function(df, params, ic50_value, direction, ic50_decimals = 2, potency_metric = "IC50") {
  observed_min <- min(df$response, na.rm = TRUE)
  observed_max <- max(df$response, na.rm = TRUE)
  min_dose <- min(df$dose, na.rm = TRUE)
  max_dose <- max(df$dose, na.rm = TRUE)
  target_response <- potency_target_response(params, potency_metric)
  target_reason <- potency_target_reason(potency_metric)
  target_phrase <- potency_target_phrase(potency_metric)
  crosses_50_observed <- observed_min <= target_response && observed_max >= target_response

  interpretation <- if (is.finite(ic50_value)) format_decimal_text(ic50_value, ic50_decimals) else NA_character_
  reliability <- "Reliable"
  warning_text <- ""
  reason_flags <- character(0)

  if (!crosses_50_observed) {
    reliability <- reliability_target_not_reached
    reason_flags <- c(reason_flags, target_reason)
    if (identical(direction, "Increasing") && observed_max < target_response) {
      interpretation <- paste0(target_reason, " (> ", format_decimal_text(max_dose, ic50_decimals), ")")
      warning_text <- sprintf("Observed response never reaches the %s target; the fitted potency value is above the highest tested concentration.", target_phrase)
    } else if (identical(direction, "Decreasing") && observed_min > target_response) {
      interpretation <- paste0(target_reason, " (< ", format_decimal_text(min_dose, ic50_decimals), ")")
      warning_text <- sprintf("Observed response never drops to the %s target; the fitted potency value is below the lowest tested concentration.", target_phrase)
    } else {
      warning_text <- sprintf("Observed data do not span the %s target response level.", target_phrase)
    }
  }

  if (is.finite(ic50_value) && (ic50_value < min_dose || ic50_value > max_dose)) {
    if (identical(reliability, "Reliable")) {
      reliability <- reliability_extrapolated
      interpretation <- paste0("Extrapolated (", format_decimal_text(ic50_value, ic50_decimals), ")")
    }
    reason_flags <- c(reason_flags, "Value outside tested range")
    warning_text <- join_messages(
      warning_text,
      "The fitted potency value is outside the tested concentration range and should be treated as extrapolated."
    )
  }

  observed_span <- observed_max - observed_min
  fitted_span <- params$top - params$bottom
  if (!is.finite(fitted_span) || fitted_span <= 0) {
    reliability <- "Unreliable fit"
    reason_flags <- c(reason_flags, "Flat or invalid fitted range")
    warning_text <- join_messages(warning_text, "The fitted curve collapsed to a flat or invalid response range.")
  }

  if (is.finite(params$bottom) && params$bottom < observed_min - 25) {
    reliability <- "Unreliable fit"
    reason_flags <- c(reason_flags, "Bottom far below data")
    warning_text <- join_messages(warning_text, "The fitted bottom is far below the observed data.")
  }

  if (is.finite(params$top) && params$top > observed_max + max(25, 0.35 * max(observed_span, 1))) {
    reliability <- "Unreliable fit"
    reason_flags <- c(reason_flags, "Top far above data")
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
    reason_text = join_reason_flags(reason_flags),
    warning_text = warning_text,
    interpretation = interpretation
  )
}

fit_single_group <- function(df, group_name, model, direction, weighting, curve_points, use_log10_axis = TRUE, extend_curve_toward_zero = FALSE, extra_log_decades = 1, ic50_decimals = 2, potency_metric = "IC50") {
  if (identical(direction, "Auto-detect")) {
    direction_check <- detect_direction_mismatch(df$dose, df$response, "Increasing")
    candidate_directions <- unique(c(direction_check$expected_direction, "Increasing", "Decreasing"))
    fit_attempts <- lapply(candidate_directions, function(candidate_direction) {
      try(
        fit_single_group(
          df,
          group_name,
          model,
          candidate_direction,
          weighting,
          curve_points,
          use_log10_axis = use_log10_axis,
          extend_curve_toward_zero = extend_curve_toward_zero,
          extra_log_decades = extra_log_decades,
          ic50_decimals = ic50_decimals,
          potency_metric = potency_metric
        ),
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
  grid <- curve_grid_values(
    x = x,
    curve_points = curve_points,
    use_log10_axis = use_log10_axis,
    extend_curve_toward_zero = extend_curve_toward_zero,
    extra_log_decades = extra_log_decades
  )
  curve_values <- predict_curve(grid, params, model, direction)
  sse <- sum((y - fitted_values)^2)
  sst <- sum((y - mean(y))^2)
  r_squared <- if (sst > 0) 1 - sse / sst else NA_real_
  half_max_ic50 <- compute_half_max_ic50(params, model, direction, x, potency_metric = potency_metric)
  direction_check <- detect_direction_mismatch(x, y, direction)
  reliability <- assess_ic50_reliability(df, params, half_max_ic50, direction, ic50_decimals = ic50_decimals, potency_metric = potency_metric)
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
      potency_metric = potency_metric,
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
      fit_reason = reliability$reason_text,
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
    potency_metric = NA_character_,
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
    fit_reason = status_text,
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

status_report_numeric <- "Report numeric potency value"
status_not_reached <- "Do not report numeric value (target not reached)"
status_extrapolated <- "Do not report numeric value (extrapolated)"
status_review_fit <- "Numeric value shown; review fit"
status_no_fit <- "No fit available"
reliability_target_not_reached <- "Target response not reached"
reliability_extrapolated <- "Value outside tested range"

reporting_status_label <- function(fit_status, fit_reliability) {
  if (!identical(fit_status, "OK")) {
    return(status_no_fit)
  }

  if (identical(fit_reliability, "Reliable")) {
    return(status_report_numeric)
  }

  if (identical(fit_reliability, reliability_target_not_reached)) {
    return(status_not_reached)
  }

  if (identical(fit_reliability, reliability_extrapolated)) {
    return(status_extrapolated)
  }

  status_review_fit
}

reporting_note_label <- function(fit_status, fit_reliability, ic50_interpretation, fit_warning) {
  if (!identical(fit_status, "OK")) {
    return(fit_status)
  }

  if (identical(fit_reliability, "Reliable")) {
    return("")
  }

  if (identical(fit_reliability, reliability_target_not_reached) &&
      nzchar(ic50_interpretation %||% "")) {
    return(ic50_interpretation)
  }

  if (identical(fit_reliability, reliability_extrapolated) &&
      nzchar(ic50_interpretation %||% "")) {
    return(ic50_interpretation)
  }

  if (identical(fit_reliability, "Unreliable fit")) {
    return("Numeric IC50 shown, but inspect the fit before reporting it.")
  }

  fit_warning %||% ""
}

refresh_results_display <- function(result_df, uncertainty_method, decimals = 2, potency_metric = "IC50") {
  if (!nrow(result_df)) {
    return(result_df)
  }

  refreshed <- result_df

  refreshed$ic50_interpretation <- vapply(seq_len(nrow(refreshed)), function(i) {
    if (!identical(refreshed$fit_status[i], "OK")) {
      return(refreshed$ic50_interpretation[i] %||% NA_character_)
    }

    reliability <- refreshed$fit_reliability[i]
    direction <- refreshed$direction[i]

    if (identical(reliability, reliability_target_not_reached)) {
      if (identical(direction, "Increasing") && is.finite(refreshed$max_tested_concentration[i])) {
        return(paste0(potency_target_reason(potency_metric), " (> ", format_decimal_text(refreshed$max_tested_concentration[i], decimals), ")"))
      }
      if (identical(direction, "Decreasing") && is.finite(refreshed$min_tested_concentration[i])) {
        return(paste0(potency_target_reason(potency_metric), " (< ", format_decimal_text(refreshed$min_tested_concentration[i], decimals), ")"))
      }
    }

    if (identical(reliability, reliability_extrapolated) && is.finite(refreshed$ic50[i])) {
      return(paste0("Extrapolated (", format_decimal_text(refreshed$ic50[i], decimals), ")"))
    }

    refreshed$ic50_interpretation[i] %||% NA_character_
  }, character(1))

  refreshed$ic50_reported <- vapply(seq_len(nrow(refreshed)), function(i) {
    if (!identical(refreshed$fit_status[i], "OK")) {
      return(refreshed$ic50_reported[i] %||% NA_character_)
    }

    if (identical(refreshed$fit_reliability[i], reliability_target_not_reached)) {
      return(refreshed$ic50_interpretation[i] %||% NA_character_)
    }

    if (is.finite(refreshed$ic50[i])) {
      return(format_ic50_report(
        ic50_value = refreshed$ic50[i],
        uncertainty_method = uncertainty_method,
        uncertainty_values = list(
          ic50_sd = refreshed$ic50_sd[i],
          ic50_sem = refreshed$ic50_sem[i],
          ic50_ci95_low = refreshed$ic50_ci95_low[i],
          ic50_ci95_high = refreshed$ic50_ci95_high[i]
        ),
        decimals = decimals
      ))
    }

    refreshed$ic50_reported[i] %||% NA_character_
  }, character(1))

  refreshed$reporting_note <- vapply(seq_len(nrow(refreshed)), function(i) {
    reporting_note_label(
      fit_status = refreshed$fit_status[i],
      fit_reliability = refreshed$fit_reliability[i],
      ic50_interpretation = refreshed$ic50_interpretation[i],
      fit_warning = refreshed$fit_warning[i]
    )
  }, character(1))

  refreshed
}

analysis_status_levels <- c(
  status_report_numeric,
  status_not_reached,
  status_extrapolated,
  status_review_fit,
  status_no_fit
)

analysis_status_descriptions <- c(
  "Numeric potency value reportable",
  "Target not reached",
  "Value extrapolated",
  "Review fit before reporting",
  "No fit available"
)

summarize_analysis_results <- function(result_df) {
  counts <- table(factor(result_df$reporting_status, levels = analysis_status_levels))
  counts <- setNames(as.integer(counts), analysis_status_levels)

  fit_reasons <- result_df$fit_reason
  fit_reasons <- fit_reasons[!is.na(fit_reasons) & nzchar(fit_reasons)]
  reason_counts <- sort(table(fit_reasons), decreasing = TRUE)
  numeric_without_50_count <- sum(
    result_df$fit_status == "OK" &
      grepl("not reached", result_df$fit_reason %||% "", fixed = TRUE) &
      is.finite(result_df$ic50),
    na.rm = TRUE
  )

  list(
    counts = counts,
    reason_counts = reason_counts,
    numeric_without_50_count = numeric_without_50_count
  )
}

analysis_summary_modal <- function(summary_info, comparison = NULL, selected_model = NULL, potency_metric = "IC50") {
  status_items <- Map(function(status_label, display_label) {
    count_value <- summary_info$counts[[status_label]]
    if (!isTRUE(count_value > 0)) {
      return(NULL)
    }
    tags$li(sprintf("%s: %s", display_label, count_value))
  }, analysis_status_levels, analysis_status_descriptions)
  status_items <- Filter(Negate(is.null), status_items)

  reason_items <- NULL
  if (length(summary_info$reason_counts) > 0) {
    top_reasons <- head(summary_info$reason_counts, 5)
    reason_items <- lapply(seq_along(top_reasons), function(i) {
      tags$li(sprintf("%s: %s", names(top_reasons)[i], as.integer(top_reasons[[i]])))
    })
  }

  comparison_block <- NULL
  if (!is.null(comparison) && nzchar(comparison$recommended_model %||% "")) {
    recommended_row <- comparison$summary[comparison$summary$model == comparison$recommended_model, , drop = FALSE]
    comparison_block <- tagList(
      tags$h4("Best model from comparison"),
      tags$p(sprintf("Suggested model: %s", comparison$recommended_model)),
      if (nrow(recommended_row) == 1) {
        tags$ul(
          class = "analysis-summary-list",
          tags$li(sprintf("Reportable numeric %s values: %s", potency_metric_label(potency_metric), recommended_row$numeric_ic50_reportable[1])),
          tags$li(sprintf("Groups fit: %s", recommended_row$groups_fit[1])),
          tags$li(sprintf("Groups needing review: %s", recommended_row$review_before_reporting[1])),
          tags$li(sprintf("Median R-squared: %s", format_signif_text(recommended_row$median_r_squared[1], 3)))
        )
      },
      if (!is.null(selected_model) && !identical(selected_model, comparison$recommended_model)) {
        tags$div(
          class = "analysis-summary-tip",
          tags$strong("Current selected model: "),
          selected_model,
          ". Use the suggested-model button in the Model Comparison tab if you want to switch."
        )
      }
    )
  }

  modalDialog(
    title = "Analysis notes",
    easyClose = TRUE,
    size = "m",
    footer = modalButton("Close"),
    tags$p(sprintf("Review these points before reporting %s values from this run.", potency_metric_label(potency_metric))),
    tags$ul(class = "analysis-summary-list", status_items),
    if (isTRUE(summary_info$numeric_without_50_count > 0)) {
      tags$div(
        class = "analysis-summary-tip",
        tags$strong("Warning: "),
        sprintf(
          "%s group(s) still have a numeric %s value even though the observed data did not reach the target response. Treat those values cautiously.",
          summary_info$numeric_without_50_count
          ,
          potency_metric_label(potency_metric)
        )
      )
    },
    comparison_block,
    if (!is.null(reason_items)) tagList(
      tags$h4("Main fit reasons"),
      tags$ul(class = "analysis-summary-list", reason_items)
    ),
    tags$div(
      class = "analysis-summary-tip",
      tags$strong("Tip: "),
      sprintf("Use reporting_status to decide whether a numeric %s value should be reported, fit_reason for the short cause, and reporting_note for the longer explanation in the exported results.", potency_metric_label(potency_metric))
    )
  )
}

fit_dataset <- function(prepared, fit_to, model, direction, weighting, curve_points, use_log10_axis = TRUE, extend_curve_toward_zero = FALSE, extra_log_decades = 1, uncertainty_method = "None", bootstrap_iterations = 200, ic50_decimals = 2, potency_metric = "IC50", progress_callback = NULL) {
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
        curve_points = curve_points,
        use_log10_axis = use_log10_axis,
        extend_curve_toward_zero = extend_curve_toward_zero,
        extra_log_decades = extra_log_decades,
        ic50_decimals = ic50_decimals,
        potency_metric = potency_metric
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
        use_log10_axis = use_log10_axis,
        extend_curve_toward_zero = extend_curve_toward_zero,
        extra_log_decades = extra_log_decades,
        bootstrap_iterations = bootstrap_iterations,
        ic50_decimals = ic50_decimals,
        potency_metric = potency_metric,
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
        uncertainty_values = uncertainty_values,
        decimals = ic50_decimals
      )
    } else if (fits[[group_name]]$result_row$fit_status[1] == "OK") {
      fits[[group_name]]$result_row$ic50_reported <- format_ic50_report(
        ic50_value = fits[[group_name]]$result_row$ic50[1],
        uncertainty_method = uncertainty_method,
        uncertainty_values = empty_uncertainty(),
        decimals = ic50_decimals
      )
    }

    if (fits[[group_name]]$result_row$fit_status[1] == "OK" &&
        identical(fits[[group_name]]$result_row$fit_reliability[1], reliability_target_not_reached)) {
      fits[[group_name]]$result_row$ic50[1] <- NA_real_
      fits[[group_name]]$result_row$ic50_reported[1] <- fits[[group_name]]$result_row$ic50_interpretation[1]
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
  results_df$reporting_note <- vapply(
    seq_len(nrow(results_df)),
    function(i) reporting_note_label(
      fit_status = results_df$fit_status[i],
      fit_reliability = results_df$fit_reliability[i],
      ic50_interpretation = results_df$ic50_interpretation[i],
      fit_warning = results_df$fit_warning[i]
    ),
    character(1)
  )
  missing_display_mask <- results_df$fit_status == "OK" &
    (is.na(results_df$ic50_reported) | !nzchar(trimws(results_df$ic50_reported))) &
    is.finite(results_df$ic50)
  if (any(missing_display_mask)) {
    results_df$ic50_reported[missing_display_mask] <- vapply(
      results_df$ic50[missing_display_mask],
      function(x) format_decimal_text(x, ic50_decimals),
      character(1)
    )
  }
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
      sprintf(
        "No %s curve could be fit with the current grouping. Try selecting the compound/sample column as Group, or set Group/compound to None if your file contains only one series.",
        potency_metric_label(potency_metric)
      )
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
      paste0(potency_metric_label(potency_metric), " uncertainty reported as ", uncertainty_method, " using bootstrap resampling (n = ", bootstrap_iterations, ").")
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

compare_models <- function(prepared, fit_to, direction, weighting, curve_points, use_log10_axis = TRUE, extend_curve_toward_zero = FALSE, extra_log_decades = 1, ic50_decimals = 2, potency_metric = "IC50", progress_callback = NULL) {
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
        use_log10_axis = use_log10_axis,
        extend_curve_toward_zero = extend_curve_toward_zero,
        extra_log_decades = extra_log_decades,
        uncertainty_method = "None",
        bootstrap_iterations = 20,
        ic50_decimals = ic50_decimals,
        potency_metric = potency_metric,
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
    reliable_mask <- ok_mask & result_df$reporting_status == status_report_numeric
    not_reached_mask <- ok_mask & result_df$reporting_status == status_not_reached
    extrapolated_mask <- ok_mask & result_df$reporting_status == status_extrapolated
    review_mask <- ok_mask & result_df$reporting_status == status_review_fit
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
    "Suggested model: %s. This no-bootstrap comparison prioritizes reportable numeric %s values first, then successful fits, then median R-squared.",
    recommended_model,
    potency_metric_label(potency_metric)
  )

  if (nrow(recommended_row) == 1) {
    recommendation_text <- sprintf(
      "%s It gave %s reportable numeric %s value(s) across %s fitted group(s), with median R-squared %s.",
      recommendation_text,
      recommended_row$numeric_ic50_reportable[1],
      potency_metric_label(potency_metric),
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
    "All black" = rep("#000000", max(n, 1)),
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

publication_theme <- function(style_name, base_size, legend_position, background_fill, grid_mode = "Match style") {
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
        axis.line = element_blank(),
        axis.ticks = element_line(colour = "#111111", size = 0.8)
      )
  }

  effective_grid_mode <- if (identical(grid_mode, "Match style")) {
    if (identical(style_name, "Minimal clean")) "Both" else "None"
  } else {
    grid_mode
  }

  grid_theme <- switch(
    effective_grid_mode,
    "Horizontal" = theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#e5e7eb")
    ),
    "Vertical" = theme(
      panel.grid.major.x = element_line(colour = "#e5e7eb"),
      panel.grid.major.y = element_blank()
    ),
    "Both" = theme(
      panel.grid.major.x = element_line(colour = "#e5e7eb"),
      panel.grid.major.y = element_line(colour = "#e5e7eb")
    ),
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank()
    )
  )

  themed <- themed + grid_theme

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
      legend_content = "Points and lines",
      show_legend_title = TRUE,
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
      legend_content = "Points and lines",
      show_legend_title = TRUE,
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
      legend_content = "Points and lines",
      show_legend_title = TRUE,
      background_fill = "White",
      use_group_shapes = FALSE,
      show_half_max_line = FALSE,
      base_font_size = 15,
      point_size = 3.2,
      line_width = 1
    ),
    "Monochrome" = list(
      plot_style = "Classic axes",
      palette_name = "All black",
      legend_position = "Right",
      legend_content = "Points only",
      show_legend_title = FALSE,
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
  legend_name <- if (isTRUE(input$show_legend_title)) "Series" else NULL
  show_point_legend <- !identical(input$legend_content, "Lines only")
  show_line_legend <- !identical(input$legend_content, "Points only")

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
        size = input$point_size * 0.8,
        show.legend = FALSE
      )
    } else {
      p <- p + geom_point(
        data = prepared$raw,
        aes(x = dose, y = response, color = group),
        alpha = 0.45,
        size = input$point_size * 0.8,
        show.legend = FALSE
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
        size = max(0.3, input$line_width * 0.7),
        show.legend = FALSE
      )
    }
  }

  if (isTRUE(input$use_group_shapes)) {
    p <- p +
      geom_point(
        data = prepared$summary,
        aes(x = dose, y = response, color = group, shape = group),
        size = input$point_size,
        stroke = 0.8,
        show.legend = show_point_legend
      )
  } else {
    p <- p +
      geom_point(
        data = prepared$summary,
        aes(x = dose, y = response, color = group),
        size = input$point_size,
        stroke = 0.8,
        show.legend = show_point_legend
      )
  }

  if (nrow(fit_data$curves) > 0) {
    p <- p +
      geom_line(
        data = fit_data$curves,
        aes(x = dose, y = response, color = group),
        size = input$line_width,
        show.legend = show_line_legend
      )
  }

  if (isTRUE(input$show_ic50_guides)) {
    ic50_df <- fit_data$results[
      is.finite(fit_data$results$ic50) &
        fit_data$results$reporting_status == status_report_numeric,
      c("group", "ic50")
    ]
    if (nrow(ic50_df) > 0) {
      p <- p + geom_vline(
        data = ic50_df,
        aes(xintercept = ic50, color = group),
        linetype = "dashed",
        alpha = 0.5,
        show.legend = FALSE
      )
    }
  }

  if (isTRUE(input$show_half_max_line)) {
    p <- p + geom_hline(yintercept = 50, linetype = "dashed", size = 0.8, colour = "#9ca3af")
  }

  p <- p +
    scale_color_manual(values = palette_values, name = legend_name) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_label,
      y = y_label
    ) +
    publication_theme(
      style_name = input$plot_style,
      base_size = input$base_font_size,
      legend_position = input$legend_position,
      background_fill = input$background_fill,
      grid_mode = input$plot_grid_mode
    )

  if (isTRUE(input$use_group_shapes)) {
    p <- p + scale_shape_manual(values = shape_values, name = legend_name)
  }

  if (identical(input$legend_content, "Points only")) {
    p <- p + guides(color = guide_legend(order = 1), shape = guide_legend(order = 1))
  } else if (identical(input$legend_content, "Lines only")) {
    if (isTRUE(input$use_group_shapes)) {
      p <- p + guides(shape = "none", color = guide_legend(order = 1))
    } else {
      p <- p + guides(color = guide_legend(order = 1))
    }
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

build_bioassay_plot <- function(prepared, input) {
  raw_df <- prepared$raw
  summary_df <- apply_bioassay_summary_preferences(
    prepared$summary,
    statistic = input$bioassay_summary_stat,
    error_bars = input$bioassay_error_bars
  )
  label_info <- resolve_bioassay_label_data(prepared, summary_df, input)
  summary_df <- label_info$summary

  has_series <- !is.null(prepared$series_label) &&
    length(unique(as.character(raw_df$series))) > 1
  series_levels <- levels(raw_df$series)
  palette_values <- publication_palette(length(series_levels), input$palette_name)
  names(palette_values) <- series_levels
  legend_name <- if (has_series && isTRUE(input$show_legend_title)) prepared$series_label else NULL
  plot_type <- input$bioassay_plot_type %||% "Bar plot"

  plot_title <- trimws(input$bioassay_title)
  if (!nzchar(plot_title)) {
    plot_title <- "Other response plot"
  }
  if (!isTRUE(input$show_bioassay_title)) {
    plot_title <- NULL
  }

  x_label <- if (nzchar(trimws(input$bioassay_x_axis_title))) {
    trimws(input$bioassay_x_axis_title)
  } else {
    prepared$x_label
  }

  y_label <- if (nzchar(trimws(input$bioassay_y_axis_title))) {
    trimws(input$bioassay_y_axis_title)
  } else {
    prepared$y_label
  }

  label_rows <- label_info$label_rows

  y_range <- range(c(raw_df$y, summary_df$ymax, summary_df$summary_y), na.rm = TRUE)
  y_span <- diff(y_range)
  if (!is.finite(y_span) || y_span <= 0) {
    y_span <- max(abs(y_range[is.finite(y_range)]), 1)
  }
  label_offset <- y_span * 0.06
  summary_df$label_y <- row_max_finite(summary_df$summary_y, summary_df$ymax, summary_df$max) + label_offset
  label_rows$label_y <- row_max_finite(label_rows$summary_y, label_rows$ymax, label_rows$max) + label_offset
  show_label_rows <- nrow(label_rows) > 0

  p <- ggplot()

  if (identical(plot_type, "Bar plot")) {
    bar_position <- if (has_series) ggplot2::position_dodge(width = 0.78) else "identity"

    if (has_series) {
      p <- p + geom_col(
        data = summary_df,
        aes(x = x, y = summary_y, fill = series),
        width = 0.72,
        position = bar_position,
        colour = "#111111",
        size = 0.25
      )
    } else {
      p <- p + geom_col(
        data = summary_df,
        aes(x = x, y = summary_y),
        fill = palette_values[1],
        width = 0.72,
        colour = "#111111",
        size = 0.25,
        show.legend = FALSE
      )
    }

    error_df <- summary_df[is.finite(summary_df$ymin) & is.finite(summary_df$ymax), , drop = FALSE]
    if (nrow(error_df) > 0) {
      if (has_series) {
        p <- p + geom_errorbar(
          data = error_df,
          aes(x = x, ymin = ymin, ymax = ymax, colour = series, group = series),
          position = bar_position,
          width = 0.16,
          size = max(0.35, input$line_width * 0.75),
          show.legend = FALSE
        )
      } else {
        p <- p + geom_errorbar(
          data = error_df,
          aes(x = x, ymin = ymin, ymax = ymax),
          colour = "#111111",
          position = bar_position,
          width = 0.16,
          size = max(0.35, input$line_width * 0.75),
          show.legend = FALSE
        )
      }
    }

    if (isTRUE(input$bioassay_show_points)) {
      if (has_series) {
        p <- p + geom_point(
          data = raw_df,
          aes(x = x, y = y, colour = series),
          position = ggplot2::position_jitterdodge(jitter.width = 0.12, dodge.width = 0.78),
          alpha = 0.62,
          size = input$point_size * 0.72,
          show.legend = FALSE
        )
      } else {
        p <- p + geom_point(
          data = raw_df,
          aes(x = x, y = y),
          position = ggplot2::position_jitter(width = 0.12, height = 0),
          alpha = 0.62,
          size = input$point_size * 0.72,
          colour = "#111111",
          show.legend = FALSE
        )
      }
    }

    if (show_label_rows) {
      if (has_series) {
        p <- p + geom_text(
          data = label_rows,
          aes(x = x, y = label_y, label = display_label, group = series),
          position = bar_position,
          vjust = 0,
          size = input$bioassay_label_size,
          fontface = "bold",
          show.legend = FALSE
        )
      } else {
        p <- p + geom_text(
          data = label_rows,
          aes(x = x, y = label_y, label = display_label),
          vjust = 0,
          size = input$bioassay_label_size,
          fontface = "bold",
          show.legend = FALSE
        )
      }
    }
  } else if (identical(plot_type, "Boxplot")) {
    box_position <- if (has_series) ggplot2::position_dodge2(width = 0.78, preserve = "single") else "identity"

    if (has_series) {
      p <- p + geom_boxplot(
        data = raw_df,
        aes(x = x, y = y, fill = series),
        width = 0.72,
        position = box_position,
        outlier.shape = NA,
        alpha = 0.8,
        size = 0.35
      )
    } else {
      p <- p + geom_boxplot(
        data = raw_df,
        aes(x = x, y = y),
        fill = palette_values[1],
        width = 0.72,
        position = box_position,
        outlier.shape = NA,
        alpha = 0.8,
        size = 0.35,
        show.legend = FALSE
      )
    }

    if (isTRUE(input$bioassay_show_points)) {
      if (has_series) {
        p <- p + geom_point(
          data = raw_df,
          aes(x = x, y = y, colour = series),
          position = ggplot2::position_jitterdodge(jitter.width = 0.14, dodge.width = 0.78),
          alpha = 0.55,
          size = input$point_size * 0.7,
          show.legend = FALSE
        )
      } else {
        p <- p + geom_point(
          data = raw_df,
          aes(x = x, y = y),
          position = ggplot2::position_jitter(width = 0.14, height = 0),
          alpha = 0.55,
          size = input$point_size * 0.7,
          colour = "#111111",
          show.legend = FALSE
        )
      }
    }

    if (show_label_rows) {
      if (has_series) {
        p <- p + geom_text(
          data = label_rows,
          aes(x = x, y = label_y, label = display_label, group = series),
          position = ggplot2::position_dodge(width = 0.78),
          vjust = 0,
          size = input$bioassay_label_size,
          fontface = "bold",
          show.legend = FALSE
        )
      } else {
        p <- p + geom_text(
          data = label_rows,
          aes(x = x, y = label_y, label = display_label),
          vjust = 0,
          size = input$bioassay_label_size,
          fontface = "bold",
          show.legend = FALSE
        )
      }
    }
  } else {
    line_df <- if (prepared$x_is_numeric) {
      summary_df[order(summary_df$facet, summary_df$series, summary_df$x_numeric), , drop = FALSE]
    } else {
      summary_df[order(summary_df$facet, summary_df$series, summary_df$x_order), , drop = FALSE]
    }
    error_df <- line_df[is.finite(line_df$ymin) & is.finite(line_df$ymax), , drop = FALSE]
    error_width <- if (prepared$x_is_numeric) {
      x_range <- diff(range(raw_df$x_numeric, na.rm = TRUE))
      if (!is.finite(x_range) || x_range <= 0) 0.08 else x_range / 60
    } else {
      0.12
    }

    if (prepared$x_is_numeric) {
      if (has_series) {
        p <- p +
          geom_line(
            data = line_df,
            aes(x = x_numeric, y = summary_y, colour = series, group = series),
            size = input$line_width
          ) +
          geom_point(
            data = line_df,
            aes(x = x_numeric, y = summary_y, colour = series),
            size = input$point_size
          )
      } else {
        p <- p +
          geom_line(
            data = line_df,
            aes(x = x_numeric, y = summary_y, group = 1),
            size = input$line_width,
            colour = palette_values[1]
          ) +
          geom_point(
            data = line_df,
            aes(x = x_numeric, y = summary_y),
            size = input$point_size,
            colour = palette_values[1]
          )
      }

      if (nrow(error_df) > 0) {
        if (has_series) {
          p <- p + geom_errorbar(
            data = error_df,
            aes(x = x_numeric, ymin = ymin, ymax = ymax, colour = series),
            width = error_width,
            size = max(0.35, input$line_width * 0.7),
            show.legend = FALSE
          )
        } else {
          p <- p + geom_errorbar(
            data = error_df,
            aes(x = x_numeric, ymin = ymin, ymax = ymax),
            width = error_width,
            size = max(0.35, input$line_width * 0.7),
            colour = palette_values[1],
            show.legend = FALSE
          )
        }
      }

      if (isTRUE(input$bioassay_show_points)) {
        if (has_series) {
          p <- p + geom_point(
            data = raw_df,
            aes(x = x_numeric, y = y, colour = series),
            alpha = 0.45,
            size = input$point_size * 0.72,
            show.legend = FALSE
          )
        } else {
          p <- p + geom_point(
            data = raw_df,
            aes(x = x_numeric, y = y),
            alpha = 0.45,
            size = input$point_size * 0.72,
            colour = "#111111",
            show.legend = FALSE
          )
        }
      }

      if (show_label_rows) {
        if (has_series) {
          p <- p + geom_text(
            data = label_rows,
            aes(x = x_numeric, y = label_y, label = display_label),
            vjust = 0,
            size = input$bioassay_label_size,
            fontface = "bold",
            show.legend = FALSE
          )
        } else {
          p <- p + geom_text(
            data = label_rows,
            aes(x = x_numeric, y = label_y, label = display_label),
            vjust = 0,
            size = input$bioassay_label_size,
            fontface = "bold",
            show.legend = FALSE
          )
        }
      }
    } else {
      if (has_series) {
        p <- p +
          geom_line(
            data = line_df,
            aes(x = x, y = summary_y, colour = series, group = series),
            size = input$line_width
          ) +
          geom_point(
            data = line_df,
            aes(x = x, y = summary_y, colour = series),
            size = input$point_size
          )
      } else {
        p <- p +
          geom_line(
            data = line_df,
            aes(x = x, y = summary_y, group = 1),
            size = input$line_width,
            colour = palette_values[1]
          ) +
          geom_point(
            data = line_df,
            aes(x = x, y = summary_y),
            size = input$point_size,
            colour = palette_values[1]
          )
      }

      if (nrow(error_df) > 0) {
        if (has_series) {
          p <- p + geom_errorbar(
            data = error_df,
            aes(x = x, ymin = ymin, ymax = ymax, colour = series, group = series),
            width = error_width,
            size = max(0.35, input$line_width * 0.7),
            show.legend = FALSE
          )
        } else {
          p <- p + geom_errorbar(
            data = error_df,
            aes(x = x, ymin = ymin, ymax = ymax, group = 1),
            width = error_width,
            size = max(0.35, input$line_width * 0.7),
            colour = palette_values[1],
            show.legend = FALSE
          )
        }
      }

      if (isTRUE(input$bioassay_show_points)) {
        if (has_series) {
          p <- p + geom_point(
            data = raw_df,
            aes(x = x, y = y, colour = series),
            alpha = 0.45,
            size = input$point_size * 0.72,
            position = ggplot2::position_jitter(width = 0.08, height = 0),
            show.legend = FALSE
          )
        } else {
          p <- p + geom_point(
            data = raw_df,
            aes(x = x, y = y),
            alpha = 0.45,
            size = input$point_size * 0.72,
            position = ggplot2::position_jitter(width = 0.08, height = 0),
            colour = "#111111",
            show.legend = FALSE
          )
        }
      }

      if (show_label_rows) {
        if (has_series) {
          p <- p + geom_text(
            data = label_rows,
            aes(x = x, y = label_y, label = display_label),
            vjust = 0,
            size = input$bioassay_label_size,
            fontface = "bold",
            show.legend = FALSE
          )
        } else {
          p <- p + geom_text(
            data = label_rows,
            aes(x = x, y = label_y, label = display_label),
            vjust = 0,
            size = input$bioassay_label_size,
            fontface = "bold",
            show.legend = FALSE
          )
        }
      }
    }
  }

  if (has_series) {
    p <- p +
      scale_colour_manual(values = palette_values, name = legend_name) +
      scale_fill_manual(values = palette_values, name = legend_name)
  }

  p <- p +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label
    ) +
    publication_theme(
      style_name = input$plot_style,
      base_size = input$base_font_size,
      legend_position = if (has_series) input$legend_position else "None",
      background_fill = input$background_fill,
      grid_mode = input$plot_grid_mode
    ) +
    scale_y_continuous(
      labels = format_axis_labels,
      expand = ggplot2::expansion(mult = c(0.02, if (show_label_rows) 0.16 else 0.08))
    )

  if (identical(plot_type, "Line plot") && prepared$x_is_numeric) {
    if (isTRUE(input$bioassay_use_log10_x) && all(raw_df$x_numeric > 0, na.rm = TRUE)) {
      p <- p + scale_x_log10(labels = format_axis_labels)
    } else {
      p <- p + scale_x_continuous(labels = format_axis_labels)
    }
  } else {
    p <- p + scale_x_discrete(drop = FALSE)
  }

  x_levels <- levels(raw_df$x)
  if (length(x_levels) > 4 || any(nchar(x_levels) > 7)) {
    p <- p + theme(axis.text.x = element_text(angle = 25, hjust = 1))
  }

  if (length(levels(raw_df$facet)) > 1) {
    p <- p + facet_wrap(~facet)
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
      .run-bar {
        display: flex;
        align-items: center;
        gap: 14px;
        flex-wrap: wrap;
        background: linear-gradient(135deg, #fff9ef 0%, #f6efe3 100%);
        border: 1px solid #eadfca;
        border-radius: 16px;
        padding: 14px 16px;
        margin-bottom: 14px;
      }
      .run-button.btn-primary {
        min-width: 168px;
        min-height: 54px;
        border-radius: 16px;
        border: 1px solid #0c5f59;
        background: linear-gradient(180deg, #13877f 0%, #0f766e 100%);
        box-shadow: 0 10px 24px rgba(15, 118, 110, 0.16);
        font-size: 21px;
        font-weight: 700;
        padding: 10px 22px;
      }
      .run-button.btn-primary:hover,
      .run-button.btn-primary:focus {
        background: linear-gradient(180deg, #0f766e 0%, #0c5f59 100%);
        border-color: #0c5f59;
      }
      .run-copy {
        flex: 1 1 220px;
        min-width: 220px;
      }
      .run-copy-title {
        color: #102a43;
        font-weight: 700;
        margin-bottom: 2px;
      }
      .run-copy-text {
        color: #52606d;
        line-height: 1.45;
      }
      .note-block {
        background: #fff9ef;
        border-left: 5px solid #b45309;
        border-radius: 12px;
        padding: 12px 14px;
        margin-bottom: 14px;
      }
      .analysis-summary-list {
        padding-left: 18px;
        margin-bottom: 10px;
      }
      .analysis-summary-list li {
        margin-bottom: 6px;
      }
      .analysis-summary-tip {
        background: #fff9ef;
        border-radius: 12px;
        border: 1px solid #eadfca;
        padding: 10px 12px;
        margin-top: 8px;
      }
      .well {
        background: #fffdfa;
        border: 1px solid #eadfca;
        border-radius: 14px;
      }
      .sidebar-panel details {
        margin-bottom: 12px;
      }
      .sidebar-panel details > summary {
        cursor: pointer;
        font-weight: 700;
        color: #102a43;
        list-style: none;
      }
      .sidebar-panel details > summary::-webkit-details-marker {
        display: none;
      }
      .sidebar-panel details > summary::before {
        content: '+';
        display: inline-block;
        width: 14px;
        text-align: center;
        margin-right: 8px;
        color: #0f766e;
      }
      .sidebar-panel details[open] > summary::before {
        content: '-';
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
      .secondary-action {
        border-radius: 999px;
        border: 1px solid #d8c8ad;
        background: #fffdfa;
        color: #8b5e34;
        font-weight: 700;
      }
      .secondary-action:hover,
      .secondary-action:focus {
        background: #f8efe2;
        color: #7a4f28;
        border-color: #c9b18a;
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
    div(class = "app-title", "IC50 Studio"),
    div(
      class = "app-subtitle",
      "Load dose-response data, fit multiple sigmoid equations, calculate IC50 or EC50 values, and export publication-ready plots in an open workflow."
    ),
    sidebarLayout(
      sidebarPanel(
        class = "sidebar-panel",
        width = 4,
        div(
          class = "run-bar",
          actionButton("run_analysis", "Run analysis", class = "btn-primary run-button"),
          div(
            class = "run-copy",
            div(class = "run-copy-title", "Refresh fits, potency values, and warnings"),
            div(class = "run-copy-text", "Change settings, then run the analysis to update the curves, result table, and model suggestions.")
          )
        ),
        tags$details(
          class = "well",
          open = NA,
          tags$summary("Data and mapping"),
          br(),
          fileInput("data_file", "Upload data", accept = c(".csv", ".tsv", ".txt", ".xls", ".xlsx")),
          actionButton("load_example", "Use example dataset", class = "secondary-action"),
          br(), br(),
          uiOutput("sheet_ui"),
          uiOutput("mapping_ui")
        ),
        tags$details(
          class = "well",
          open = NA,
          tags$summary("Analysis settings"),
          br(),
          selectInput(
            "normalization",
            "Response scaling",
            choices = c(
              "Raw values",
              "Normalize 0 to 100 (min to max)",
              "Normalize 100 to 0 (max to min)",
              manual_control_normalization
            ),
            selected = "Raw values"
          ),
          conditionalPanel(
            condition = sprintf("input.normalization === '%s'", manual_control_normalization),
            numericInput("control_100_value", "100% control response", value = 100, step = 0.1),
            numericInput("control_0_value", "0% control response", value = 0, step = 0.1),
            helpText("Useful for absolute-style IC50 or EC50 analysis based on assay controls. Formula: 100 * (Y - control_0) / (control_100 - control_0).")
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
          selectInput(
            "model_equation",
            "Model",
            choices = all_model_choices,
            selected = "4PL"
          ),
          selectInput(
            "potency_metric",
            "Potency metric",
            choices = c("IC50", "EC50"),
            selected = "IC50"
          ),
          helpText("IC50 uses the response = 50 target. EC50 uses the half-max effect between the fitted bottom and top."),
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
            "Potency uncertainty",
            choices = c("95% CI", "\u00b1 SD", "\u00b1 SEM", "None"),
            selected = "None"
          ),
          numericInput("ic50_decimals", "Potency decimal places", value = 2, min = 0, max = 8, step = 1),
          numericInput("bootstrap_iterations", "Bootstrap iterations", value = 50, min = 20, max = 2000, step = 10),
          helpText("Use 100 to 200 bootstrap iterations for publication-oriented potency uncertainty. Reported potency values and \u00b1 uncertainty use the same number of decimals."),
          numericInput("curve_points", "Curve resolution", value = 250, min = 100, max = 1000, step = 50)
        ),
        tags$details(
          class = "well",
          tags$summary("Curve plot quick controls"),
          br(),
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
          checkboxInput("extend_curve_toward_zero", "Extend fitted curve toward zero-dose baseline", value = FALSE),
          numericInput("extra_log_decades", "Extra log10 decades below the minimum dose", value = 1, min = 0, max = 4, step = 0.25),
          checkboxInput("show_ic50_guides", "Show IC50 guide lines", value = TRUE),
          checkboxInput("show_half_max_line", "Show 50% reference line", value = TRUE)
        ),
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
            "plot_grid_mode",
            "Plot grid lines",
            choices = c("Match style", "None", "Horizontal", "Vertical", "Both"),
            selected = "Match style"
          ),
          selectInput(
            "palette_name",
            "Palette",
            choices = c("Bright contrast", "All black", "Colorblind safe", "Nature muted", "Black and gray", "Earth tones", "Viridis"),
            selected = "Bright contrast"
          ),
          selectInput(
            "legend_position",
            "Legend position",
            choices = c("Right", "Top", "Bottom", "Left", "None"),
            selected = "Right"
          ),
          selectInput(
            "legend_content",
            "Legend content",
            choices = c("Points and lines", "Points only", "Lines only"),
            selected = "Points and lines"
          ),
          checkboxInput("show_legend_title", "Show legend title", value = TRUE),
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
          tags$summary("Other plots module"),
          br(),
          selectInput(
            "bioassay_plot_type",
            "Other plot type",
            choices = c("Bar plot", "Boxplot", "Line plot"),
            selected = "Bar plot"
          ),
          uiOutput("bioassay_mapping_ui"),
          conditionalPanel(
            condition = "input.bioassay_plot_type !== 'Boxplot'",
            selectInput(
              "bioassay_summary_stat",
              "Summary value",
              choices = c("Mean", "Median"),
              selected = "Mean"
            ),
            selectInput(
              "bioassay_error_bars",
              "Error bars",
              choices = c("SEM", "SD", "95% CI", "IQR", "None"),
              selected = "SEM"
            )
          ),
          checkboxInput("bioassay_show_points", "Overlay raw points", value = TRUE),
          selectInput(
            "bioassay_label_mode",
            "Label mode",
            choices = c("None", "Annotation column", "Auto significance letters (bar plot)"),
            selected = "None"
          ),
          conditionalPanel(
            condition = "input.bioassay_label_mode === 'Auto significance letters (bar plot)' && input.bioassay_plot_type === 'Bar plot'",
            selectInput(
              "bioassay_letters_method",
              "Letter test",
              choices = c("ANOVA + Tukey HSD", "Kruskal + pairwise Wilcoxon"),
              selected = "ANOVA + Tukey HSD"
            ),
            selectInput(
              "bioassay_letters_scope",
              "Compare bars",
              choices = c(
                "Across all bars in each facet",
                "Within each series in each facet",
                "Within each x group in each facet"
              ),
              selected = "Within each x group in each facet"
            ),
            numericInput("bioassay_letters_alpha", "Letter alpha", value = 0.05, min = 0.001, max = 0.2, step = 0.005),
            helpText("Letters are calculated from the raw replicate rows. For grouped bar figures like peptide x concentration, the usual choice is 'Within each x group in each facet' so concentrations are compared within each peptide. Use this only when the replicates are independent biological observations, not only technical repeats.")
          ),
          checkboxInput("bioassay_use_log10_x", "Use log10 x-axis when the selected x column is numeric", value = FALSE),
          numericInput("bioassay_label_size", "Letter / annotation size", value = 5, min = 2, max = 12, step = 0.25),
          textInput("bioassay_title", "Other plot title", value = "Other response plot"),
          checkboxInput("show_bioassay_title", "Show other-plot title", value = TRUE),
          textInput("bioassay_x_axis_title", "Other plot x-axis title", value = ""),
          textInput("bioassay_y_axis_title", "Other plot y-axis title", value = ""),
          helpText("For inhibitor-style grouped bars like the example image, map X = compound or peptide, Y = response, Series = concentration, and either choose an annotation column or turn on automatic significance letters.")
        ),
        tags$details(
          class = "well",
          tags$summary("Curve plot axis breaks and limits"),
          br(),
          textInput("x_breaks", "Custom x breaks", value = "", placeholder = "e.g. 1.56, 6.25, 25, 100, 400"),
          textInput("y_breaks", "Custom y breaks", value = "", placeholder = "e.g. 0, 25, 50, 75, 100"),
          textInput("x_limits", "X-axis limits", value = "", placeholder = "e.g. 0.5, 400"),
          textInput("y_limits", "Y-axis limits", value = "", placeholder = "e.g. 0, 100")
        ),
        tags$details(
          class = "well",
          tags$summary("Export settings (all plots)"),
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
            DTOutput("summary_data_table"),
            br(),
            h4("Other-plots summary"),
            DTOutput("bioassay_summary_table")
          ),
          tabPanel(
            "Other Plots",
            br(),
            div(class = "note-block", textOutput("bioassay_notes")),
            plotOutput("bioassay_plot", height = "620px"),
            br(),
            downloadButton("download_bioassay_plot", "Download other plot"),
            downloadButton("download_bioassay_summary", "Download other-plot summary CSV")
          ),
          tabPanel(
            "Instructions",
            br(),
            h4("Quick start"),
            tags$p("1. Upload your file or use the example dataset."),
            tags$p("2. Map the concentration or dose column, the response column, and an optional group column such as compound or sample."),
            tags$p("3. Leave Curve direction on Auto-detect unless you have a strong reason to force Increasing or Decreasing."),
            tags$p("4. Choose a model, or turn on 'Compare all models first (no bootstrap)' to get a fast suggestion."),
            tags$p("5. Click Run analysis."),
            tags$p("6. For final reporting, turn on IC50 uncertainty only after you are happy with the model and plot."),
            br(),
            h4("Other plots module"),
            tags$p("Use the Other Plots tab when you want assay-style figures without fitting a sigmoid model."),
            tags$p("Typical mappings are X = peptide or treatment, Y = response, Series = concentration or condition, and Optional annotation = significance letters or compact letter displays."),
            tags$p("Bar plot is useful for grouped inhibitor summaries, Boxplot is useful for replicate distributions, and Line plot is useful for time-course or concentration-course summaries."),
            tags$p("The other-plots module uses the same uploaded file, styling palette, and export settings as the curve plot tab, but it has its own x/y/series mapping and summary controls."),
            tags$p("Bar plots can now add automatic significance letters from ANOVA plus Tukey HSD or Kruskal plus pairwise Wilcoxon comparisons. Use these letters only when your rows are independent biological replicates, not only technical repeats."),
            br(),
            h4("Recommended workflow"),
            tags$p("Start with Group means and IC50 uncertainty = None."),
            tags$p("If you are not sure which equation to use, enable model comparison first. The app will suggest a model based on how many groups give reportable IC50 values and how well the curves fit."),
            tags$p("Once you choose the model, run the final fit again with bootstrap uncertainty if you want 95% CI, SD, or SEM."),
            tags$p("Use the plot controls only after the fitting looks right. Styling should be the last step, not the first."),
            tags$p("If you want the displayed sigmoid to continue toward the zero-dose baseline even when you did not measure a zero point, turn on 'Extend fitted curve toward zero-dose baseline'."),
            br(),
            h4("Data format"),
            tags$p("Supported files: CSV, TSV, TXT, XLS, XLSX."),
            tags$p("Recommended columns: one numeric concentration or dose column, one numeric response column, and an optional grouping column such as compound, sample, treatment, or replicate set."),
            tags$p("Concentration values must be greater than zero if you want a log-scale x axis or a standard IC50 fit."),
            tags$p("The app does not assume a fixed unit. You can use nM, uM, ug/mL, mg/mL, or any other unit as long as the concentration column is numeric, then write the exact unit in the x-axis title."),
            tags$p("If you want GraphPad-like absolute normalization, choose 'Normalize using manual 0% and 100% controls' and enter the assay control responses used to define 100% and 0%."),
            tags$p("For the other-plots module, the x column can be categorical or numeric, the y column should be numeric, and the optional annotation column can contain letters or short labels to place above bars, boxes, or line points."),
            br(),
            h4("Choosing a model"),
            tags$p("4PL is a good general starting point when both lower and upper plateaus are visible."),
            tags$p("3PL (Hill fixed = 1) is useful when you want a simpler fixed-slope model."),
            tags$p("3PL (Bottom = 0) is useful when the response should start near 0."),
            tags$p("3PL (Top = 100) is useful when the response should approach 100."),
            tags$p("5PL allows asymmetry, but the reported IC50 is solved from the fitted curve and can differ from the midpoint parameter."),
            br(),
            h4("Interpreting the results table"),
            tags$p("Green rows mean the app considers the IC50 suitable to report as a numeric value."),
            tags$p("Yellow rows mean the data did not reach 50%, so you should report the result as not reaching 50% instead of giving a numeric IC50."),
            tags$p("Orange rows mean the fitted IC50 is outside the tested range and should be treated as extrapolated."),
            tags$p("Red rows mean the fit needs review before reporting."),
            tags$p("Gray rows mean the app could not fit the curve."),
            tags$p("The fit_reason column gives the short explanation for each flagged row, while reporting_note keeps the longer sentence for export and deeper review."),
            br(),
            h4("Understanding fit_reason"),
            tags$p("Top far above data: the fitted upper plateau is much higher than the observed points, so a flexible model such as 4PL is extrapolating beyond the measured range."),
            tags$p("Bottom far below data: the fitted lower plateau falls well below the observed low-concentration responses."),
            tags$p("Target not reached: the observed data do not cross the response target used for the selected potency metric, so the app does not report a numeric value."),
            tags$p("Value outside tested range: the fitted potency value lies outside the concentration range you actually tested."),
            tags$p("Flat or invalid fitted range: the model collapsed to a nearly flat or otherwise implausible curve."),
            tags$p("If several groups are flagged for top or bottom problems, try a simpler model such as 3PL or expand the tested concentration range."),
            br(),
            h4("Uncertainty"),
            tags$p("95% CI is usually the best option for publication because it communicates uncertainty around the fitted parameter."),
            tags$p("SD and SEM in this app come from bootstrap resampling of IC50, not from the raw response standard deviation at each concentration."),
            tags$p("For speed, keep uncertainty off during exploration. Around 50 bootstrap iterations is a quick preview, and 100 to 200 is a more reasonable starting point for final reporting."),
            br(),
            h4("After each run"),
            tags$p("When the analysis finishes, the app can show a short summary window if any groups need attention."),
            tags$p("Use reporting_status to decide whether a numeric IC50 should be reported, fit_reason for the short cause, and reporting_note when you want the longer explanation."),
            br(),
            h4("Troubleshooting"),
            tags$p("If the fit looks flat or wrong, first check the selected group column and curve direction."),
            tags$p("If the app reports that 50% was not reached, expand the concentration range instead of forcing a numeric IC50."),
            tags$p("For enzyme inhibition-style figures, use Response transform = 'Invert as 100 - response', Increasing direction, and a 0 to 100 y-axis."),
            tags$p("On log-scale plots, the zero-dose extension works by drawing the curve for extra log10 decades below the minimum observed concentration, because zero itself cannot be shown on a log axis."),
            h4("Example layout"),
            DTOutput("example_table")
          ),
          tabPanel(
            "About",
            br(),
            h3("IC50 Studio"),
            tags$p("IC50 Studio is an open R Shiny app for dose-response fitting, IC50 interpretation, and publication-oriented figure export."),
            br(),
            h4("Developed by"),
            tags$p(tags$strong("Dr. Ivan Sanchis")),
            br(),
            h4("Affiliation"),
            tags$p("Laboratory of Bioactive Peptides (LPB)"),
            tags$p("Faculty of Biochemistry and Biological Sciences (FBCB)"),
            tags$p("National University of the Littoral (UNL)"),
            tags$p("Santa Fe, Argentina"),
            br(),
            h4("Contact"),
            tags$p(
              "Institutional email: ",
              tags$a("sanchisivan@fbcb.unl.edu.ar", href = "mailto:sanchisivan@fbcb.unl.edu.ar")
            ),
            tags$p(
              "Personal email: ",
              tags$a("sanchisivan@gmail.com", href = "mailto:sanchisivan@gmail.com")
            ),
            br(),
            h4("Repository"),
            tags$p(
              tags$a(
                "github.com/sanchisivan/ic50-studio",
                href = "https://github.com/sanchisivan/ic50-studio",
                target = "_blank"
              )
            ),
            br(),
            h4("License"),
            tags$p("MIT License. You can use, modify, and redistribute the software with attribution. See the LICENSE file in the repository for the full text.")
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
    updateSelectInput(session, "legend_content", selected = preset_values$legend_content)
    updateSelectInput(session, "background_fill", selected = preset_values$background_fill)
    updateCheckboxInput(session, "show_legend_title", value = preset_values$show_legend_title)
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

  output$bioassay_mapping_ui <- renderUI({
    df <- current_data()
    nm <- names(df)
    req(length(nm) > 0)

    numeric_cols <- nm[vapply(df, is_numericish_column, logical(1))]
    y_choices <- if (length(numeric_cols)) numeric_cols else nm
    plot_type <- input$bioassay_plot_type %||% "Bar plot"

    default_x <- guess_bioassay_x_column(
      data = df,
      plot_type = plot_type,
      dose_col = input$dose_col %||% NULL,
      group_col = input$group_col %||% NULL
    )
    default_y <- input$response_col %||%
      guess_column(df, c("response", "signal", "viability", "inhibition", "activity", "effect", "hemol"), y_choices[1] %||% nm[1])
    default_series <- guess_bioassay_series_column(
      data = df,
      plot_type = plot_type,
      x_col = default_x,
      dose_col = input$dose_col %||% NULL,
      group_col = input$group_col %||% NULL
    )
    selected_x <- isolate(input$bioassay_x_col)
    if (is.null(selected_x) || !selected_x %in% nm) {
      selected_x <- default_x
    }

    selected_y <- isolate(input$bioassay_y_col)
    if (is.null(selected_y) || !selected_y %in% y_choices) {
      selected_y <- if (!is.null(default_y) && default_y %in% y_choices) default_y else y_choices[1]
    }

    selected_series <- isolate(input$bioassay_series_col)
    if (is.null(selected_series) || (!identical(selected_series, "None") && !selected_series %in% nm)) {
      selected_series <- if (!is.null(default_series) && default_series %in% nm) default_series else "None"
    }

    selected_facet <- isolate(input$bioassay_facet_col)
    if (is.null(selected_facet) || (!identical(selected_facet, "None") && !selected_facet %in% nm)) {
      selected_facet <- "None"
    }

    selected_label <- isolate(input$bioassay_label_col)
    if (is.null(selected_label) || (!identical(selected_label, "None") && !selected_label %in% nm)) {
      selected_label <- "None"
    }

    tagList(
      h4("Other plot column mapping"),
      selectInput(
        "bioassay_x_col",
        "X column",
        choices = nm,
        selected = selected_x
      ),
      selectInput(
        "bioassay_y_col",
        "Y column",
        choices = y_choices,
        selected = selected_y
      ),
      selectInput(
        "bioassay_series_col",
        "Series / color column",
        choices = c("None", nm),
        selected = selected_series
      ),
      selectInput(
        "bioassay_facet_col",
        "Facet column",
        choices = c("None", nm),
        selected = selected_facet
      ),
      selectInput(
        "bioassay_label_col",
        "Optional annotation column",
        choices = c("None", nm),
        selected = selected_label
      )
    )
  })

  bioassay_result <- reactive({
    req(input$bioassay_x_col, input$bioassay_y_col)

    prepare_bioassay_dataset(
      data = current_data(),
      x_col = input$bioassay_x_col,
      y_col = input$bioassay_y_col,
      series_col = input$bioassay_series_col %||% "None",
      facet_col = input$bioassay_facet_col %||% "None",
      label_col = input$bioassay_label_col %||% "None"
    )
  })

  bioassay_display <- reactive({
    summary_df <- apply_bioassay_summary_preferences(
      bioassay_result()$summary,
      statistic = input$bioassay_summary_stat,
      error_bars = input$bioassay_error_bars
    )

    resolve_bioassay_label_data(
      prepared = bioassay_result(),
      summary_df = summary_df,
      input = input
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
        response_transform = input$response_transform,
        control_100 = input$control_100_value %||% NA_real_,
        control_0 = input$control_0_value %||% NA_real_
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
          use_log10_axis = input$use_log10_axis,
          extend_curve_toward_zero = input$extend_curve_toward_zero,
          extra_log_decades = input$extra_log_decades,
          ic50_decimals = input$ic50_decimals,
          potency_metric = input$potency_metric,
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
          use_log10_axis = input$use_log10_axis,
          extend_curve_toward_zero = input$extend_curve_toward_zero,
          extra_log_decades = input$extra_log_decades,
          uncertainty_method = input$ic50_uncertainty,
          bootstrap_iterations = input$bootstrap_iterations,
          ic50_decimals = input$ic50_decimals,
          potency_metric = input$potency_metric,
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

  observeEvent(analysis_result(), ignoreInit = TRUE, {
    result_df <- analysis_result()$fit$results
    summary_info <- summarize_analysis_results(result_df)
    comparison_result <- analysis_result()$comparison
    issue_total <- sum(summary_info$counts[names(summary_info$counts) != status_report_numeric])

    if (issue_total > 0 || !is.null(comparison_result)) {
      showModal(analysis_summary_modal(
        summary_info = summary_info,
        comparison = comparison_result,
        selected_model = input$model_equation,
        potency_metric = input$potency_metric
      ))
    } else {
      showNotification(
        sprintf("Analysis complete. All fitted groups are currently reportable as numeric %s values.", potency_metric_label(input$potency_metric)),
        type = "message",
        duration = 5
      )
    }
  })

  output$data_notes <- renderText({
    comparison_note <- NULL
    if (!is.null(analysis_result()$comparison)) {
      comparison_note <- analysis_result()$comparison$recommendation_text
    }

    note_parts <- c(analysis_result()$prepared$notes, analysis_result()$fit$message, comparison_note)
    note_parts <- note_parts[nzchar(note_parts)]
    paste(note_parts, collapse = " | ")
  })

  output$bioassay_notes <- renderText({
    note_parts <- c(bioassay_result()$notes, bioassay_display()$notes)

    if (identical(input$bioassay_plot_type, "Line plot") &&
        isTRUE(input$bioassay_use_log10_x)) {
      if (!bioassay_result()$x_is_numeric) {
        note_parts <- c(note_parts, "Log10 x-axis is available only when the selected other-plot x column is numeric.")
      } else if (any(bioassay_result()$raw$x_numeric <= 0, na.rm = TRUE)) {
        note_parts <- c(note_parts, "Log10 x-axis was skipped because the selected other-plot x column contains zero or negative values.")
      }
    }

    paste(note_parts, collapse = " | ")
  })

  output$equation_used <- renderText({
    equation_label(input$model_equation, input$direction)
  })

  output$dose_plot <- renderPlot({
    build_plot(analysis_result()$prepared, analysis_result()$fit, input)
  }, res = 130)

  output$bioassay_plot <- renderPlot({
    build_bioassay_plot(bioassay_result(), input)
  }, res = 130)

  output$fit_results_table <- renderDT({
    result_df <- refresh_results_display(
      analysis_result()$fit$results,
      uncertainty_method = input$ic50_uncertainty,
      decimals = input$ic50_decimals,
      potency_metric = input$potency_metric
    )
    result_df$needs_50_crossing_warning <- with(
      result_df,
      fit_status == "OK" &
        grepl("not reached", ifelse(is.na(fit_reason), "", fit_reason), fixed = TRUE) &
        is.finite(ic50)
    )
    ic50_numeric_cols <- intersect(
      c("ic50", "midpoint_parameter", "ic50_sd", "ic50_sem", "ic50_ci95_low", "ic50_ci95_high"),
      names(result_df)
    )
    other_numeric_cols <- setdiff(names(result_df)[vapply(result_df, is.numeric, logical(1))], ic50_numeric_cols)
    result_df[ic50_numeric_cols] <- lapply(result_df[ic50_numeric_cols], function(x) round(x, input$ic50_decimals))
    result_df[other_numeric_cols] <- lapply(result_df[other_numeric_cols], function(x) round(x, 4))

    display_order <- c(
      "group",
      "model",
      "direction",
      "ic50",
      "ic50_reported",
      "reporting_status",
      "fit_reliability",
      "fit_reason",
      "suggested_direction",
      "r_squared",
      "n_points",
      "fit_status"
    )
    display_cols <- intersect(display_order, names(result_df))
    hidden_cols <- setdiff(names(result_df), display_cols)
    visible_df <- result_df[, c(display_cols, hidden_cols), drop = FALSE]
    names(visible_df) <- rename_potency_column_names(names(visible_df), input$potency_metric)
    potency_value_col <- potency_metric_lower(input$potency_metric)
    potency_reported_col <- paste0(potency_value_col, "_reported")
    hidden_display_cols <- rename_potency_column_names(hidden_cols, input$potency_metric)

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
          list(visible = FALSE, targets = match(hidden_display_cols, names(visible_df)) - 1)
        )
      )
    )

    dt <- formatStyle(
      dt,
      "reporting_status",
      target = "row",
      backgroundColor = styleEqual(
        c(
          status_report_numeric,
          status_not_reached,
          status_extrapolated,
          status_review_fit,
          status_no_fit
        ),
        c("#eefbf3", "#fff7db", "#fff0d6", "#fdecec", "#f3f4f6")
      )
    )
    dt <- formatStyle(dt, "reporting_status", fontWeight = "700")
    dt <- formatStyle(
      dt,
      "group",
      valueColumns = "needs_50_crossing_warning",
      borderLeft = styleEqual(c(TRUE, FALSE), c("6px solid #d97706", "")),
      fontWeight = styleEqual(c(TRUE, FALSE), c("700", "400"))
    )
    dt <- formatStyle(
      dt,
      c(potency_value_col, potency_reported_col),
      valueColumns = "needs_50_crossing_warning",
      backgroundColor = styleEqual(c(TRUE, FALSE), c("#fff1c2", "")),
      color = styleEqual(c(TRUE, FALSE), c("#9a3412", "")),
      fontWeight = styleEqual(c(TRUE, FALSE), c("700", "400"))
    )
    dt
  })

  output$model_comparison_summary <- renderUI({
    comparison <- analysis_result()$comparison

    if (is.null(comparison)) {
      return(tags$p("Turn on 'Compare all models first (no bootstrap)' and click Run analysis to benchmark the equations before choosing one for bootstrap uncertainty."))
    }

    tagList(
      tags$p(comparison$recommendation_text),
      tags$p(sprintf("Bootstrap is not used in this comparison. If you want uncertainty, apply or choose the suggested model and run the analysis again with %s uncertainty enabled.", potency_metric_label(input$potency_metric)))
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

  output$bioassay_summary_table <- renderDT({
    display_df <- bioassay_display()$summary
    numeric_cols <- vapply(display_df, is.numeric, logical(1))
    display_df[numeric_cols] <- lapply(display_df[numeric_cols], function(x) round(x, 4))

    datatable(
      display_df,
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
      paste0("dose_response_", potency_metric_lower(input$potency_metric), "_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_df <- refresh_results_display(
        analysis_result()$fit$results,
        uncertainty_method = input$ic50_uncertainty,
        decimals = input$ic50_decimals,
        potency_metric = input$potency_metric
      )
      names(export_df) <- rename_potency_column_names(names(export_df), input$potency_metric)
      utils::write.csv(export_df, file, row.names = FALSE)
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

  output$download_bioassay_plot <- downloadHandler(
    filename = function() {
      base_name <- trimws(input$export_filename)
      if (!nzchar(base_name)) {
        base_name <- paste0("other_plot_", Sys.Date())
      } else {
        base_name <- paste0(base_name, "_other_plot")
      }
      paste0(base_name, ".", plot_export_extension(input$export_format))
    },
    content = function(file) {
      ggplot2::ggsave(
        filename = file,
        plot = build_bioassay_plot(bioassay_result(), input),
        device = plot_export_device(input$export_format),
        width = input$export_width,
        height = input$export_height,
        units = input$export_units,
        dpi = input$export_dpi,
        bg = resolve_plot_fill(input$background_fill)
      )
    }
  )

  output$download_bioassay_summary <- downloadHandler(
    filename = function() {
      paste0("other_plot_summary_", Sys.Date(), ".csv")
    },
    content = function(file) {
      export_df <- bioassay_display()$summary
      utils::write.csv(export_df, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

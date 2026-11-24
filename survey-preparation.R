# Source this script from the project directory. No files are read or written on source().
# All numeric codes below are canonical analysis codes, independent of Qualtrics recodes.
rating_groups <- list(
  message_credibility = paste0("MC", 1:3),
  provider_trust = paste0("TR", 1:3),
  professionalism = paste0("PR", 1:3),
  consultation_intention = paste0("CI", 1:3)
)
rating_ids <- c(paste0("IQ", 1:4), unlist(rating_groups, use.names = FALSE))
agreement <- c("Strongly disagree" = 1, "Disagree" = 2,
               "Neither agree nor disagree" = 3, "Agree" = 4, "Strongly agree" = 5)
describes <- c("Not at all" = 1, "Slightly" = 2, "Moderately" = 3,
               "Very" = 4, "Extremely" = 5)
category_labels <- list(
  C0 = c("Yes", "No"), S1 = c("Yes", "No", "Prefer not to answer"),
  S2 = c("Myself only", "My child only", "Both", "Neither", "Prefer not to answer"),
  S2a = c("Self", "Child"),
  S3 = c("San Gabriel", "Chino Hills", "Another city", "Prefer not to answer"),
  Q1 = c("Within 3 months", "4–6 months", "7–12 months", "More than 12 months",
         "Undecided", "Already chosen", "Prefer not to answer"),
  Q3 = c("English", "Spanish", "Chinese", "Another language", "No preference",
         "Prefer not to answer"),
  Q3a = c("Simplified Chinese", "Traditional Chinese", "Either", "Not sure"),
  C1 = c("Never heard of it", "A little familiar", "Moderately familiar",
         "Very familiar", "Not sure", "Prefer not to answer"),
  Q4 = c("Reviewed", "Could not view clearly", "Visible but not reviewed",
         "Prefer not to answer"),
  C2 = c("Yes", "No", "Not sure", "Prefer not to answer")
)

read_authorized_sav <- function(path, authorized = FALSE) {
  if (!isTRUE(authorized)) stop("Verify protocol and export authorization before import.")
  haven::read_sav(path, user_na = TRUE)
}

inspect_metadata <- function(raw) {
  data.frame(
    export_name = names(raw), storage_type = vapply(raw, typeof, ""),
    variable_label = vapply(raw, function(x) paste(attr(x, "label"), collapse = ""), ""),
    value_labels = vapply(raw, function(x) {
      labels <- attr(x, "labels")
      paste(paste(names(labels), unname(labels), sep = "="), collapse = " | ")
    }, ""),
    effective_missing = vapply(raw, function(x) sum(is.na(haven::zap_missing(x)) |
      (is.character(x) & trimws(as.character(x)) == ""), na.rm = TRUE), integer(1))
  )
}

# column_map maps every supported logical item to its verified export column.
# code_maps maps each logical item to a named canonical-code vector.
# Names are raw values, e.g. c("10" = 1, "99" = 6) only when the export verifies that mapping.
# Explicit codes may map SPSS user-defined missing markers to the statuses below.
prepare_core <- function(raw, column_map, code_maps, provenance,
                         authorized = FALSE, synthetic = FALSE,
                         dictionary_path = "survey-variable-dictionary.csv") {
  if (!isTRUE(authorized) && !isTRUE(synthetic)) stop("Authorization required.")
  allowed_provenance <- if (synthetic) "synthetic" else "authorized_research"
  if (length(provenance) != nrow(raw)) stop("Provide one reviewed provenance status per row.")
  keep <- !is.na(provenance) & provenance == allowed_provenance
  raw_kept <- raw[keep, , drop = FALSE]
  ids <- c(names(category_labels), rating_ids)
  if (!all(ids %in% names(column_map)) || !all(ids %in% names(code_maps)) ||
      !all(column_map[ids] %in% names(raw_kept)) || anyDuplicated(column_map[ids])) {
    stop("Supply verified, distinct export columns and code maps for all core items.")
  }
  dictionary <- read.csv(dictionary_path, stringsAsFactors = FALSE)
  label_for <- setNames(dictionary$variable_label, dictionary$logical_id)
  measure_for <- setNames(dictionary$measurement, dictionary$logical_id)

  decode <- function(id, valid) {
    x <- raw_kept[[column_map[[id]]]]
    original <- as.vector(unclass(x))
    keys <- trimws(as.character(original))
    map <- code_maps[[id]]
    if (!length(map) || is.null(names(map)) || anyNA(map) ||
        anyDuplicated(names(map)) || !all(map %in% c(valid, -1L, -2L))) {
      stop("Invalid code map for ", id)
    }
    code <- as.integer(unname(map[keys]))
    unknown <- !is.na(keys) & nzchar(keys) & is.na(code)
    if (any(unknown)) stop("Unmapped nonblank values in ", id)
    code
  }
  label_item <- function(code, labels, id, scale = NULL) {
    x <- haven::labelled(code, labels = labels, label = unname(label_for[id]))
    attr(x, "measurement_scale") <- if (is.null(scale)) unname(measure_for[id]) else scale
    x
  }
  out <- dplyr::tibble(source_row = which(keep))
  for (id in names(category_labels)) {
    choices <- category_labels[[id]]
    code <- decode(id, seq_along(choices))
    refusal_code <- match("Prefer not to answer", choices, nomatch = 0L)
    out[[paste0(id, "_status")]] <- dplyr::case_when(
      is.na(code) ~ "missing",
      code == -1 ~ "not_applicable",
      code == -2 ~ "prefer_not_to_answer",
      code == refusal_code ~ "prefer_not_to_answer",
      TRUE ~ "answered"
    )
    code[code < 0] <- NA_integer_
    out[[paste0(id, "_clean")]] <- label_item(code, setNames(seq_along(choices), choices), id)
  }
  out <- dplyr::mutate(out,
    screen_status = dplyr::case_when(
      C0_clean == 2L ~ "declined_consent",
      is.na(C0_clean) ~ "incomplete_screening",
      S1_clean %in% c(2L, 3L) | S1_status == "prefer_not_to_answer" ~ "age_screen_termination",
      is.na(S1_clean) ~ "incomplete_screening",
      S2_clean %in% c(4L, 5L) | S2_status == "prefer_not_to_answer" ~ "role_screen_termination",
      is.na(S2_clean) | (S2_clean == 3L & is.na(S2a_clean)) ~ "incomplete_screening",
      TRUE ~ "eligible"
    ),
    decision_focus = dplyr::case_when(
      screen_status != "eligible" ~ NA_character_,
      S2_clean == 1L | (S2_clean == 3L & S2a_clean == 1L) ~ "Self",
      S2_clean == 2L | (S2_clean == 3L & S2a_clean == 2L) ~ "Child"
    ),
    exposure_status = dplyr::case_when(
      screen_status != "eligible" ~ "not_applicable",
      Q4_clean == 1L ~ "reviewed",
      is.na(Q4_clean) & Q4_status == "missing" ~ "missing",
      TRUE ~ "incomplete"
    ),
    city_comparison = dplyr::case_when(
      screen_status == "eligible" & S3_clean == 1L ~ "San Gabriel",
      screen_status == "eligible" & S3_clean == 2L ~ "Chino Hills"
    ),
    time_band = ordered(ifelse(screen_status == "eligible" & Q1_clean %in% 1:4,
      as.integer(Q1_clean), NA_integer_), levels = 1:4, labels = category_labels$Q1[1:4])
  )
  for (id in rating_ids) {
    code <- decode(id, 1:6)
    out[[paste0(id, "_status")]] <- dplyr::case_when(
      out$screen_status != "eligible" ~ out$screen_status,
      out$exposure_status == "missing" ~ "exposure_unverified",
      out$exposure_status != "reviewed" | code == -1 ~ "not_applicable",
      code == -2 ~ "prefer_not_to_answer",
      code == 6 ~ "cannot_judge",
      is.na(code) ~ "missing",
      TRUE ~ "answered"
    )
    code[!out[[paste0(id, "_status")]] %in% "answered"] <- NA_integer_
    anchors <- if (startsWith(id, "MC")) describes else agreement
    out[[paste0(id, "_score")]] <- label_item(code, anchors, id, "ordinal")
  }
  list(raw_retained = raw_kept, analysis = out, metadata = inspect_metadata(raw_kept),
       provenance_exclusions = sum(!keep), synthetic = synthetic,
       authorized = isTRUE(authorized) && !synthetic)
}

# Run this on authorized samples after inspecting missingness and response distributions.
# Choose a minimum complete-case N with faculty, before reviewing model results.
evaluate_candidate <- function(prepared, group, min_complete_n) {
  if (!isTRUE(prepared$authorized)) stop("Requires authorized research data.")
  items <- rating_groups[[group]]
  if (is.null(items) || length(min_complete_n) != 1L || !is.finite(min_complete_n) ||
      min_complete_n <= length(items)) stop("Specify a defined scale and defensible minimum N.")
  x <- as.data.frame(lapply(prepared$analysis[paste0(items, "_score")], as.numeric))
  complete <- x[stats::complete.cases(x), , drop = FALSE]
  n <- nrow(complete)
  if (n < min_complete_n || any(vapply(complete, stats::sd, 0) == 0)) {
    return(list(complete_n = n, status = "insufficient cases or zero item variance"))
  }
  covariance <- stats::cov(complete)
  k <- length(items)
  total_variance <- sum(covariance)
  alpha <- if (total_variance > 0) k / (k - 1) * (1 - sum(diag(covariance)) / total_variance) else NA_real_
  list(complete_n = n, alpha = alpha, correlations = stats::cor(complete),
       correlation_eigenvalues = eigen(stats::cor(complete), symmetric = TRUE)$values,
       complete_items = complete)
}

score_approved_scale <- function(prepared, group, approved = FALSE, min_items = 3L) {
  if (!isTRUE(approved)) stop("Document a scale review before computing a mean.")
  items <- rating_groups[[group]]
  if (is.null(items) || length(min_items) != 1L || !min_items %in% 2:length(items)) {
    stop("Specify a candidate group and a valid item minimum.")
  }
  mat <- as.matrix(data.frame(lapply(prepared$analysis[paste0(items, "_score")], as.numeric)))
  valid <- rowSums(!is.na(mat))
  result <- rowMeans(mat, na.rm = TRUE)
  result[valid < min_items] <- NA_real_
  prepared$analysis[[paste0(group, "_items_answered")]] <- valid
  prepared$analysis[[paste0(group, "_mean")]] <- result
  prepared
}

write_authorized_clean_sav <- function(prepared, path) {
  if (!isTRUE(prepared$authorized)) stop("Only authorized analysis output may be exported.")
  if (file.exists(path)) stop("Choose a new restricted output file.")
  data <- prepared$analysis[prepared$analysis$screen_status == "eligible", , drop = FALSE]
  haven::write_sav(data, path)
  invisible(path)
}

test_synthetic <- function() {
  # Synthetic test data — not participant responses or empirical research findings.
  raw <- as.data.frame(lapply(category_labels, function(x) rep(1L, 5L)))
  for (id in rating_ids) raw[[id]] <- c(5L, 6L, 4L, 3L, 2L)
  raw$C0[4] <- 2L
  raw$Q4[3] <- 2L
  raw$S3[2] <- 4L
  raw$S2[2] <- 3L
  raw$S2a[2] <- 2L
  raw$Q1[5] <- 5L
  ids <- names(raw)
  maps <- lapply(category_labels, function(x) setNames(seq_along(x), seq_along(x)))
  for (id in rating_ids) maps[[id]] <- setNames(1:6, 1:6)
  prepared <- prepare_core(raw, setNames(ids, ids), maps,
    c(rep("synthetic", 4L), "unresolved"), synthetic = TRUE)
  x <- score_approved_scale(prepared, "consultation_intention", approved = TRUE)$analysis
  stopifnot(nrow(x) == 4L, x$decision_focus[2] == "Child",
    x$S3_status[2] == "prefer_not_to_answer", is.na(x$city_comparison[2]),
    x$IQ1_status[2] == "cannot_judge", x$IQ1_status[3] == "not_applicable",
    x$screen_status[4] == "declined_consent",
    identical(x$consultation_intention_mean, c(5, NA_real_, NA_real_, NA_real_)),
    prepared$provenance_exclusions == 1)
  message("Synthetic test data — not participant responses or empirical research findings. Checks passed.")
  invisible(TRUE)
}

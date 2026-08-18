# Core generator for REDCap randomization allocation tables.
# Produces development and production CSVs in the format required by
# Randomization Setup Step 3 (column names = REDCap variables only).

is_blank <- function(x) {
  is.null(x) || (length(x) == 1 && (is.na(x) || !nzchar(trimws(as.character(x)))))
}

trim_chr <- function(x) {
  trimws(as.character(x))
}

parse_csv_vals <- function(x) {
  if (is_blank(x)) {
    return(character(0))
  }
  vals <- trim_chr(unlist(strsplit(as.character(x), ",", fixed = TRUE)))
  vals[nzchar(vals)]
}

parse_int_csv <- function(x) {
  vals <- parse_csv_vals(x)
  nums <- suppressWarnings(as.integer(vals))
  if (length(vals) == 0 || any(is.na(nums))) {
    stop("Expected a comma-separated list of integers, got: ", x, call. = FALSE)
  }
  nums
}

# "sex=1,2;age_gp=1,2" -> list(sex = c("1","2"), age_gp = c("1","2"))
parse_strata_spec <- function(x) {
  if (is_blank(x)) {
    return(list())
  }
  parts <- trim_chr(unlist(strsplit(as.character(x), ";", fixed = TRUE)))
  parts <- parts[nzchar(parts)]
  out <- list()
  for (part in parts) {
    if (!grepl("=", part, fixed = TRUE)) {
      stop("Stratum spec must look like name=v1,v2 (got: ", part, ")", call. = FALSE)
    }
    kv <- strsplit(part, "=", fixed = TRUE)[[1]]
    name <- trim_chr(kv[[1]])
    codes <- parse_csv_vals(paste(kv[-1], collapse = "="))
    if (!nzchar(name) || length(codes) == 0) {
      stop("Stratum spec must include a variable name and at least one code: ", part, call. = FALSE)
    }
    out[[name]] <- codes
  }
  out
}

valid_redcap_variable <- function(x) {
  grepl("^[a-z][a-z0-9_]*$", x)
}

REDCAP_MAX_STRATA <- 14L
STRATA_WARN_FIELDS <- 8L

count_stratum_combos <- function(p) {
  factors <- p$strata
  if (!is.null(p$site)) {
    factors[[p$site$name]] <- p$site$values
  }
  if (length(factors) == 0) {
    return(1L)
  }
  nrow(factor_grid(factors))
}

tables_identical <- function(a, b) {
  if (nrow(a) != nrow(b) || !identical(names(a), names(b))) {
    return(FALSE)
  }
  identical(as.matrix(a), as.matrix(b))
}

strata_warnings <- function(p, n_combos, n_min) {
  warns <- character(0)
  n_strata <- length(p$strata)
  if (n_strata > REDCAP_MAX_STRATA) {
    warns <- c(warns, paste0(
      "You have ", n_strata, " stratum fields; REDCap allows at most ",
      REDCAP_MAX_STRATA, ". Remove strata before uploading."
    ))
  } else if (n_strata > STRATA_WARN_FIELDS) {
    warns <- c(warns, paste0(
      "You have ", n_strata, " stratum fields (REDCap allows up to ",
      REDCAP_MAX_STRATA, "). More strata means fewer participants per cell ",
      "and weaker balance."
    ))
  }
  if (n_combos > 32L) {
    warns <- c(warns, paste0(
      n_combos, " stratum x site combinations — expect very few rows per cell ",
      "(min per combination: ", n_min, "). Consider fewer strata or a larger N."
    ))
  } else if (n_combos > 1L && n_min < max(p$block_sizes)) {
    warns <- c(warns, paste0(
      "Only ", n_min, " rows per combination (largest block size is ",
      max(p$block_sizes), "). Increase N, buffer, or use --n-per-stratum."
    ))
  }
  if (n_combos * n_min > ceiling(p$n * (1 + p$buffer) * 3)) {
    warns <- c(warns, paste0(
      "Total rows (", n_combos * n_min, ") are much larger than buffered N (",
      ceiling(p$n * (1 + p$buffer)), "). This is usually fine but check your inputs."
    ))
  }
  warns
}

role_blinding_notes <- function(p) {
  c(
    "User roles and blinding (assign in REDCap User Rights):",
    "  - Setup: uploads allocation tables; must NOT need to stay blinded.",
    "  - Dashboard: sees used/unused counts and group breakdown; cannot stay blinded.",
    "  - Randomize: clicks Randomize on forms/surveys; may be blinded if concealed.",
    if (identical(p$type, "blinded")) {
      c(
        "  - Mapping CSVs: unblinded statistician / pharmacy only. Do not share with blinded staff.",
        "  - Alternative: store the code-break mapping in a separate REDCap project."
      )
    } else if (isTRUE(p$include_rand_number)) {
      "  - redcap_randomization_number supports [rand-number] on open allocation."
    } else {
      character(0)
    },
    "  - One allocation table pair (_dev / _prod) per randomization model.",
    "    Platform / SMART trials need separate runs with different --field / --prefix."
  )
}

default_block_sizes <- function(ratio) {
  unit <- sum(ratio)
  as.integer(c(2L, 3L, 4L) * unit)
}

validate_params <- function(p) {
  errs <- character(0)
  if (is.null(p$n) || is.na(p$n) || p$n < 1) {
    errs <- c(errs, "Target sample size N must be a positive integer.")
  }
  if (is.null(p$buffer) || is.na(p$buffer) || p$buffer < 0) {
    errs <- c(errs, "Buffer must be a proportion >= 0 (e.g. 0.25 for 25%).")
  }
  if (is_blank(p$field) || !valid_redcap_variable(p$field)) {
    errs <- c(errs, "Randomization field must be a REDCap variable name (lowercase, start with a letter, only letters/numbers/underscores).")
  }
  if (!p$type %in% c("open", "blinded")) {
    errs <- c(errs, "Allocation type must be 'open' or 'blinded'.")
  }
  if (length(p$arms) < 2) {
    errs <- c(errs, "Provide at least two allocation groups/arms.")
  }
  if (any(!nzchar(p$arms)) || any(duplicated(p$arms))) {
    errs <- c(errs, "Allocation codes must be non-empty and unique.")
  }
  if (length(p$ratio) != length(p$arms) || any(is.na(p$ratio)) || any(p$ratio < 1)) {
    errs <- c(errs, "Allocation ratio must be a positive integer for each arm (e.g. 1,1 or 2,1).")
  }
  if (length(p$arm_labels) && length(p$arm_labels) != length(p$arms)) {
    errs <- c(errs, "If arm labels are provided, there must be one label per arm.")
  }
  unit <- sum(p$ratio)
  if (length(p$block_sizes) == 0 || any(is.na(p$block_sizes)) || any(p$block_sizes < unit)) {
    errs <- c(errs, "Block sizes are required and must be at least the allocation-ratio total.")
  } else if (any(p$block_sizes %% unit != 0)) {
    errs <- c(errs, paste0(
      "Each block size must be a multiple of the allocation-ratio total (", unit, "). ",
      "For 1:1 use 4,6,8; for 2:1 or three 1:1:1 arms use 3,6,9."
    ))
  }
  stratum_names <- names(p$strata)
  if (length(p$strata)) {
    if (is.null(stratum_names) || any(!nzchar(stratum_names))) {
      errs <- c(errs, "Each stratum must have a REDCap variable name.")
    } else {
      bad <- stratum_names[!vapply(stratum_names, valid_redcap_variable, logical(1))]
      if (length(bad)) {
        errs <- c(errs, paste("Invalid stratum variable name(s):", paste(bad, collapse = ", ")))
      }
      if (any(duplicated(stratum_names))) {
        errs <- c(errs, "Stratum variable names must be unique.")
      }
      if (p$field %in% stratum_names) {
        errs <- c(errs, "A stratum field cannot be the same as the randomization field.")
      }
      empty <- stratum_names[vapply(p$strata, function(v) length(v) == 0, logical(1))]
      if (length(empty)) {
        errs <- c(errs, paste("Stratum fields need at least one coded value:", paste(empty, collapse = ", ")))
      }
    }
  }
  if (!is.null(p$site)) {
    if (is_blank(p$site$name) || !valid_redcap_variable(p$site$name)) {
      errs <- c(errs, "Group/site field must be a REDCap variable name (use redcap_data_access_group for DAGs).")
    } else if (identical(p$site$name, p$field) || p$site$name %in% names(p$strata)) {
      errs <- c(errs, "Group/site field must be different from the randomization field and strata.")
    }
    if (length(p$site$values) == 0) {
      errs <- c(errs, "Group/site needs at least one coded value.")
    } else if (identical(p$site$name, "redcap_data_access_group") &&
        !all(grepl("^[0-9]+$", p$site$values))) {
      errs <- c(errs, "For DAG randomization, use numeric group IDs from the Step 2 template (not display names).")
    }
  }
  if (length(p$strata) > REDCAP_MAX_STRATA) {
    errs <- c(errs, paste0(
      "REDCap supports at most ", REDCAP_MAX_STRATA, " stratification fields; you have ",
      length(p$strata), "."
    ))
  }
  if (identical(p$type, "open") && isTRUE(p$include_rand_number)) {
    reserved <- "redcap_randomization_number"
    if (identical(p$field, reserved) || reserved %in% names(p$strata) ||
        (!is.null(p$site) && identical(p$site$name, reserved))) {
      errs <- c(errs, "redcap_randomization_number is reserved when used as an optional audit column.")
    }
  }
  if (!is.null(p$n_per_stratum) && (is.na(p$n_per_stratum) || p$n_per_stratum < 1)) {
    errs <- c(errs, "n_per_stratum must be a positive integer if provided.")
  }
  if (!is.null(p$seed) && (is.na(p$seed) || p$seed < 0)) {
    errs <- c(errs, "Seed must be a non-negative integer.")
  }
  if (!is.null(p$seed_prod) && (is.na(p$seed_prod) || p$seed_prod < 0)) {
    errs <- c(errs, "Production seed must be a non-negative integer.")
  }
  if (identical(p$type, "blinded")) {
    if (!p$code_style %in% c("alphanumeric", "sequential")) {
      errs <- c(errs, "Concealed code style must be 'alphanumeric' or 'sequential'.")
    }
    if (identical(p$code_style, "alphanumeric")) {
      if (is.null(p$code_length) || is.na(p$code_length) || p$code_length < 3) {
        errs <- c(errs, "Alphanumeric concealment codes must be at least 3 characters.")
      }
      if (length(p$code_alphabet) < 2) {
        errs <- c(errs, "Code alphabet needs at least 2 symbols.")
      }
    }
    reserved <- "redcap_randomization_group"
    if (identical(p$field, reserved) || reserved %in% names(p$strata) ||
        (!is.null(p$site) && identical(p$site$name, reserved))) {
      errs <- c(errs, "redcap_randomization_group is a reserved REDCap column; do not use it as a field name.")
    }
  }
  errs
}

# One permuted block: treatments appear in the requested ratio, then shuffled.
generate_block <- function(arms, ratio, block_size) {
  k <- as.integer(block_size / sum(ratio))
  block <- rep(arms, times = ratio * k)
  sample(block)
}

# Concatenate random-sized permuted blocks until at least n_min assignments exist.
# The last block is kept in full so within-stratum ratios stay exact.
generate_stratum_sequence <- function(n_min, arms, ratio, block_sizes) {
  seqv <- character(0)
  while (length(seqv) < n_min) {
    bs <- sample(block_sizes, 1)
    seqv <- c(seqv, generate_block(arms, ratio, bs))
  }
  seqv
}

default_code_alphabet <- function(exclude_ambiguous = FALSE) {
  chars <- c(LETTERS, as.character(0:9))
  if (isTRUE(exclude_ambiguous)) {
    chars <- setdiff(chars, c("0", "O", "1", "I", "L"))
  }
  chars
}

# Unique concealment codes (A-Z0-9 by default), base R equivalent of stri_rand_strings().
generate_unique_codes <- function(n, code_length = 3L, alphabet = default_code_alphabet()) {
  n <- as.integer(n)
  code_length <- as.integer(code_length)
  alphabet <- unique(as.character(alphabet))
  if (n < 1) {
    return(character(0))
  }
  space <- length(alphabet)^code_length
  if (is.na(space) || n > space) {
    stop(
      sprintf(
        "Cannot create %s unique codes of length %s from %s symbols (max %s). Increase --code-length.",
        n, code_length, length(alphabet), space
      ),
      call. = FALSE
    )
  }
  codes <- character(0)
  guard <- 0L
  while (length(codes) < n) {
    guard <- guard + 1L
    if (guard > 1000L) {
      stop("Could not generate enough unique concealment codes.", call. = FALSE)
    }
    needed <- n - length(codes)
    draw <- as.integer(min(space, max(needed * 2L, needed + 32L)))
    batch <- replicate(
      draw,
      paste(sample(alphabet, code_length, replace = TRUE), collapse = ""),
      simplify = TRUE
    )
    codes <- unique(c(codes, batch))
  }
  codes[seq_len(n)]
}

factor_grid <- function(factors) {
  if (length(factors) == 0) {
    return(NULL)
  }
  grid <- expand.grid(factors, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  rownames(grid) <- NULL
  grid
}

normalize_params <- function(
    n,
    field,
    arms = c("1", "2"),
    ratio = c(1L, 1L),
    strata = list(),
    site = NULL,
    block_sizes = NULL,
    buffer = 0.25,
    n_per_stratum = NULL,
    type = "open",
    arm_labels = NULL,
    rand_start_dev = 1001L,
    rand_start_prod = 5001L,
    seed = NULL,
    prod_seed = NULL,
    prefix = "RandomizationAllocationTemplate",
    code_style = "alphanumeric",
    code_length = 3L,
    exclude_ambiguous = FALSE,
    include_group_col = TRUE,
    include_rand_number = FALSE) {
  field <- trim_chr(field)
  arms <- trim_chr(arms)
  type <- match.arg(type, c("open", "blinded"))
  code_style <- match.arg(code_style, c("alphanumeric", "sequential"))
  ratio <- as.integer(ratio)
  if (is.null(block_sizes) || length(block_sizes) == 0) {
    block_sizes <- default_block_sizes(ratio)
  } else {
    block_sizes <- as.integer(block_sizes)
  }
  if (is.null(arm_labels) || (length(arm_labels) == 1 && is_blank(arm_labels))) {
    arm_labels <- character(0)
  } else {
    arm_labels <- trim_chr(arm_labels)
  }
  if (is.null(seed) || is.na(seed)) {
    seed <- as.integer(Sys.time()) %% 1e8
  } else {
    seed <- as.integer(seed)
  }
  if (is.null(prod_seed) || is.na(prod_seed)) {
    seed_prod <- seed + 104729L
  } else {
    seed_prod <- as.integer(prod_seed)
  }
  list(
    n = as.integer(n),
    field = field,
    arms = arms,
    ratio = ratio,
    strata = strata,
    site = site,
    block_sizes = block_sizes,
    buffer = as.numeric(buffer),
    n_per_stratum = if (is.null(n_per_stratum)) NULL else as.integer(n_per_stratum),
    type = type,
    arm_labels = arm_labels,
    rand_start_dev = as.integer(rand_start_dev),
    rand_start_prod = as.integer(rand_start_prod),
    seed = seed,
    seed_prod = seed_prod,
    prefix = prefix,
    code_style = code_style,
    code_length = as.integer(code_length),
    exclude_ambiguous = isTRUE(exclude_ambiguous),
    code_alphabet = default_code_alphabet(exclude_ambiguous),
    include_group_col = isTRUE(include_group_col),
    include_rand_number = isTRUE(include_rand_number) && type == "open"
  )
}

build_one_table <- function(p, seed) {
  set.seed(seed)
  factors <- p$strata
  if (!is.null(p$site)) {
    factors[[p$site$name]] <- p$site$values
  }
  grid <- factor_grid(factors)
  n_combos <- if (is.null(grid)) 1L else nrow(grid)
  target_total <- ceiling(p$n * (1 + p$buffer))
  n_min <- if (!is.null(p$n_per_stratum)) {
    p$n_per_stratum
  } else {
    as.integer(ceiling(target_total / n_combos))
  }

  make_chunk <- function(combo = NULL) {
    seq_arms <- generate_stratum_sequence(n_min, p$arms, p$ratio, p$block_sizes)
    out <- setNames(data.frame(seq_arms, stringsAsFactors = FALSE), p$field)
    if (!is.null(combo)) {
      extra <- combo[rep(1, length(seq_arms)), , drop = FALSE]
      out <- cbind(out, extra)
    }
    out
  }

  if (is.null(grid)) {
    df <- make_chunk(NULL)
  } else {
    chunks <- lapply(seq_len(n_combos), function(i) make_chunk(grid[i, , drop = FALSE]))
    df <- do.call(rbind, chunks)
  }
  rownames(df) <- NULL
  keep <- c(p$field, names(factors))
  df <- df[, keep, drop = FALSE]
  df[] <- lapply(df, as.character)
  df
}

apply_concealed_codes <- function(raw, codes, p) {
  mapping <- data.frame(
    rand_number = as.character(codes),
    treatment_code = raw[[p$field]],
    stringsAsFactors = FALSE
  )
  if (length(p$arm_labels)) {
    mapping$treatment_label <- p$arm_labels[match(mapping$treatment_code, p$arms)]
  }
  extra <- setdiff(names(raw), p$field)
  if (length(extra)) {
    mapping <- cbind(mapping, raw[extra])
  }
  blinded <- raw
  blinded[[p$field]] <- mapping$rand_number
  if (isTRUE(p$include_group_col)) {
    blinded$redcap_randomization_group <- mapping$treatment_code
    keep <- c(p$field, "redcap_randomization_group", extra)
    blinded <- blinded[, keep, drop = FALSE]
  }
  list(table = blinded, mapping = mapping)
}

apply_open_rand_numbers <- function(raw, codes, field) {
  out <- raw
  out$redcap_randomization_number <- as.character(codes)
  strata_cols <- setdiff(names(raw), field)
  keep <- c(field, "redcap_randomization_number", strata_cols)
  out[, keep, drop = FALSE]
}

make_open_rand_pair <- function(dev_raw, prod_raw, p) {
  n_dev <- nrow(dev_raw)
  n_prod <- nrow(prod_raw)
  set.seed(p$seed)
  all_codes <- generate_unique_codes(n_dev + n_prod, p$code_length, p$code_alphabet)
  list(
    dev = apply_open_rand_numbers(dev_raw, all_codes[seq_len(n_dev)], p$field),
    prod = apply_open_rand_numbers(prod_raw, all_codes[seq.int(n_dev + 1L, n_dev + n_prod)], p$field)
  )
}

make_concealed_pair <- function(dev_raw, prod_raw, p) {
  n_dev <- nrow(dev_raw)
  n_prod <- nrow(prod_raw)
  if (identical(p$code_style, "sequential")) {
    codes_dev <- as.character(seq.int(p$rand_start_dev, length.out = n_dev))
    codes_prod <- as.character(seq.int(p$rand_start_prod, length.out = n_prod))
  } else {
    set.seed(p$seed)
    all_codes <- generate_unique_codes(n_dev + n_prod, p$code_length, p$code_alphabet)
    codes_dev <- all_codes[seq_len(n_dev)]
    codes_prod <- all_codes[seq.int(n_dev + 1L, n_dev + n_prod)]
  }
  list(
    dev = apply_concealed_codes(dev_raw, codes_dev, p),
    prod = apply_concealed_codes(prod_raw, codes_prod, p)
  )
}

arm_label_text <- function(p) {
  if (length(p$arm_labels)) {
    paste(sprintf("%s=%s (ratio %s)", p$arms, p$arm_labels, p$ratio), collapse = "; ")
  } else {
    paste(sprintf("%s (ratio %s)", p$arms, p$ratio), collapse = "; ")
  }
}

balance_counts <- function(df, field, group_cols) {
  cols <- c(group_cols, field)
  tab <- as.data.frame(table(df[cols], useNA = "ifany"), stringsAsFactors = FALSE)
  tab[do.call(order, tab[cols]), ]
}

# For blinded tables the REDCap column holds rand numbers; the log should show treatment codes.
log_balance_frame <- function(df, p) {
  if (identical(p$type, "blinded") && p$field %in% names(df)) {
    names(df)[names(df) == p$field] <- "treatment"
  }
  df
}

format_balance <- function(df, p) {
  alloc_col <- intersect(c("treatment", p$field), names(df))[1]
  group_cols <- setdiff(names(df), c(alloc_col, "redcap_randomization_group", "redcap_randomization_number"))
  counts <- balance_counts(df, alloc_col, group_cols)
  capture.output(print(counts, row.names = FALSE))
}

build_log <- function(p, dev, prod, n_combos, n_min, warnings = character(0)) {
  lines <- c(
    "REDCap randomization allocation table",
    paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("Filename prefix:", p$prefix),
    "",
    "Seeds (keep this log; do not share production seed with blinded staff):",
    paste("  Development:", p$seed),
    paste("  Production: ", p$seed_prod),
    "",
    "Model (maps to REDCap Randomization Setup Step 1):",
    paste("  C) Randomization field:", p$field),
    paste("  Allocation type:      ", p$type,
          if (p$type == "open") "(dropdown/radio field; group codes written to the field)"
          else "(text field; concealment code written to the field)"),
    paste("  Allocation groups:    ", arm_label_text(p)),
    paste("  Block sizes:          ", paste(p$block_sizes, collapse = ", ")),
    paste("  Target N:             ", p$n),
    paste("  Buffer:               ", paste0(round(100 * p$buffer), "%")),
    paste("  Min. per combination: ", n_min),
    paste("  Combinations:         ", n_combos),
    paste("  Rows generated (dev): ", nrow(dev)),
    paste("  Rows generated (prod):", nrow(prod))
  )
  if (identical(p$type, "blinded")) {
    lines <- c(
      lines,
      paste("  Concealment codes:    ", p$code_style,
            if (identical(p$code_style, "alphanumeric")) {
              paste0("(length ", p$code_length, ", alphabet size ", length(p$code_alphabet), ")")
            } else {
              paste0("(dev starts ", p$rand_start_dev, ", prod starts ", p$rand_start_prod, ")")
            }),
      paste("  Include group column: ", if (isTRUE(p$include_group_col)) {
        "yes (redcap_randomization_group; hidden from blinded users, visible to admins)"
      } else {
        "no (group stays in the mapping files only)"
      })
    )
  } else if (isTRUE(p$include_rand_number)) {
    lines <- c(lines, paste(
      "  Optional audit column: redcap_randomization_number (enables [rand-number] smart variable)"
    ))
  }
  lines <- c(lines, "")
  if (length(p$strata)) {
    lines <- c(lines, "  A) Strata:")
    for (nm in names(p$strata)) {
      lines <- c(lines, paste0("     ", nm, ": ", paste(p$strata[[nm]], collapse = ", ")))
    }
  } else {
    lines <- c(lines, "  A) Strata: none")
  }
  if (is.null(p$site)) {
    lines <- c(lines, "  B) Group/site: none")
  } else {
    lines <- c(lines, paste0(
      "  B) Group/site: ", p$site$name, " = ", paste(p$site$values, collapse = ", ")
    ))
  }
  lines <- c(
    lines,
    "",
    "Method: permuted-block randomization generated independently within each",
    "stratum x group/site combination. REDCap assigns the next unused matching row.",
    "",
    "REDCap upload reminders:",
    "  - Do not include record IDs. Do not add block_no or internal shuffle columns.",
    "  - A 'notes' column in the Step 2 template is ignored by REDCap if present.",
    "  - Development and Production tables must NOT be identical (REDCap rejects duplicates).",
    "  - Upload the _dev CSV while the project is in Development.",
    "  - Upload the _prod CSV for Production; tables lock after moving to production.",
    "  - Include more rows than the planned sample size (already applied via buffer).",
    "  - Column headers and coded values must match the template from Setup Step 2.",
    "  - For DAG stratification, use numeric group IDs from the template, not display names.",
    if (p$type == "blinded") {
      c(
        if (isTRUE(p$include_group_col)) {
          "  - redcap_randomization_group is optional in REDCap; blinded dashboard users do not see it."
        } else {
          character(0)
        },
        "  - Mapping CSVs are the unblinded audit copy. Do not share them with blinded staff."
      )
    } else {
      character(0)
    },
    "",
    role_blinding_notes(p),
    if (length(warnings)) {
      c("", "Warnings:", paste0("  ! ", warnings))
    } else {
      character(0)
    },
    "",
    "Balance (development table):",
    format_balance(dev, p),
    "",
    "Balance (production table):",
    format_balance(prod, p)
  )
  paste(lines, collapse = "\n")
}

build_prod_raw_distinct <- function(p, dev_raw, max_attempts = 20L) {
  prod_seed <- p$seed_prod
  for (attempt in seq_len(max_attempts)) {
    prod_raw <- build_one_table(p, prod_seed)
    if (!tables_identical(dev_raw, prod_raw)) {
      p$seed_prod <- prod_seed
      return(list(prod_raw = prod_raw, params = p))
    }
    prod_seed <- prod_seed + 7919L * attempt
  }
  stop(
    "Development and production allocation tables are identical after ",
    max_attempts, " reshuffles. Use a different --prod-seed or --seed.",
    call. = FALSE
  )
}

finalize_tables <- function(p, dev_raw, prod_raw) {
  mapping_dev <- NULL
  mapping_prod <- NULL
  if (identical(p$type, "blinded")) {
    concealed <- make_concealed_pair(dev_raw, prod_raw, p)
    dev <- concealed$dev$table
    prod <- concealed$prod$table
    mapping_dev <- concealed$dev$mapping
    mapping_prod <- concealed$prod$mapping
  } else if (isTRUE(p$include_rand_number)) {
    numbered <- make_open_rand_pair(dev_raw, prod_raw, p)
    dev <- numbered$dev
    prod <- numbered$prod
  } else {
    dev <- dev_raw
    prod <- prod_raw
  }
  if (tables_identical(dev, prod)) {
    stop(
      "Development and production upload CSVs are identical. REDCap rejects duplicate ",
      "allocation tables. Change --prod-seed or increase model complexity.",
      call. = FALSE
    )
  }
  list(
    dev = dev,
    prod = prod,
    mapping_dev = mapping_dev,
    mapping_prod = mapping_prod
  )
}

# Returns list(dev, prod, mapping_dev, mapping_prod, log, params, n_combos, n_min, warnings)
generate_allocation_tables <- function(...) {
  p <- normalize_params(...)
  errs <- validate_params(p)
  if (length(errs)) {
    stop(paste(errs, collapse = "\n"), call. = FALSE)
  }

  n_combos <- count_stratum_combos(p)
  target_total <- ceiling(p$n * (1 + p$buffer))
  n_min <- if (!is.null(p$n_per_stratum)) p$n_per_stratum else as.integer(ceiling(target_total / n_combos))
  warnings <- strata_warnings(p, n_combos, n_min)

  dev_raw <- build_one_table(p, p$seed)
  prod_built <- build_prod_raw_distinct(p, dev_raw)
  p <- prod_built$params
  prod_raw <- prod_built$prod_raw

  finalized <- finalize_tables(p, dev_raw, prod_raw)

  list(
    params = p,
    dev = finalized$dev,
    prod = finalized$prod,
    mapping_dev = finalized$mapping_dev,
    mapping_prod = finalized$mapping_prod,
    n_combos = n_combos,
    n_min = n_min,
    warnings = warnings,
    log = build_log(
      p,
      log_balance_frame(if (identical(p$type, "blinded")) dev_raw else finalized$dev, p),
      log_balance_frame(if (identical(p$type, "blinded")) prod_raw else finalized$prod, p),
      n_combos,
      n_min,
      warnings
    )
  )
}

allocation_filenames <- function(outdir, prefix, type) {
  list(
    dev = file.path(outdir, paste0(prefix, "_dev.csv")),
    prod = file.path(outdir, paste0(prefix, "_prod.csv")),
    mapping_dev = file.path(outdir, paste0(prefix, "_mapping_dev.csv")),
    mapping_prod = file.path(outdir, paste0(prefix, "_mapping_prod.csv")),
    log = file.path(outdir, paste0(prefix, "_log.txt"))
  )
}

write_csv_redcap <- function(df, path) {
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

write_allocation_tables <- function(result, outdir, prefix = NULL) {
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }
  prefix <- if (is.null(prefix)) result$params$prefix else prefix
  files <- allocation_filenames(outdir, prefix, result$params$type)
  write_csv_redcap(result$dev, files$dev)
  write_csv_redcap(result$prod, files$prod)
  writeLines(result$log, files$log, useBytes = TRUE)
  if (!is.null(result$mapping_dev)) {
    write_csv_redcap(result$mapping_dev, files$mapping_dev)
    write_csv_redcap(result$mapping_prod, files$mapping_prod)
  } else {
    files$mapping_dev <- NULL
    files$mapping_prod <- NULL
  }
  Filter(Negate(is.null), files)
}

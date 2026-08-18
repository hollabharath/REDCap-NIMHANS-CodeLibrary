#!/usr/bin/env Rscript
#
# Generate REDCap randomization allocation tables (development + production).
#
# Examples:
#   Rscript REDCap_Randomize.R --shiny
#   Rscript REDCap_Randomize.R --n 120 --field randomize --strata "sex=1,2;age_gp=1,2"
#   Rscript REDCap_Randomize.R --n 160 --field redcap_randomization_number \
#       --type blinded --arms INT,CNT --strata "gender=1,2" \
#       --block-sizes 4,6,8 --code-length 3 --seed 2025 --prod-seed 4525 \
#       --outdir ./output

this_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]))))
  }
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    ctx <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
    if (nzchar(ctx)) {
      return(dirname(normalizePath(ctx)))
    }
  }
  getwd()
}

script_dir <- this_dir()
source(file.path(script_dir, "generate_allocation.R"), local = environment())

HELP <- "
Generate REDCap randomization allocation tables (development + production).

Usage:
  Rscript REDCap_Randomize.R --shiny
  Rscript REDCap_Randomize.R [options]

The Shiny app is the easiest way to define strata, group/site, and arms.
Command-line mode writes CSVs matching Randomization Setup Step 3.

Options:
  --n                 Target sample size (default: 120)
  --buffer            Extra assignments as a proportion (default: 0.25)
  --n-per-stratum     Override: minimum rows per stratum x site combination
  --field             Randomization field / REDCap variable (default: randomize)
  --type              open (default) or blinded
  --arms              Comma-separated allocation codes (default: 1,2)
  --arm-labels        Comma-separated labels (optional; used in log/mapping)
  --ratio             Allocation ratio, one integer per arm (default: 1,1)
  --strata            name=v1,v2;name2=v1,v2   (Setup Step 1A)
  --site              name=v1,v2,v3            (Setup Step 1B, a site field)
  --dag               Numeric DAG group IDs from Step 2 template (not display names)
  --block-sizes       Comma-separated. Default is 2x,3x,4x the ratio total
  --code-style        alphanumeric (default) or sequential; blinded only
  --code-length       Length of alphanumeric codes (default: 3)
  --exclude-ambiguous Drop 0/O/1/I/L from alphanumeric codes
  --no-group-col      Do not add redcap_randomization_group to the upload CSV
  --include-rand-number  Open only: add redcap_randomization_number column ([rand-number])
  --rand-start-dev    First number for sequential/dev (default: 1001)
  --rand-start-prod   First number for sequential/prod (default: 5001)
  --seed              Integer seed for the development table
  --prod-seed         Integer seed for the production table (default: seed+104729)
  --outdir            Output directory (default: current directory)
  --prefix            Filename prefix (default: RandomizationAllocationTemplate)
  --dry-run           Print the generation log only; do not write files
  --shiny             Launch the Shiny app
  --help              Show this help

REDCap notes:
  Column headers must match the randomization field, stratum fields, and
  optional group/site field (or redcap_data_access_group). Do not include
  record IDs or block_no columns. Dev and prod tables must differ.
  Upload _dev.csv in Development and _prod.csv in Production.
  For concealed randomization, alphanumeric codes go in the text field;
  redcap_randomization_group is included by default for admin troubleshooting.
  One table pair per randomization model; use --prefix for multiple models.
"

cli_error <- function(...) {
  stop(paste0(...), call. = FALSE)
}

hyphen_to_underscore <- function(x) gsub("-", "_", x, fixed = TRUE)

parse_cli <- function(args) {
  defaults <- list(
    n = 120L,
    buffer = 0.25,
    n_per_stratum = NULL,
    field = "randomize",
    type = "open",
    arms = "1,2",
    arm_labels = NULL,
    ratio = "1,1",
    strata = NULL,
    site = NULL,
    dag = NULL,
    block_sizes = NULL,
    code_style = "alphanumeric",
    code_length = 3L,
    exclude_ambiguous = FALSE,
    no_group_col = FALSE,
    include_rand_number = FALSE,
    rand_start_dev = 1001L,
    rand_start_prod = 5001L,
    seed = NULL,
    prod_seed = NULL,
    outdir = getwd(),
    prefix = "RandomizationAllocationTemplate",
    dry_run = FALSE,
    shiny = FALSE,
    help = FALSE
  )
  if (length(args) == 0) {
    return(defaults)
  }

  flags <- c("dry-run", "shiny", "help", "exclude-ambiguous", "no-group-col", "include-rand-number")
  i <- 1
  while (i <= length(args)) {
    raw <- args[[i]]
    if (!startsWith(raw, "--")) {
      cli_error("Unexpected argument: ", raw, "\nUse --help for usage.")
    }
    tok <- sub("^--", "", raw)
    if (grepl("=", tok, fixed = TRUE)) {
      kv <- strsplit(tok, "=", fixed = TRUE)[[1]]
      key <- hyphen_to_underscore(kv[[1]])
      val <- paste(kv[-1], collapse = "=")
    } else if (tok %in% flags) {
      key <- hyphen_to_underscore(tok)
      val <- TRUE
    } else {
      key <- hyphen_to_underscore(tok)
      if (i == length(args) || startsWith(args[[i + 1]], "--")) {
        cli_error("Option --", tok, " requires a value.")
      }
      i <- i + 1
      val <- args[[i]]
    }
    if (!key %in% names(defaults)) {
      cli_error("Unknown option --", tok, "\nUse --help for usage.")
    }
    defaults[[key]] <- val
    i <- i + 1
  }
  defaults
}

coerce_cli <- function(opt) {
  opt$n <- as.integer(opt$n)
  opt$buffer <- as.numeric(opt$buffer)
  opt$code_length <- as.integer(opt$code_length)
  opt$rand_start_dev <- as.integer(opt$rand_start_dev)
  opt$rand_start_prod <- as.integer(opt$rand_start_prod)
  opt$dry_run <- isTRUE(opt$dry_run) || identical(opt$dry_run, "TRUE")
  opt$shiny <- isTRUE(opt$shiny) || identical(opt$shiny, "TRUE")
  opt$help <- isTRUE(opt$help) || identical(opt$help, "TRUE")
  opt$exclude_ambiguous <- isTRUE(opt$exclude_ambiguous) || identical(opt$exclude_ambiguous, "TRUE")
  opt$no_group_col <- isTRUE(opt$no_group_col) || identical(opt$no_group_col, "TRUE")
  opt$include_rand_number <- isTRUE(opt$include_rand_number) || identical(opt$include_rand_number, "TRUE")
  if (!is.null(opt$n_per_stratum) && !is_blank(opt$n_per_stratum)) {
    opt$n_per_stratum <- as.integer(opt$n_per_stratum)
  } else {
    opt$n_per_stratum <- NULL
  }
  if (!is.null(opt$seed) && !is_blank(opt$seed)) {
    opt$seed <- as.integer(opt$seed)
  } else {
    opt$seed <- NULL
  }
  if (!is.null(opt$prod_seed) && !is_blank(opt$prod_seed)) {
    opt$prod_seed <- as.integer(opt$prod_seed)
  } else {
    opt$prod_seed <- NULL
  }
  if (!is.null(opt$block_sizes) && !is_blank(opt$block_sizes)) {
    opt$block_sizes <- parse_int_csv(opt$block_sizes)
  } else {
    opt$block_sizes <- NULL
  }
  opt$arms <- parse_csv_vals(opt$arms)
  opt$ratio <- parse_int_csv(opt$ratio)
  if (is_blank(opt$arm_labels)) {
    opt$arm_labels <- NULL
  } else {
    opt$arm_labels <- parse_csv_vals(opt$arm_labels)
  }
  opt$strata <- parse_strata_spec(opt$strata)

  site <- NULL
  if (!is_blank(opt$site) && !is_blank(opt$dag)) {
    cli_error("Use either --site or --dag, not both.")
  }
  if (!is_blank(opt$site)) {
    spec <- parse_strata_spec(opt$site)
    if (length(spec) != 1) {
      cli_error("--site must be a single field, e.g. --site \"site=1,2,3\".")
    }
    site <- list(name = names(spec)[[1]], values = spec[[1]])
  }
  if (!is_blank(opt$dag)) {
    site <- list(name = "redcap_data_access_group", values = parse_csv_vals(opt$dag))
  }
  opt$site <- site
  opt
}

run_shiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The Shiny app requires the 'shiny' package. Install it with install.packages(\"shiny\")", call. = FALSE)
  }
  shiny::runApp(script_dir, launch.browser = TRUE)
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  opt <- parse_cli(args)
  if (isTRUE(opt$help) || identical(opt$help, "TRUE")) {
    cat(HELP)
    return(invisible(NULL))
  }
  if (length(args) == 0 || isTRUE(opt$shiny) || identical(opt$shiny, "TRUE")) {
    if (length(args) == 0) {
      cat("No arguments given. Launching the Shiny app.\n")
      cat("Use --help for command-line options, or pass --n, --strata, etc.\n\n")
    }
    run_shiny()
    return(invisible(NULL))
  }

  opt <- coerce_cli(opt)
  result <- generate_allocation_tables(
    n = opt$n,
    field = opt$field,
    arms = opt$arms,
    ratio = opt$ratio,
    strata = opt$strata,
    site = opt$site,
    block_sizes = opt$block_sizes,
    buffer = opt$buffer,
    n_per_stratum = opt$n_per_stratum,
    type = opt$type,
    arm_labels = opt$arm_labels,
    rand_start_dev = opt$rand_start_dev,
    rand_start_prod = opt$rand_start_prod,
    seed = opt$seed,
    prod_seed = opt$prod_seed,
    prefix = opt$prefix,
    code_style = opt$code_style,
    code_length = opt$code_length,
    exclude_ambiguous = opt$exclude_ambiguous,
    include_group_col = !isTRUE(opt$no_group_col),
    include_rand_number = isTRUE(opt$include_rand_number)
  )
  if (length(result$warnings)) {
    cat("\nWarnings:\n")
    cat(paste0("  ! ", result$warnings, collapse = "\n"), "\n")
  }
  cat(result$log)
  cat("\n")
  if (isTRUE(opt$dry_run)) {
    cat("\nDry run: no files written.\n")
    return(invisible(result))
  }
  files <- write_allocation_tables(result, opt$outdir, opt$prefix)
  cat("\nWrote:\n")
  cat(paste0("  ", unlist(files), collapse = "\n"), "\n", sep = "")
  invisible(result)
}

invoked_via_rscript <- function() {
  file_args <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  length(file_args) > 0 && grepl("REDCap_Randomize\\.R$", sub("^--file=", "", file_args[[1]]))
}

if (invoked_via_rscript()) {
  tryCatch(main(), error = function(e) {
    message("Error: ", conditionMessage(e))
    quit(status = 1)
  })
}

# Shiny app for REDCap allocation tables.
# Launch: Rscript REDCap_Randomize.R --shiny
#     or: shiny::runApp("Randomisation")

src <- if (file.exists("generate_allocation.R")) {
  "generate_allocation.R"
} else {
  file.path("Randomisation", "generate_allocation.R")
}
source(src, local = environment())

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

APP_VERSION <- "1.3"

# Fresh development seed each time the app process starts (not tied to calendar date).
INITIAL_SEED <- as.integer(sample.int(1e8L - 1L, 1L))

help_box <- function(...) {
  shiny::div(class = "help-box", shiny::HTML(paste(...)))
}

ui <- shiny::fluidPage(
  shiny::tags$head(shiny::tags$style(shiny::HTML("
    body { font-size: 14px; }
    .title-bar {
      background: #1a365d; color: #fff; padding: 18px 24px; margin: 0 -15px 20px;
    }
    .title-bar h2 { margin: 0 0 4px; font-size: 22px; }
    .title-bar p { margin: 0; opacity: 0.85; }
    .step-card {
      background: #f7fafc; border: 1px solid #e2e8f0; border-radius: 8px;
      padding: 16px 18px; margin-bottom: 16px;
    }
    .step-card h4 { margin-top: 0; color: #1a365d; }
    .help-box {
      background: #edf2f7; border-left: 4px solid #2b6cb0;
      padding: 8px 12px; margin: 8px 0 12px; font-size: 13px; color: #2d3748;
    }
    .warn-box {
      background: #fffaf0; border-left: 4px solid #c05621;
      padding: 8px 12px; margin: 8px 0; font-size: 13px;
    }
    .ok-box {
      background: #f0fff4; border-left: 4px solid #276749;
      padding: 8px 12px; margin: 8px 0; font-size: 13px; white-space: pre-wrap;
      font-family: ui-monospace, SFMono-Regular, Menlo, monospace; max-height: 360px;
      overflow: auto;
    }
    .preview-table { font-size: 13px; }
    .app-footer {
      margin: 28px -15px 0; padding: 16px 24px 20px;
      border-top: 1px solid #e2e8f0; background: #f7fafc;
      font-size: 12px; line-height: 1.55; color: #4a5568; text-align: center;
    }
    .app-footer .citation { margin-bottom: 8px; color: #2d3748; }
    .app-footer a { color: #2b6cb0; text-decoration: none; }
    .app-footer a:hover { text-decoration: underline; }
    .app-footer .meta { color: #718096; }
  "))),
  shiny::div(
    class = "title-bar",
    shiny::h2("REDCap allocation table generator"),
    shiny::p("Build development and production CSVs for Randomization Setup Step 3. Column names and codes must match your project.")
  ),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,
      shiny::numericInput("n", "Target sample size (N)", value = 120, min = 1, step = 1),
      shiny::sliderInput("buffer_pct", "Extra assignments (drop-in / drop-out buffer)", min = 0, max = 100, value = 25, step = 5, post = "%"),
      shiny::textInput("block_sizes", "Block sizes (comma-separated)", value = "4,6,8"),
      shiny::helpText("Each size must be a multiple of the allocation-ratio total (2 for 1:1, 3 for 2:1). With equalize on, every combination gets the same exact length."),
      shiny::checkboxInput(
        "equalize",
        "Equalize lengths (exact common row count per combination; recommended)",
        value = TRUE
      ),
      shiny::numericInput("seed", "Random seed (development table)", value = INITIAL_SEED, min = 0, step = 1),
      shiny::helpText("Seed is chosen at random when the app starts; change it only if you need to reproduce a prior table."),
      shiny::numericInput("prod_seed", "Production seed (optional)", value = NA, min = 0, step = 1),
      shiny::actionButton("reshuffle_seed", "New random seed", class = "btn-default btn-sm"),
      shiny::textInput("prefix", "Filename prefix (one per randomization model)", value = "RandomizationAllocationTemplate"),
      shiny::textInput("field", "C) Randomization field (REDCap variable)", value = "randomize"),
      shiny::radioButtons(
        "type",
        "Allocation type",
        choices = c(
          "Open — dropdown/radio; group code is saved" = "open",
          "Concealed (blinded) — text field; randomization number is saved" = "blinded"
        ),
        selected = "open"
      ),
      shiny::conditionalPanel(
        "input.type == 'open'",
        shiny::checkboxInput(
          "include_rand_number",
          "Add redcap_randomization_number column (optional audit / [rand-number] smart variable)",
          value = FALSE
        )
      ),
      shiny::conditionalPanel(
        "input.type == 'blinded'",
        help_box("Concealed codes are unique alphanumeric IDs (like bottle codes). Optionally include <code>redcap_randomization_group</code> in the upload CSV; REDCap hides that column from blinded dashboard users."),
        shiny::radioButtons(
          "code_style",
          "Concealment code style",
          choices = c(
            "Alphanumeric (e.g. K7Q, 3BM)" = "alphanumeric",
            "Sequential numbers (e.g. 1001, 1002)" = "sequential"
          ),
          selected = "alphanumeric"
        ),
        shiny::conditionalPanel(
          "input.code_style == 'alphanumeric'",
          shiny::numericInput("code_length", "Code length", value = 3, min = 3, max = 12, step = 1),
          shiny::checkboxInput("exclude_ambiguous", "Exclude ambiguous characters (0/O, 1/I/L)", value = FALSE)
        ),
        shiny::conditionalPanel(
          "input.code_style == 'sequential'",
          shiny::numericInput("rand_start_dev", "First rand number (dev)", value = 1001, min = 1, step = 1),
          shiny::numericInput("rand_start_prod", "First rand number (prod)", value = 5001, min = 1, step = 1)
        ),
        shiny::checkboxInput(
          "include_group_col",
          "Include redcap_randomization_group in the upload CSV",
          value = TRUE
        )
      )
    ),
    shiny::mainPanel(
      width = 8,
      shiny::div(
        class = "step-card",
        shiny::h4("Allocation groups"),
        help_box("Codes must match the choices on an open randomization field (for example 1/2 or INT/CNT). For concealed randomization they become <code>redcap_randomization_group</code>."),
        shiny::numericInput("n_arms", "Number of groups", value = 2, min = 2, max = 8, step = 1),
        shiny::uiOutput("arms_ui")
      ),
      shiny::div(
        class = "step-card",
        shiny::h4("A) Use stratified randomization?"),
        help_box("Choose the same multiple-choice fields and coded values as in REDCap Setup Step 1A (for example sex, age group). Balance is enforced <em>within</em> each combination of codes. With equalize on, combinations share equal lengths unless you set a strata quota or ratio below."),
        shiny::checkboxInput("use_strata", "Stratify by one or more fields", value = FALSE),
        shiny::conditionalPanel(
          "input.use_strata",
          shiny::numericInput("n_strata", "Number of strata fields (REDCap max 14)", value = 2, min = 1, max = 14, step = 1),
          shiny::uiOutput("strata_ui"),
          shiny::radioButtons(
            "strata_alloc_mode",
            "Strata sample-size plan",
            choices = c(
              "Equal share of N (default)" = "equal",
              "Unequal ratio weights" = "ratio",
              "Fixed quota per combination (before buffer)" = "quota"
            ),
            selected = "equal"
          ),
          shiny::conditionalPanel(
            "input.strata_alloc_mode == 'ratio'",
            shiny::helpText("Enter a weight for each coded value. With multiple fields, combination weight = product of the field weights."),
            shiny::uiOutput("strata_ratio_ui")
          ),
          shiny::conditionalPanel(
            "input.strata_alloc_mode == 'quota'",
            shiny::helpText("One planned N (before buffer) per stratum combination. Not available together with group/site stratification — use ratio weights within sites, or turn site off."),
            shiny::uiOutput("strata_quota_ui")
          )
        )
      ),
      shiny::div(
        class = "step-card",
        shiny::h4("B) Randomize by group/site?"),
        help_box("For multi-site studies, stratify by a site field <em>or</em> by Data Access Groups. For DAGs, enter <strong>numeric group IDs</strong> from the Step 2 template (not display names). With equalize on, sites share equal totals unless you set a quota or ratio below."),
        shiny::checkboxInput("use_site", "Stratify by group/site", value = FALSE),
        shiny::conditionalPanel(
          "input.use_site",
          shiny::radioButtons(
            "site_mode",
            NULL,
            choices = c(
              "Existing multiple-choice field" = "field",
              "Data Access Groups (column redcap_data_access_group)" = "dag"
            )
          ),
          shiny::conditionalPanel(
            "input.site_mode == 'field'",
            shiny::textInput("site_field", "Group/site field (REDCap variable)", value = "site", placeholder = "site")
          ),
          shiny::textInput("site_values", "Coded values (site field) or numeric DAG IDs", placeholder = "1,2,3"),
          shiny::radioButtons(
            "site_alloc_mode",
            "Site sample-size plan",
            choices = c(
              "Equal share of N (default)" = "equal",
              "Unequal ratio weights" = "ratio",
              "Fixed quota per site (before buffer)" = "quota"
            ),
            selected = "equal"
          ),
          shiny::conditionalPanel(
            "input.site_alloc_mode == 'ratio'",
            shiny::helpText("One weight per site code."),
            shiny::uiOutput("site_ratio_ui")
          ),
          shiny::conditionalPanel(
            "input.site_alloc_mode == 'quota'",
            shiny::helpText("Planned N before buffer for each site. Buffer is applied on top."),
            shiny::uiOutput("site_quota_ui")
          )
        )
      ),
      shiny::div(
        class = "step-card",
        shiny::h4("Generate and download"),
        shiny::div(
          class = "warn-box",
          shiny::HTML(paste(
            "<strong>REDCap reminders:</strong> no record ID column; dev and prod tables must differ;",
            "upload <code>_dev.csv</code> in Development and <code>_prod.csv</code> in Production;",
            "Setup and Dashboard users cannot stay blinded; mapping files are unblinded only;",
            "one table pair per randomization model (use a distinct filename prefix).",
            "Automatic triggering (Setup Step 4) is configured in REDCap, not in this file."
          ))
        ),
        shiny::actionButton("generate", "Generate tables", class = "btn-primary"),
        shiny::br(), shiny::br(),
        shiny::uiOutput("download_ui"),
        shiny::uiOutput("warnings_ui"),
        shiny::uiOutput("status_ui"),
        shiny::h5("Preview (development table, first 25 rows)"),
        shiny::tableOutput("preview")
      )
    )
  ),
  shiny::div(
    class = "app-footer",
    shiny::div(
      class = "citation",
      "Stevens L, Kennedy N, Taylor RJ, et al. A REDCap advanced randomization module to meet the needs of modern trials. ",
      shiny::em("J Biomed Inform"),
      ". 2025;171:104925. ",
      shiny::tags$a(
        href = "https://doi.org/10.1016/j.jbi.2025.104925",
        target = "_blank",
        rel = "noopener noreferrer",
        "doi:10.1016/j.jbi.2025.104925"
      )
    ),
    shiny::div(
      class = "meta",
      paste0(
        "NIMHANS REDCap Code Library | Randomisation | App Version: ", APP_VERSION,
        " | For inquiries or issues, contact Dr. Bharath Holla"
      )
    )
  )
)

server <- function(input, output, session) {
  result <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$reshuffle_seed, {
    shiny::updateNumericInput(session, "seed", value = as.integer(sample.int(1e8L - 1L, 1L)))
  })

  output$arms_ui <- shiny::renderUI({
    n <- input$n_arms %||% 2
    lapply(seq_len(n), function(i) {
      shiny::fluidRow(
        shiny::column(3, shiny::textInput(
          paste0("arm_code_", i),
          if (i == 1) "Code" else NULL,
          value = shiny::isolate(input[[paste0("arm_code_", i)]] %||% as.character(i))
        )),
        shiny::column(5, shiny::textInput(
          paste0("arm_label_", i),
          if (i == 1) "Label (optional)" else NULL,
          value = shiny::isolate(input[[paste0("arm_label_", i)]] %||% if (i == 1) "Control" else if (i == 2) "Treatment" else "")
        )),
        shiny::column(4, shiny::numericInput(
          paste0("arm_ratio_", i),
          if (i == 1) "Ratio" else NULL,
          value = shiny::isolate(input[[paste0("arm_ratio_", i)]] %||% 1),
          min = 1,
          step = 1
        ))
      )
    })
  })

  output$strata_ui <- shiny::renderUI({
    n <- input$n_strata %||% 1
    lapply(seq_len(n), function(i) {
      shiny::fluidRow(
        shiny::column(6, shiny::textInput(
          paste0("stratum_name_", i),
          if (i == 1) "REDCap variable" else NULL,
          value = shiny::isolate(input[[paste0("stratum_name_", i)]] %||% ""),
          placeholder = "sex"
        )),
        shiny::column(6, shiny::textInput(
          paste0("stratum_codes_", i),
          if (i == 1) "Coded values (comma-separated)" else NULL,
          value = shiny::isolate(input[[paste0("stratum_codes_", i)]] %||% ""),
          placeholder = "1,2"
        ))
      )
    })
  })

  # Live strata list from the A) field rows (same source as generate).
  current_strata <- shiny::reactive({
    if (!isTRUE(input$use_strata)) {
      return(list())
    }
    n <- input$n_strata %||% 0
    out <- list()
    for (i in seq_len(n)) {
      nm <- trim_chr(input[[paste0("stratum_name_", i)]] %||% "")
      codes <- parse_csv_vals(input[[paste0("stratum_codes_", i)]] %||% "")
      if (nzchar(nm) && length(codes) > 0) {
        out[[nm]] <- codes
      }
    }
    out
  })

  current_site_codes <- shiny::reactive({
    if (!isTRUE(input$use_site)) {
      return(character(0))
    }
    parse_csv_vals(input$site_values %||% "")
  })

  output$strata_ratio_ui <- shiny::renderUI({
    strata <- current_strata()
    if (length(strata) == 0) {
      return(shiny::helpText("Add stratum fields and coded values above first."))
    }
    blocks <- lapply(seq_along(strata), function(fi) {
      nm <- names(strata)[[fi]]
      codes <- strata[[fi]]
      header <- shiny::fluidRow(
        shiny::column(4, shiny::tags$strong(paste0(nm, " code"))),
        shiny::column(4, shiny::tags$strong("Weight"))
      )
      rows <- lapply(seq_along(codes), function(ci) {
        id <- paste0("strata_w_", fi, "_", ci)
        shiny::fluidRow(
          shiny::column(4, shiny::tags$div(style = "padding-top: 8px;", codes[[ci]])),
          shiny::column(4, shiny::numericInput(
            id,
            NULL,
            value = shiny::isolate(input[[id]] %||% 1),
            min = 1,
            step = 1
          ))
        )
      })
      shiny::tagList(header, rows, shiny::br())
    })
    shiny::tagList(blocks)
  })

  output$strata_quota_ui <- shiny::renderUI({
    strata <- current_strata()
    if (length(strata) == 0) {
      return(shiny::helpText("Add stratum fields and coded values above first."))
    }
    grid <- factor_grid(strata)
    n_combos <- nrow(grid)
    if (n_combos > 64) {
      return(shiny::div(
        class = "warn-box",
        paste0(
          "Too many combinations (", n_combos,
          ") for the row editor. Reduce coded values, or use the CLI --strata-quota option."
        )
      ))
    }
    field_names <- names(strata)
    n_fields <- length(field_names)
    # Header
    header_cols <- lapply(seq_len(n_fields), function(j) {
      shiny::column(max(2, min(3, floor(8 / n_fields))), shiny::tags$strong(field_names[[j]]))
    })
    header <- do.call(
      shiny::fluidRow,
      c(header_cols, list(shiny::column(4, shiny::tags$strong("Planned N"))))
    )
    equal_share <- max(1L, as.integer(round((input$n %||% 100) / n_combos)))
    rows <- lapply(seq_len(n_combos), function(i) {
      id <- paste0("strata_q_", i)
      code_cols <- lapply(seq_len(n_fields), function(j) {
        shiny::column(
          max(2, min(3, floor(8 / n_fields))),
          shiny::tags$div(
            style = "padding-top: 8px;",
            as.character(grid[i, j])
          )
        )
      })
      do.call(
        shiny::fluidRow,
        c(
          code_cols,
          list(shiny::column(
            4,
            shiny::numericInput(
              id,
              NULL,
              value = shiny::isolate(input[[id]] %||% equal_share),
              min = 1,
              step = 1
            )
          ))
        )
      )
    })
    shiny::tagList(header, rows)
  })

  output$site_ratio_ui <- shiny::renderUI({
    codes <- current_site_codes()
    if (length(codes) == 0) {
      return(shiny::helpText("Enter site / DAG coded values above first."))
    }
    lapply(seq_along(codes), function(i) {
      id <- paste0("site_w_", i)
      shiny::fluidRow(
        shiny::column(4, shiny::tags$div(style = "padding-top: 8px;", paste("Code", codes[[i]]))),
        shiny::column(4, shiny::numericInput(
          id,
          if (i == 1) "Weight" else NULL,
          value = shiny::isolate(input[[id]] %||% 1),
          min = 1,
          step = 1
        ))
      )
    })
  })

  output$site_quota_ui <- shiny::renderUI({
    codes <- current_site_codes()
    if (length(codes) == 0) {
      return(shiny::helpText("Enter site / DAG coded values above first."))
    }
    equal_share <- max(1L, as.integer(round((input$n %||% 100) / length(codes))))
    lapply(seq_along(codes), function(i) {
      id <- paste0("site_q_", i)
      shiny::fluidRow(
        shiny::column(4, shiny::tags$div(style = "padding-top: 8px;", paste("Code", codes[[i]]))),
        shiny::column(4, shiny::numericInput(
          id,
          if (i == 1) "Planned N" else NULL,
          value = shiny::isolate(input[[id]] %||% equal_share),
          min = 1,
          step = 1
        ))
      )
    })
  })

  collect_arms <- function() {
    n <- input$n_arms %||% 2
    codes <- vapply(seq_len(n), function(i) trim_chr(input[[paste0("arm_code_", i)]] %||% ""), character(1))
    labels <- vapply(seq_len(n), function(i) trim_chr(input[[paste0("arm_label_", i)]] %||% ""), character(1))
    ratios <- vapply(seq_len(n), function(i) as.integer(input[[paste0("arm_ratio_", i)]] %||% 1), integer(1))
    list(codes = codes, labels = labels, ratios = ratios)
  }

  collect_strata <- function() {
    if (!isTRUE(input$use_strata)) {
      return(list())
    }
    n <- input$n_strata %||% 0
    out <- list()
    for (i in seq_len(n)) {
      nm <- trim_chr(input[[paste0("stratum_name_", i)]] %||% "")
      codes <- parse_csv_vals(input[[paste0("stratum_codes_", i)]] %||% "")
      if (nzchar(nm)) {
        out[[nm]] <- codes
      }
    }
    out
  }

  collect_site <- function() {
    if (!isTRUE(input$use_site)) {
      return(NULL)
    }
    name <- if (identical(input$site_mode, "dag")) {
      "redcap_data_access_group"
    } else {
      trim_chr(input$site_field %||% "")
    }
    list(name = name, values = parse_csv_vals(input$site_values %||% ""))
  }

  collect_site_quota <- function() {
    if (!isTRUE(input$use_site) || !identical(input$site_alloc_mode, "quota")) {
      return(NULL)
    }
    codes <- current_site_codes()
    if (length(codes) == 0) {
      stop("Enter site / DAG coded values before setting quotas.", call. = FALSE)
    }
    q <- vapply(seq_along(codes), function(i) {
      val <- as.numeric(input[[paste0("site_q_", i)]] %||% NA_real_)
      if (is.na(val) || val <= 0) {
        stop("Each site quota must be a positive number (code ", codes[[i]], ").", call. = FALSE)
      }
      val
    }, numeric(1))
    names(q) <- codes
    q
  }

  collect_site_ratio <- function() {
    if (!isTRUE(input$use_site) || !identical(input$site_alloc_mode, "ratio")) {
      return(NULL)
    }
    codes <- current_site_codes()
    if (length(codes) == 0) {
      stop("Enter site / DAG coded values before setting weights.", call. = FALSE)
    }
    w <- vapply(seq_along(codes), function(i) {
      val <- as.integer(input[[paste0("site_w_", i)]] %||% NA_integer_)
      if (is.na(val) || val < 1) {
        stop("Each site weight must be a positive integer (code ", codes[[i]], ").", call. = FALSE)
      }
      val
    }, integer(1))
    names(w) <- codes
    w
  }

  collect_strata_quota <- function(strata) {
    if (!isTRUE(input$use_strata) || !identical(input$strata_alloc_mode, "quota")) {
      return(NULL)
    }
    if (length(strata) == 0) {
      stop("Add stratum fields before setting quotas.", call. = FALSE)
    }
    grid <- factor_grid(strata)
    if (nrow(grid) > 64) {
      stop("Too many stratum combinations for the row editor; use CLI --strata-quota.", call. = FALSE)
    }
    q <- numeric(0)
    for (i in seq_len(nrow(grid))) {
      val <- as.numeric(input[[paste0("strata_q_", i)]] %||% NA_real_)
      if (is.na(val) || val <= 0) {
        stop("Each stratum quota must be a positive number (row ", i, ").", call. = FALSE)
      }
      key <- if (length(strata) == 1) {
        as.character(grid[i, 1])
      } else {
        paste(as.character(unlist(grid[i, , drop = TRUE])), collapse = "|")
      }
      q[[key]] <- val
    }
    q
  }

  collect_strata_ratio <- function(strata) {
    if (!isTRUE(input$use_strata) || !identical(input$strata_alloc_mode, "ratio")) {
      return(NULL)
    }
    if (length(strata) == 0) {
      stop("Add stratum fields before setting weights.", call. = FALSE)
    }
    out <- list()
    for (fi in seq_along(strata)) {
      nm <- names(strata)[[fi]]
      codes <- strata[[fi]]
      w <- vapply(seq_along(codes), function(ci) {
        val <- as.integer(input[[paste0("strata_w_", fi, "_", ci)]] %||% NA_integer_)
        if (is.na(val) || val < 1) {
          stop("Each strata weight must be a positive integer (", nm, "=", codes[[ci]], ").", call. = FALSE)
        }
        val
      }, integer(1))
      names(w) <- as.character(codes)
      out[[nm]] <- w
    }
    out
  }

  shiny::observeEvent(input$generate, {
    arms <- collect_arms()
    labels <- if (all(!nzchar(arms$labels))) NULL else arms$labels
    block_sizes <- tryCatch(parse_int_csv(input$block_sizes), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error")
      NULL
    })
    if (is.null(block_sizes)) {
      return()
    }
    site_quota <- tryCatch(collect_site_quota(), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error")
      "bad"
    })
    if (identical(site_quota, "bad")) {
      return()
    }
    site_ratio <- tryCatch(collect_site_ratio(), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error")
      "bad"
    })
    if (identical(site_ratio, "bad")) {
      return()
    }
    strata <- collect_strata()
    strata_quota <- tryCatch(collect_strata_quota(strata), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error")
      "bad"
    })
    if (identical(strata_quota, "bad")) {
      return()
    }
    strata_ratio <- tryCatch(collect_strata_ratio(strata), error = function(e) {
      shiny::showNotification(conditionMessage(e), type = "error")
      "bad"
    })
    if (identical(strata_ratio, "bad")) {
      return()
    }
    tryCatch({
      res <- generate_allocation_tables(
        n = as.integer(input$n),
        field = input$field,
        arms = arms$codes,
        ratio = arms$ratios,
        strata = strata,
        site = collect_site(),
        block_sizes = block_sizes,
        buffer = (input$buffer_pct %||% 25) / 100,
        type = input$type,
        arm_labels = labels,
        rand_start_dev = as.integer(input$rand_start_dev %||% 1001),
        rand_start_prod = as.integer(input$rand_start_prod %||% 5001),
        seed = as.integer(input$seed),
        prod_seed = if (!is.null(input$prod_seed) && !is.na(input$prod_seed)) as.integer(input$prod_seed) else NULL,
        prefix = input$prefix,
        code_style = input$code_style %||% "alphanumeric",
        code_length = as.integer(input$code_length %||% 3),
        exclude_ambiguous = isTRUE(input$exclude_ambiguous),
        include_group_col = isTRUE(input$include_group_col),
        include_rand_number = isTRUE(input$include_rand_number),
        equalize = isTRUE(input$equalize),
        site_quota = site_quota,
        site_ratio = site_ratio,
        strata_quota = strata_quota,
        strata_ratio = strata_ratio
      )
      result(res)
      msg <- paste("Generated", nrow(res$dev), "rows for development and production.")
      if (length(res$warnings)) {
        msg <- paste(msg, length(res$warnings), "warning(s) — see log.")
      }
      shiny::showNotification(msg, type = "message")
    }, error = function(e) {
      result(NULL)
      shiny::showNotification(conditionMessage(e), type = "error", duration = 10)
    })
  })

  output$warnings_ui <- shiny::renderUI({
    res <- result()
    if (is.null(res) || !length(res$warnings)) {
      return(NULL)
    }
    shiny::div(
      class = "warn-box",
      shiny::HTML(paste0("<strong>Warnings:</strong><br>", paste(res$warnings, collapse = "<br>")))
    )
  })

  output$status_ui <- shiny::renderUI({
    res <- result()
    if (is.null(res)) {
      return(NULL)
    }
    shiny::div(class = "ok-box", res$log)
  })

  output$preview <- shiny::renderTable({
    res <- result()
    if (is.null(res)) {
      return(NULL)
    }
    utils::head(res$dev, 25)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_ui <- shiny::renderUI({
    res <- result()
    if (is.null(res)) {
      return(shiny::helpText("Generate tables to enable downloads."))
    }
    shiny::tagList(
      shiny::downloadButton("dl_dev", "Development CSV"),
      shiny::downloadButton("dl_prod", "Production CSV"),
      shiny::downloadButton("dl_log", "Generation log"),
      if (!is.null(res$mapping_dev)) {
        shiny::downloadButton("dl_map_dev", "Mapping (dev) — do not upload")
      },
      if (!is.null(res$mapping_dev)) {
        shiny::downloadButton("dl_map_prod", "Mapping (prod) — do not upload")
      }
    )
  })

  prefix_or <- function() {
    res <- result()
    if (is.null(res)) "RandomizationAllocationTemplate" else res$params$prefix
  }

  output$dl_dev <- shiny::downloadHandler(
    filename = function() paste0(prefix_or(), "_dev.csv"),
    content = function(file) write_csv_redcap(result()$dev, file)
  )
  output$dl_prod <- shiny::downloadHandler(
    filename = function() paste0(prefix_or(), "_prod.csv"),
    content = function(file) write_csv_redcap(result()$prod, file)
  )
  output$dl_log <- shiny::downloadHandler(
    filename = function() paste0(prefix_or(), "_log.txt"),
    content = function(file) writeLines(result()$log, file, useBytes = TRUE)
  )
  output$dl_map_dev <- shiny::downloadHandler(
    filename = function() paste0(prefix_or(), "_mapping_dev.csv"),
    content = function(file) write_csv_redcap(result()$mapping_dev, file)
  )
  output$dl_map_prod <- shiny::downloadHandler(
    filename = function() paste0(prefix_or(), "_mapping_prod.csv"),
    content = function(file) write_csv_redcap(result()$mapping_prod, file)
  )
}

shiny::shinyApp(ui, server)

### REDCap randomization allocation tables

This folder generates the CSV allocation tables that REDCap Randomization Setup Step 3 expects. Define the model here (arms, strata, optional group/site), then upload `_dev.csv` while the project is in Development and `_prod.csv` when it is in Production.

REDCap does not create the random sequence. It uses your table as a lookup list: the next unused row that matches the record's stratum (and site, if used) is assigned.

These tools follow guidance from the [Seattle Children's REDCap randomization help](https://redcap.seattlechildrens.org/surveys/?s=3oa2yzEgQrpXp4wy) and the [REDCap advanced randomization module paper](https://pmc.ncbi.nlm.nih.gov/articles/PMC12977000/) (REDCap 14.7+).

#### What improved vs the previous script

- Strata, arms, block sizes, and output paths are no longer hardcoded.
- Permuted blocks are generated **within each stratum × site combination**.
- Extra columns such as `block_no` are omitted. A `notes` column from the Step 2 template is ignored by REDCap if present.
- **Development and production tables must differ** — REDCap rejects identical uploads; the generator auto-reshuffles production if needed.
- Blinded randomization uses unique alphanumeric concealment codes; optional `redcap_randomization_group` for admin troubleshooting.
- Open randomization can optionally include `redcap_randomization_number` for the `[rand-number]` smart variable.
- Strata explosion warnings, DAG ID validation, role/blinding notes in the generation log.
- A 25% buffer is added by default so drop-ins do not exhaust the list.

Setup Step 4 (automatic triggering when logic becomes true) is configured in REDCap, not in these files. If you use strata, those fields must already be saved before auto-randomization can run.

#### Files

1. `generate_allocation.R` — core generator (base R).
2. `REDCap_Randomize.R` — command-line entry point; launches Shiny if run with no arguments.
3. `app.R` — Shiny UI that follows Setup Step 1 A/B/C (strata, group/site, randomization field).
4. `manifest.json` — R package list for [Posit Connect Cloud](https://docs.posit.co/connect-cloud/how-to/r/dependencies.html). Primary file: `Randomisation/app.R`. Regenerate with `rsconnect::writeManifest(appPrimaryDoc = "app.R")` from this folder.

#### Shiny app

```bash
cd Randomisation
Rscript REDCap_Randomize.R --shiny
```

Or from R:

```r
install.packages("shiny")   # once
shiny::runApp("Randomisation")
```

Fill in the randomization field name, allocation type (open vs blinded), group codes, optional strata, and optional site/DAG values using the **same variable names and coded values** as the template downloaded from Setup Step 2. Generate, inspect the balance log, then download the CSVs.

#### Command line

```bash
# Same model as the original script (N=120, sex and age_gp)
Rscript REDCap_Randomize.R \
  --n 120 \
  --field randomize \
  --strata "sex=1,2;age_gp=1,2" \
  --block-sizes 4,6,8 \
  --seed 42 \
  --outdir ./output

# Open allocation with optional audit numbers ([rand-number] smart variable)
Rscript REDCap_Randomize.R \
  --n 120 \
  --field randomize \
  --include-rand-number \
  --strata "sex=1,2" \
  --seed 42 \
  --outdir ./output

# Blinded, alphanumeric concealment codes (INT/CNT example)
Rscript REDCap_Randomize.R \
  --n 160 \
  --field redcap_randomization_number \
  --type blinded \
  --arms INT,CNT \
  --strata "gender=1,2" \
  --block-sizes 4,6,8 \
  --code-length 3 \
  --seed 2025 \
  --prod-seed 4525 \
  --outdir ./output

# Data Access Groups — numeric IDs only (from Step 2 template)
Rscript REDCap_Randomize.R --n 120 --field randomize --dag "1,2,3" --outdir ./output

# Second randomization model in the same project (platform / SMART trial)
Rscript REDCap_Randomize.R \
  --n 80 \
  --field rescue_arm \
  --prefix RescueAllocation \
  --strata "sex=1,2" \
  --outdir ./output

Rscript REDCap_Randomize.R --help
```

| Option | Meaning |
| --- | --- |
| `--n` | Target sample size |
| `--buffer` | Extra rows as a proportion (default `0.25`) |
| `--field` | Randomization field (Setup Step 1C) |
| `--type` | `open` (group codes) or `blinded` (concealment codes) |
| `--arms` / `--ratio` / `--arm-labels` | Allocation codes, ratio, optional labels |
| `--strata` | `name=v1,v2;name2=v1,v2` (Setup Step 1A; max 14 fields) |
| `--site` | `name=v1,v2` (Setup Step 1B, a field) |
| `--dag` | Numeric DAG group IDs from the Step 2 template |
| `--block-sizes` | Multiples of the ratio total; default `4,6,8` for 1:1 |
| `--include-rand-number` | Open only: add `redcap_randomization_number` column |
| `--code-style` / `--code-length` | Concealed code style (blinded mode) |
| `--no-group-col` | Omit `redcap_randomization_group` from upload CSV |
| `--seed` / `--prod-seed` | Development seed; production default is `seed + 104729` |
| `--outdir` / `--prefix` | Output location; **one prefix per randomization model** |

#### Output files

- `{prefix}_dev.csv` — upload in Development.
- `{prefix}_prod.csv` — upload for Production (must differ from dev).
- `{prefix}_log.txt` — seeds, model, warnings, balance counts, role/blinding notes.
- `{prefix}_mapping_dev.csv` / `_mapping_prod.csv` — blinded mode only; unblinded statistician / pharmacy only.

#### User roles (configure in REDCap, not in this tool)

| Privilege | Who | Blinding |
| --- | --- | --- |
| Setup | Statistician / DM | Must see allocation tables |
| Dashboard | PI / PM | Cannot stay blinded |
| Randomize | Coordinator / survey | May be blinded if concealed |

#### Method

For every combination of stratum codes (and group/site if used), the generator draws random block sizes, fills each block with the allocation ratio, permutes the block, and concatenates blocks until that combination has at least `ceil(N × (1 + buffer) / combinations)` rows. REDCap assigns the next unused matching row in list order.

Open randomization writes group codes into the randomization column. Blinded randomization writes unique alphanumeric codes into the text field and, by default, adds `redcap_randomization_group` with the arm. Mapping CSVs remain an unblinded audit copy.

Multiple randomization models (platform trials, SMART designs) require **separate generator runs** with different `--field` and `--prefix` values — one upload pair per model.

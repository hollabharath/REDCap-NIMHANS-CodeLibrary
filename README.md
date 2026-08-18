# REDCap-NIMHANS-CodeLibrary

NIMHANS REDCap management scripts for common project setup tasks.

## Randomisation

Build **development** and **production** allocation CSVs for the REDCap Randomization module (Setup Step 3). The generator uses permuted-block randomization within each stratum and optional site/DAG combination; REDCap assigns the next unused matching row from your uploaded table.

| How to run | Link |
| --- | --- |
| **Cloud app** (no install) | [hollabharath-redcap-randomise.share.connect.posit.cloud](https://hollabharath-redcap-randomise.share.connect.posit.cloud/) |
| **Local Shiny** | `cd Randomisation && Rscript REDCap_Randomize.R --shiny` |
| **Command line** | `Rscript REDCap_Randomize.R --n 120 --field randomize --strata "sex=1,2" --outdir ./output` |

Supports open and concealed (blinded) allocation, optional strata (up to 14 fields), multi-site/DAG stratification, and separate dev/prod tables with a built-in buffer for drop-ins.

Full documentation: [`Randomisation/README.md`](Randomisation/README.md)

## Translations

Multi-Language Management (MLM) helpers for exporting, editing, and re-importing REDCap translation JSON.

See [`Translations/README.md`](Translations/README.md)

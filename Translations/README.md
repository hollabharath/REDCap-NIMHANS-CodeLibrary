### Documentation for Multi-Language Management (MLM) Translation Scripts

This folder contains scripts and resources related to managing translations for REDCap projects

#### Scripts Overview

1. 01_MLM_json2csv.py
   - Purpose: Converts a JSON file from REDCap into a CSV format to simplify the translation process.
   - Usage:
     ```
     python 01_MLM_json2csv.py <path/to/original/jsonfile.json>
     ```
     - Arguments:
       - `<path/to/original/jsonfile.json>`: The path to the JSON file that needs to be converted to CSV.

2. 02_MLM_csv2json.py
   - Purpose: Converts a CSV file back into JSON format after translations have been applied, preparing it for re-importation into REDCap.
   - Usage:
     ```
     python 02_MLM_csv2json.py <path/to/original/jsonfile.json> <path/to/updated/csvfile.csv>
     ```
     - Arguments:
       - `<path/to/original/jsonfile.json>`: The path to the original JSON file used as a template.
       - `<path/to/updated/csvfile.csv>`: The path to the CSV file containing the updated translations.

3. 03_MLM_report_translated_forms.py
   - Purpose: Generates a report detailing which forms have been translated, assisting in tracking and managing the translation process.
   - Usage:
     ```
     python 03_MLM_report_translated_forms.py <path/to/updated/jsonfile.json>
     ```
     - Arguments:
       - `<path/to/updated/jsonfile.json>`: The path to the JSON file with the translations applied.

4. 04_MLM_update_translation_settings.py
   - Purpose: Generates a translation settings JSON that can be imported in REDCap MLM module's general settings. This will ensure that translations are enabled correctly across the project.
   - Usage:
     ```
     python 04_MLM_update_translation_settings.py <settings_json_file_path> <forms_csv_file_path> <language_tag>
     ```
     - Arguments:
       - `<settings_json_file_path>`: The path to the JSON file containing translation settings (Exported from MLM general settings) .
       - `<forms_csv_file_path>`: The path to the CSV file listing the forms to be updated.
       - `<language_tag>`: A language tag (e.g., "kn", "hi" etc) to apply to the translation settings.

import json
import csv
import sys

def update_settings(settings_file_path, forms_csv_path, language):
    # Load the original settings
    with open(settings_file_path, 'r') as file:
        settings = json.load(file)
    
    # Ensure en-US is True for all forms currently in dataEntryEnabled
    for form, languages in settings['dataEntryEnabled'].items():
        languages['en-US'] = True
    
    # Read form names from the CSV file
    with open(forms_csv_path, newline='') as csvfile:
        reader = csv.reader(csvfile)
        next(reader, None)  # Skip the header row
        forms_with_translations = [row[0] for row in reader]

    # Update the `dataEntryEnabled` for the specified language in each relevant form from the CSV
    # Also ensure these forms have en-US set to True
    for form in forms_with_translations:
        if form not in settings['dataEntryEnabled']:
            settings['dataEntryEnabled'][form] = {'en-US': True}  # en-US is True by default
        
        # Update or add the specified language, enabling it
        settings['dataEntryEnabled'][form][language] = True

    # Save the updated settings back to a new JSON file
    updated_file_path = os.path.splitext(settings_file_path)[0] + "_updated.json"
    with open(updated_file_path, 'w') as file:
        json.dump(settings, file, indent=4)

    print(f"Updated settings have been saved to {updated_file_path}")

# Ensure the correct number of arguments are provided
if len(sys.argv) != 4:
    print("")
    print("")
    print(f"Usage: python {sys.argv[0]} <settings_json_file_path> <forms_csv_file_path> <language_tag>")
    print("")
    print("")
else:
    settings_file_path = sys.argv[1]
    forms_csv_path = sys.argv[2]
    language_tag = sys.argv[3]
    update_settings(settings_file_path, forms_csv_path, language_tag)

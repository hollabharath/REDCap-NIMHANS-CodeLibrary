import json
import csv
import os
import sys

def update_json_with_csv(original_json_path, updated_csv_path):
    # Load the original JSON data
    with open(original_json_path, 'r', encoding='utf-8') as file:
        data = json.load(file)

    # Read the updated CSV file
    with open(updated_csv_path, newline='', encoding='utf-8') as csvfile:
        reader = csv.DictReader(csvfile)
        csv_data = list(reader)

    # Mapping from CSV back to JSON
    for field in data.get('fieldTranslations', []):
        if 'label' in field:  # Check if 'label' key exists
            # Find CSV rows corresponding to the current JSON field
            field_rows = [row for row in csv_data if row['field_id'] == field['id'] and row['enum_id'] == '']
            # Update the field's label translation if any rows match
            if field_rows:
                field['label']['translation'] = field_rows[0]['translation']
                field['label']['hash'] = field_rows[0]['hash'][1:]  # Remove the '#' from the hash

        # Update enum translations if any
        if 'enum' in field:
            for enum in field['enum']:
                enum_rows = [row for row in csv_data if row['field_id'] == field['id'] and row['enum_id'] == str(enum['id'])]
                if enum_rows:
                    enum['translation'] = enum_rows[0]['translation']
                    enum['hash'] = enum_rows[0]['hash'][1:]  # Remove the '#' from the hash

    # Write the updated JSON data back to a file
    updated_json_path = os.path.splitext(original_json_path)[0] + "_updated.json"
    with open(updated_json_path, 'w', encoding='utf-8') as file:
        json.dump(data, file, ensure_ascii=False, indent=4)  # ensure_ascii=False to allow Unicode characters to be written directly

    print(f"Updated JSON file has been created at {updated_json_path}")

if __name__ == "__main__":
    if len(sys.argv) < 3:
        print("")
        print("")
        print("Please provide the path to the original JSON file and the updated CSV file as arguments.")
        print(f"Example usage: python {sys.argv[0]} <path/to/original/jsonfile.json> <path/to/updated/csvfile.csv>")
        print("")
        print("")
        sys.exit(1)

    original_json_path = sys.argv[1]
    updated_csv_path = sys.argv[2]
    update_json_with_csv(original_json_path, updated_csv_path)

import json
import csv
import os
import sys

def convert_json_to_csv(json_filepath):
    # Read the JSON file
    with open(json_filepath, 'r') as file:
        data = json.load(file)

    # Extract the directory and basename for the output CSV
    dir_name = os.path.dirname(json_filepath)
    base_name = os.path.splitext(os.path.basename(json_filepath))[0]
    csv_filepath = os.path.join(dir_name, base_name + ".csv")

    # Prepare to write to CSV
    with open(csv_filepath, 'w', newline='') as csvfile:
        fieldnames = ['field_id', 'form', 'enum_id', 'hash', 'prompt', 'default', 'translation']
        writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
        
        writer.writeheader()
        
        # Process each field in the JSON data
        for field in data.get('fieldTranslations', []):
            label = field.get('label', {})
            # Add '#' to hash value
            writer.writerow({
                'field_id': field.get('id', ''),
                'form': field.get('form', ''),
                'enum_id': '',  # Empty for parent field
                'hash': '#' + label.get('hash', ''),    # Prepend '#' to hashes to ensure Excel treats them as text; this prefix will be removed when converting back to JSON.
                'prompt': label.get('prompt', ''),
                'default': label.get('default', ''),
                'translation': label.get('translation', '')
            })
            
            # Handle enumerated fields if they exist
            enums = field.get('enum', [])
            for enum in enums:
                writer.writerow({
                    'field_id': field.get('id', ''),    # Parent field id
                    'form': field.get('form', ''),
                    'enum_id': enum.get('id', ''),      # Unique id for the enum choice
                    'hash': '#' + enum.get('hash', ''), # Prepend '#' to hashes to ensure Excel treats them as text; this prefix will be removed when converting back to JSON.
                    'prompt': enum.get('prompt', ''),
                    'default': enum.get('default', ''),
                    'translation': enum.get('translation', '')
                })

    print(f"CSV file has been created at {csv_filepath}")

# Example usage: python script.py path/to/your/jsonfile.json
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("")
        print("")
        print("Please provide the JSON file path as an argument.")
        print(f"Example usage: python {sys.argv[0]} <path/to/original/jsonfile.json>")
        print("")
        print("")
        sys.exit(1)

    json_filepath = sys.argv[1]
    convert_json_to_csv(json_filepath)

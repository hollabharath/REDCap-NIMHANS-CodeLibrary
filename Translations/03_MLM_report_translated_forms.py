import json
import csv
import os
import sys

def extract_unique_forms(json_file_path):
    basename = os.path.basename(json_file_path)  
    name_without_extension = os.path.splitext(basename)[0] 
    output_csv_path = f"unique_forms_{name_without_extension}.csv"  

    # Set to store unique form names
    unique_forms = set()
    
    # Open and load the JSON file
    with open(json_file_path, 'r') as file:
        data = json.load(file)
        
        # Check "fieldTranslations" exists and proceed
        if "fieldTranslations" in data:
            # Iterate through each entry in the "fieldTranslations" array
            for entry in data["fieldTranslations"]:
                # Check if "form" key exists and "label" translation is not empty
                if "form" in entry and entry.get("label", {}).get("translation", "").strip():
                    # Add the form name to the set (duplicates are automatically handled)
                    unique_forms.add(entry["form"])
    
    # Write the unique form names to the constructed CSV file path
    with open(output_csv_path, 'w', newline='') as csvfile:
        formwriter = csv.writer(csvfile)
        formwriter.writerow(['Form Names'])  # Header row
        for form_name in unique_forms:
            formwriter.writerow([form_name])

    print(f"Unique forms have been extracted to {output_csv_path}")

# Check if the input JSON file path is provided
if len(sys.argv) != 2:
    print("")
    print("")
    print("Please provide the updated JSON file path as an argument.")
    print(f"Example usage: python {sys.argv[0]} <path/to/updated/jsonfile.json>")
    print("")
    print("")
else:
    input_json_file_path = sys.argv[1]
    extract_unique_forms(input_json_file_path)

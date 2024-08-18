import os

def combine_jsonl_files(output_file, *input_files):
    # Ensure the directory exists
    os.makedirs(os.path.dirname(output_file), exist_ok=True)
    
    with open(output_file, 'w', encoding='utf-8') as outfile:
        for file in input_files:
            with open(file, 'r', encoding='utf-8') as infile:
                for line in infile:
                    outfile.write(line)

# Define the paths to your input and output files
test_file = 'data/raw/test.jsonl'
train_file = 'data/raw/train.jsonl'
valid_file = 'data/raw/valid.jsonl'
combined_file = 'data/processed/combined.jsonl'

# Combine the files
combine_jsonl_files(combined_file, test_file, train_file, valid_file)
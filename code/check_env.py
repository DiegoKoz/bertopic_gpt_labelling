import os
print("Current Working Directory:", os.getcwd())

file_path = 'data/biology_abstracts.csv'
if os.path.exists(file_path):
    print("File found")
else:
    print("File not found")

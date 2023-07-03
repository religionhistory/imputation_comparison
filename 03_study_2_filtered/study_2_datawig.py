
# Load libraries
import pandas as pd
import numpy as np
import datawig
import os
import glob

# Load data
path = "../output/study2/datawig_data/"
files = glob.glob(os.path.join(path, "*.csv"))

# Check if output directory exist
output_dir = ("../output/study2/datawig/")
check_folder = os.path.isdir(output_dir)

# If directory doesn't exist, then create it.
if not check_folder:
    os.makedirs(output_dir)
    print("created folder : ", output_dir)

else:
    print(output_dir, "folder already exists.")

# Impute data
def imputeDatawig(data):
    imputed = datawig.SimpleImputer.complete(data)
    return imputed

for file in files:
    
    # Read data
    df = pd.read_csv(file)
    
    # Impute data
    imputed = imputeDatawig(df)
    
    # Get filename and create output filename
    fileName = os.path.basename(file).split('.')[0]
    newFileName = 'datawig_' + fileName + '.csv'
    newPath = '../output/study2/datawig/'
    newFilePath = newPath + newFileName
    
    # Save imputed data
    df.to_csv(newFilePath, index=False)

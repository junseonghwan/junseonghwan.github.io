# python code for reading in the data file

import numpy as np
import pandas as pd

import argparse
import os
import subprocess
import time
import sys

# code to process command line arguments
parser = argparse.ArgumentParser(description='Process file destination.')
parser.add_argument('dest', help="Specify the destination directory.", type=str)
parser.add_argument('master_file', help="Specify the master file downloaded from ENA.", type=str)
parser.add_argument('patient_id', help="Specify the patient ID. E.g. KTN102.", type=str)
parser.add_argument('project_id', help="Project ID.", type=str)
parser.add_argument('--debug', help="Enter yes/no. If no is provided, it will only print the command", type=str)
parser.add_argument('--time', help="Number of hours per job.", type=str, default="4:00:00")
parser.add_argument('--job_name', help="Job name.", type=str, default="wget_sc")
args = parser.parse_args()

# read in the master file and extract the files that we want
df = pd.read_table(args.master_file, sep='\t')
patient_ids = df['library_name'].astype(str).str[0:6]
patients = set(patient_ids)
#print(sorted(patients)) 
#print(np.sum(patient_ids == args.patient_id)) # there are 511 files associated with this patient
# there are 21 patients, but there are supposed to be only 20 patients according to the paper
# KTN609 does not appear in the appendix of the paper: https://www.cell.com/cms/attachment/2119295259/2091819478/mmc1.pdf

# extract records corresponding to the specified patient
df2 = df.loc[patient_ids == args.patient_id]
#print(sorted(set(df2['library_name'].astype(str))))

# make a directory if it does not exist alrady
if not os.path.exists(args.dest):
	os.mkdir(args.dest)

# make a directory for patient_id if it does not exist already
download_dest = os.path.join(args.dest, args.patient_id)
print(download_dest)
if not os.path.exists(download_dest):
	os.mkdir(download_dest)

# generate a directory to hold just the scRNA data files to be downloaded
rna_download_dest = os.path.join(download_dest, "scRNA")
if not os.path.exists(rna_download_dest):
	os.mkdir(rna_download_dest)

# generate the log files for each download job
logs = os.path.join(rna_download_dest, "logs")
if not os.path.exists(logs):
	os.mkdir(logs)

# code to download all RNA data files
for idx, row in df2.iterrows():
	fastq_ftp_loc = row['fastq_ftp']
	locs = fastq_ftp_loc.split(";")
	library_name = row['library_name']
	if len(locs) == 1: # scRNA data are in one fastq.gz file
		accession_number = row["run_accession"]

		# retrieve the file name and form the destination path
		loc_split = locs[0].split("/") 
		dest_file_name = loc_split[len(loc_split)-1]		
		final_dest = os.path.join(rna_download_dest, dest_file_name)

		command = "sbatch -A %s -p core -n 1 -t %s -J %s -o %s/%s.out -e %s/%s.err worker.sbatch %s %s" % (args.project_id, args.time, args.job_name, logs, accession_number, logs, accession_number, locs[0], final_dest)
		if (args.debug is None or args.debug == "no"):
			print(command)
		else:
			# submit the job
			subprocess.call([command], shell=True)
		

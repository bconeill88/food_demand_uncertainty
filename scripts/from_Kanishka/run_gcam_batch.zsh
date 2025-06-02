#!/bin/zsh
#SBATCH -A im3
#SBATCH -t 96:00:00
#SBATCH -N 1
#SBATCH -p slurm
#SBATCH --array=1-10  # Adjust this based on the number of scenarios


# Define the CSV file containing the scenarios
scenario_file="scenarios.csv"

# Select the scenario based on the array task ID (assuming the file has one scenario per line)
scenario=$(awk "NR==$SLURM_ARRAY_TASK_ID" $scenario_file)

job=$(awk "NR==$SLURM_ARRAY_TASK_ID" $scenario_file | tr -d '\r\n')

echo 'Library config:'
ldd ./gcam.exe
 
date
time ./gcam.exe -C configuration_$job.xml -Llog_conf.xml
date

time Rscript /qfs/people/nara836/food_demand_mult_cons/food_demand_Waldhoffetal2024/Rgcam/Generate_tables.R $job 
date

import numpy as np
import pandas as pd
import subprocess
import os

# Create some sample data (a simple periodic pattern)
period_len = 96
n_periods = 5
x = np.linspace(0, 2*np.pi*n_periods, period_len*n_periods)
data = np.sin(x) + np.random.normal(0, 0.1, len(x))

# Save data to CSV
pd.DataFrame({'value': data}).to_csv('test_data.csv', index=False)

# Create R script
r_script = """
# Read the data
data <- read.csv('test_data.csv')$value

# Source the required functions
source('sncirc/R/SN-circ-general.R')

# Run analysis
result <- sncirc(
    data=data,
    period.len=96,
    dist="Normal mean",
    max.cpts=5,
    minseglen=1,
    pen.val=3*log(length(data))
)

# Save results
write.csv(result$op.cpt.loc, 'results.csv', row.names=FALSE)
"""

# Save R script
with open('run_analysis.R', 'w') as f:
    f.write(r_script)

# Run R script
subprocess.run(['Rscript', 'run_analysis.R'], check=True)

# Read and print results
if os.path.exists('results.csv'):
    results = pd.read_csv('results.csv')
    print("Optimal changepoint locations:")
    print(results.values)
else:
    print("Analysis failed to produce results")

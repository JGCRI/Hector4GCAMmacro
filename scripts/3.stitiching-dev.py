# This script assumes that the stitches-emulator pacakge has already been installed
# and that the package data has already been installed with stitches.install_package_data()

# TODOs
#  - Make it so that can change the model in a single line instead of having to make mulitple  changes through out the script.

# Set Up
import stitches as stitches
import pandas as pd
import pkg_resources
import os
from matplotlib import pyplot as plt


# Load emulated Hector time series, for now we are considering the
# ssp245 pathway.
df = pd.read_csv('output/MRI-ESM2-0.csv')

# Normalize the temperature data to stitiches reference period
# which is 1995 to 2014
ref_years = df[(df['year'] >= 1995) & (df['year'] <= 2014)]
ref_value = ref_years.value.mean()
df.value = df.value - ref_value


# Make the stitches compatible data frame
target_data = pd.DataFrame({"variable": "tas", "experiment": "ssp245", "ensemble": "NA",
                            "model": "Hector", "year": df.year,  "value": df.value,
                            "unit": "degC change from avg over 1995~2014"})

# Now set up the target data in the form that can actually be read into the stitiches function.
x = stitches.fx_processing.chunk_ts(df=target_data, n=9, base_chunk=8)
input = target_data = stitches.fx_processing.get_chunk_info(x)


# Now set up the archive we can use in the matching process. (Select a single model)
# read in the package data of all ESMs-Scenarios-ensemble members avail.
# Let's exclude ssp245 since that is our target pathway
data_directory = pkg_resources.resource_filename("stitches", "data")
path = os.path.join(data_directory, "matching_archive.csv")
data = pd.read_csv(path)

archive_data = data.loc[
    (data["experiment"].isin(["ssp119", "ssp126", "ssp370", "ssp585", "ssp460", "ssp434"]))
    & (data["model"] == "MRI-ESM2-0")
].copy()



# This is the stitiches porition of the workflow!
my_recipes = stitches.make_recipe(input, archive_data, tol=0.06,
                                  N_matches=4, reproducible=True, non_tas_variables=["pr", "hurs","rsds"])

stitched_global_temp = stitches.gmat_stitching(my_recipes)
# This is the gridded product step it takes much longer than the global temp
# so let's leave it commented out for now.
# stitches.gridded_stitching(out_dir='.',  rp=my_recipes)



# Now let's compare the stitched global means with the actual ESM data
data_directory = pkg_resources.resource_filename("stitches", "data")
data_path = os.path.join(data_directory, "tas-data", "MRI-ESM2-0_tas.csv")

comp_data = pd.read_csv(data_path)
comp_data = comp_data.loc[comp_data["experiment"] == "ssp245"]


# full ensemble of actual ESM runs:
groups = comp_data.groupby("ensemble")
for name, group in groups:
    if group.ensemble.unique() == "x":
        plt.plot(group.year, group.value, color="black", linewidth=2.0)
    else:
        plt.plot(group.year, group.value, color="0.5", alpha=0.5)

# The stitched realizations:
groups = stitched_global_temp.groupby("stitching_id")
for name, group in groups:
    plt.plot(group.year, group.value, linewidth=1.0, label=name)

plt.legend()
plt.xlabel("Year")
plt.ylabel("C")
plt.title("Stitched Global Mean Temperature vs MRI-ESM2-0 Results")
plt.show()
plt.close()
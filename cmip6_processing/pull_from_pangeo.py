import fsspec
import intake
import xarray as xr
from tqdm import tqdm

url = "https://storage.googleapis.com/cmip6/pangeo-cmip6.json"
out = intake.open_esm_datastore(url)
out.df.to_csv("pangeo_catalog.csv", index=False)
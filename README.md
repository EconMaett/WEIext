# US Weekly Economic Index: Replication and extension

Replication data for Wegmueller, P., Glocker, C. (2023): US Weekly Economic Index: Replication and extension (replication data). Version: 1. Journal of Applied Econometrics. Dataset. https://doi.org/10.15456/jae.2023075.0909504723.

All files are zipped in the file [replica_uswei.zip](https://doi.org/10.15456/jae.2023075.0909504723).

## Data

### Confidential data
- [Staffing Index](https://americanstaffing.net/asa/contact-asa/)
- [Withholding Tax](https://www.taxtracking.com)
- [Rasmussen Consumer Index](https://www.rasmussenreports.com/econ/econ_page)
- [EEI](https://www.eei.org/about-eei/About#contactus)
- [Johnson Redbook](https://www.redbookresearch.com/8733.html)

### Publicly available data
Downloaded from Macrobond and stored in Excel-Data
- [Railroad traffic](https://fred.stlouisfed.org/series/RAILFRTINTERMODALD11)
- [Fuel sales](https://www.eia.gov/petroleum/gasdiesel/)
- [Steel production](https://www.steel.org/industry-data/)
- [Jobless Claims, Initial](https://fred.stlouisfed.org/series/ICSA)
- [Jobless Claims, Continuing](https://fred.stlouisfed.org/series/CCSA)

Downloaded from [Alfred](https://alfred.stlouisfed.org/) to obtain vintages
- [US GDP](https://alfred.stlouisfed.org/series?seid=GDPC1)

## Scripts
- `main.R`: source code
- `calendar_dates.R`: load regressors for calendar day adjustment
- `load_data.R`: load raw data
- `narrow.R`: Narrow replication of US WEI
- `wide.R`: Wide replication of US WEI
	
### Seasonal adjustment of HF-data
- `eei.R`		 
- `withhold.R`	 
- `staffing.R`
- `uslama3294.R`
- `uslama3394.R`
- `ulsama4628.R`
- `usprod0685.R`
- `ustran0035.R`
- `usprod0983.R`
- `consumer.R`
	
- `_remove_outliers.R`: Outlier adjustment
	
- `_scaling_to_us_gdp_yoy.R`: Scaling to US GDP

- `_oos_nomod.R`: Out-of-sample analysis
- `_oos_evaluation.R`: generates figures and tables from out-of-sample data

## Results
- `data.Rdata`: Publicly available data
- `us_wei.Rdata`: Weekly indices
- `bench_rt.Rdata`: Benchmarks
- `oos_rt.Rdata`: OoS-Simulations

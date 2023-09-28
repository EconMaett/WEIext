Philipp Wegmueller and Christian Glocker, 
"US Weekly Economic Index: Replication and Extension", 
Journal of Applied Econometrics, forthcoming.

All files are zipped in the file replica_uswei.zip.

# ========= DATA ========= 

# Confidential data
- Staffing Index		-> 	https://americanstaffing.net/asa/contact-asa/
- Withholding Tax		-> 	https://www.taxtracking.com
- Rasmussen Consumer Index	-> 	https://www.rasmussenreports.com/econ/econ_page
- EEI				-> 	https://www.eei.org/about-eei/About#contactus
- Johnson Redbook 		-> 	https://www.redbookresearch.com/8733.html

# Publicly available data	->	Downloaded from Macrobond and stored in Excel-Data
- Railroad traffic
- Fuel sales
- Steel production
- Jobless Claims, Initial
- Jobless Claims, Continuing

- US GDP 			-> 	Downloaded from Alfred to obtain vintages

# ========= SCRIPTS ========= 

- main.R 		-> 	source code
- calendar_dates.R 	-> 	load regressors for calendar day adjustment
- load_data.R 		-> 	load raw data
- narrow.R		-> 	Narrow replication of US WEI
- wide.R		-> 	Wide replication of US WEI
	#--- Seasonal adjustment of HF-data ---
	- eei.R		 
	- withhold.R	 
	- staffing.R
	- uslama3294.R
	- uslama3394.R
	- ulsama4628.R
	- usprod0685.R
	- ustran0035.R
	- usprod0983.R
	- consumer.R
	#--- Outlier adjustment ---
	- _remove_outliers.R
	#--- Scaling to US GDP ---
	_scaling_to_us_gdp_yoy.R

- _oos_nomod.R		-> 	Out-of-sample analysis
- _oos_evaluation.R 	-> 	generats figures and tables from out-of-sample data

# ========= RESULTS .RDATA ========= 
- data.Rdata		-> 	Publicly available data
- us_wei.Rdata		-> 	Weekly indices
- bench_rt.Rdata 	-> 	Benchmarks
- oos_rt.Rdata		-> 	OoS-Simulations


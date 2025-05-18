This project holds the codes needed to replicate the masters thesis project in R.
The folder does not include raw data which is under NDA.
The files are structured as following:
dataRetrieving: fetching the data through API and saving it locally
dataHandling: cleaning the data and combining additional information from xls
dataAnalysis: creating descriptive statistics of the data
survAnalysis: survival analysis calculation, done only for pathologist data
hus_rdd_for_ki67: regression discontinuity design for pathologist data, includes CFR's
ai_rdd_for_ki67: regression discontinuity design for ALGORITHMIC data, includes CFR's.

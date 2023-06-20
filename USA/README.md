# Data extraction for the USA 

The USA has a number of different resources for obtaining respiratory surveillance data. Within this collection I currently download, merge and clean data from the following sources:

- [FluNet](https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html) one can simply visit this page hit download data and download all seasons. This provides clinical and public health lab data extracted directly from NREVSS (the National Resipratory and Enteric Virus Surveillance system) on influenza cases between 1997-2022. The downloaded datasets are split into 2015-2016 and prior seasons and post 2015-2016. Post 2015-2016 CDC decided to separate clinical and public health labs data rather than combining as done previously.

- [NREVSS](https://www.cdc.gov/surveillance/nrevss/index.html) contains data on several respiratory viruses. [RSV data](https://data.cdc.gov/Laboratory-Surveillance/Respiratory-Syncytial-Virus-Laboratory-Data-NREVSS/52kb-ccu2), which is currently included in this code, is collected from approximately 600 public health and clinical labs across the US covering the period of 2010-2020. This data also provides information on the diagnostic test used (antigen or PCR). 

- [ILINet](https://gis.cdc.gov/grasp/fluview/main.html) provides influenza like illness data over the period of 1997-2022. I have not currently cleaned and merged this data yet, but will in future. 

The script data_cleaning_USA.R requires the user to download the data from the above links and replace the read_csv() commands in the script to the location of where the downloaded data sits on your PC. 
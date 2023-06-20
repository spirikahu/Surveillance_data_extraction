# Data extraction for the USA 

The USA has a number of different resources for obtaining respiratory surveillance data. Within this collection I currently download, merge and clean data from the following sources:

- [FluNet](https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html) one can simply visit this page hit download data and download all seasons. This provides clinical lab and public health lab data extracted directly from NREVSS (the National Resipratory and Enteric Virus Surveillance system) on influenza cases between 1997-2022. The downloaded datasets are split into 2015-2016 and prior seasons and post 2015-2016. Prior to the 2015-2016 season CDC combined the number of cases recorded by clinical and public health labs, later deciding to separate out the data by lab.    

- [NREVSS](https://www.cdc.gov/surveillance/nrevss/index.html) is the National respiratory and enteric virus surveillance system which contains data on several respiratory viruses. [RSV data](https://data.cdc.gov/Laboratory-Surveillance/Respiratory-Syncytial-Virus-Laboratory-Data-NREVSS/52kb-ccu2), which is currently included in this code, is collected from approximately 600 public health and clinical labs across the US covering the period of 2010-2020. This data also provides information on the diagnostic test used (antigen or PCR). 

- [ILINet](https://gis.cdc.gov/grasp/fluview/main.html) provides influenza like illness data over the period of 1997-2022. I have not currently cleaned and merged this data yet, but will in future. 
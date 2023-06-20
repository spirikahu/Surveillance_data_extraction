# Data extraction for Canada

Respiratory data for Canada can be found here [https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada.html](https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada.html) 
and can be accessed directly from the webpage or by downloading a pdf report. 

In the script data_extraction_and_cleaning_Canada.R we scrape each weekly report from the url above, pull out the html nodes representing tables and then extract Table 1 from each url which provides the total number of cases of each respiratory virus and number of tests performed by state. As I am only interested in national totals I only extract Canada for the Reporting Laboratory. This code extracts all respiratory data over the period 2014-2023.

Also contained within this folder is a data dictionary for the clean and merged data this code will create and a text document with the abbreviations provided in each weekly report. 
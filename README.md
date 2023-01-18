

https://zenodo.org/badge/517971832.svg




# Drought impact text-mining and drought impact statements dataset

This repository contains code and data on our project on assessing socio-economic drought impacts from newspaper articles for Germany between 2000-2021. The code is structured in an R targets pipeline. The dataset is provided as .csv file and can be cited with a DOI. For details on both, read below. 

The code & data are part of a pre-print currently under review ([see here](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4178096)).

Also, we have published and are still working on an [interactive data exploration tool](https://jsodoge.shinyapps.io/drought_impact_monitor/).

This repository is structured as follows: 
* The code is organized within an R targets pipeline (https://books.ropensci.org/targets/) to ensure reproducibility. It divides the process of creating a text corpus, extracting locations and impacts into different steps (i.e. functions) which are ran subsequently. The targets pipeline is defined in _targets_ file in the main directory. Functions are defined in the R folder. 
* Running the targets pipeline creates the figures within the _figures_ folder
* The raw data cannot be shared within this repository. Please get in touch for questions and data related issues. However, we cannot guarantee that all data can be shared (for license) reasons.
* The folder final_dataset holds the resulting dataset of drought impacts. See description and dataset structure below.




## Drought impact statements (DIS) dataset: Drought impacts in Germany between 2000-2020

The drought impact text mining was applied to, and resulted in a dataset of drought impacts in Germany between 2000-2020. The dataset can be found in the __final_dataset__ folder and is structured as follows as a tabular dataset:
  *  "nuts_id": using the NUT classification system to locate the drought impact   
  *  "statistical_unit": in addition to the NUTS identifier, this variable indicates the NUTS-level (1/2/3)   
  *  "type_of_class": categorizes the typ of drought impact. Keywords are used to indicate impact class, we provide more detailed definitions below.
  *   "month_date" and "year_date": provide temporal identifiers for the particular drought impact and location
  *   "DIS" (drought impact statement): provides the sum of all drought impact statements for a particular location (nuts_id) and time (month_date and year_date). Can be used as a measure of prevalence/significance of the drought impact
 


 

 
 Here, see an overview of the classified drought impacts.
 
![alt text](https://github.com/jansodoge/drought-impact-text-mining/blob/main/updated_treemap_readme_figure.png)

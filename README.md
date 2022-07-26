# Drought-impact-text-mining
Repository on code for classifying and locating drought impacts from newspaper articles. Also, provides and documents a resulting dataset of drought impacts across different sectors in Germany between 2000-2020.

This repository is structured as follows: 
* The code is organized within an R targets pipeline (https://books.ropensci.org/targets/) to ensure reproducibility. It divides the process of creating a text corpus, extracting locations and impacts into different steps (i.e. functions) which are ran subsequently. The targets pipeline is defined in _targets_ file in the main directory. Functions are defined in the R folder. 
* Running the targets pipeline creates the figures within the _figures_ folder
* The raw data cannot be shared within this repository. Please get in touch for questions and data related issues. However, we cannot guarantee that all data can be shared (for license) reasons.
* The folder final_dataset holds the resulting dataset of drought impacts. See description and dataset structure below.



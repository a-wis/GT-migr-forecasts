# <p align="center"> Now-casting Romanian Migration into the United Kingdom by Using Google Search Engine Data </p>
  
### <p align="center">Andreea Avramescu<sup>1</sup>, [Arkadiusz Wiśniowski](https://www.research.manchester.ac.uk/portal/a.wisniowski.html)<sup>2*</sup>
</p>
  
<p align="center">
<sup>1</sup> Alliance Manchester Business School, University of Manchester, </br> Booth Street West, M15 6PB, Manchester, UK
</p>


<p align="center">
<sup>2</sup> Department of Social Statistics, School of Social Sciences, University of Manchester,  </br> Oxford Road, M13 9PL, Manchester, UK

<sup>*</sup> [Corresponding author](mailto:a.wisniowski@manchester.ac.uk)
</p>
  
> ## Repository structure

```
GT-migr-forecasts/                        
├── Code                          <- R the code used for the analysis          
|   ├── analysis.R                       <- contains setting up of simulations and results saving, setting up data for output                             
|   ├── data_prep.R                      <- reads in, cleans and prepares data for analysis, also includes plots                            
|   ├── functions_modelling_plotting.R   <- includes functions for analysis and plotting that are sourced in `analysis.R`                            
|   └── outputs.R                        <- contains code for producing all tables and figures presented in the manuscript  
|
├── Data                          <- all data used for the analysis in CSV and XLSX formats  
|    ├── GT_cluster_dictionary.csv              
|    ├── GT_data                  <- the data used to create the Google Trends Index               
|    |   ├── Control_Cluster.xlsx     
|    |   ├── Education_Cluster.xlsx   
|    |   ├── Employment_Cluster.xlsx       
|    |   ├── Housing_Cluster.xlsx       
|    |   ├── Pound_Cluster.xlsx 
|    |   └── GT Data Compendium
|    ├── GT_data.csv                            
|    └── IPS_data.xlsx  
|
├── Graphs                    
|
├── Models                  
|    ├── tsmodel011.rds / stan                                              
|    ├── tsmodel011rw.rds / stan                                            
|    ├── tsmodel021.rds / stan                                             
|    └── tsmodel021rw.rds / stan   
|
├── Output                        <- outputs of the analysis (with analysis.R)                                             
|    ├── results_30.RData                <- models with data starting 2013                       
|    ├── results_31.RData                <- models with data starting 2012                       
|    ├── results_32.RData                <- models with data starting 2013 but using differences, not levels                       
|    ├── results_all.RData               <- simulations for all models and years for which forecasts were made            
|    └── results022018_30.RData  
│
├── LICENSE                       <- files excluded from git version control 
└── README.md                     <- information about the repo
```

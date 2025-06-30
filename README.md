# party_shield

Replication code and data for Bishof et. al. 'Using the Party as a Shield? How British MPs Explain Policy Positions to Constituents'. The material is shared in two folders, described below:

## R Folder

The `R` folder contains two files which share the code for the paper, each of these two files has an associated markdown file:

*  main_analysis.R (associated .Rmd file 'Figs_and_Tables.Rmd). This file contains the main replication code for the paper. It utilizes the data we will share publicly to generate the figures and tables in the paper (including in the appendix). The code is also used by the Figs_and_Tables.Rmd to generated the attached pdf which also contains the main figures and tables [This code does not generate the following tables from the paper and SI: power analysis (see code below), Table 1 (coded party positions on nine issues), Tables SI 1 & 2 with implementation details]
*  power_analysis.R (associated .Rmd file 'power_analysis.Rmd'). This file contains the replication code for the power analysis. I've kept this code separate because it is quite time consuming  to run. 


Nothing that we aren't happy to share is on github. However, the `R` folder contains a number of other R files which use the versions of the data which we don't want to share publicly. We will eventually delete these files (with relevant code that we want to share be transferred to the main_analysis.R code). To run this code requires users to have access to our Dropbox folder. This is achieved by running the code in the a data_prep_local.R file, a version of which can be found on Dropbox in the . This data_prep_local.R  will need editing to add your local machine information. It is not on github because I didn't want to publicly share information about the set-up of anyone's machine. This file should be saved to the R folder on your local machine, it simply sets two variables: d.path (path to the folder which contain our BA_MP_Experiment folder - usually the root Dropbox folder) and a.path (the path to the analysis folder in Analysis folder.

The other main other files in the R folder are :

*  data_prep.R This is the code loads data from dropbox with a full set of variables so they can be used . If run in full it also creates and saves the reduced versions of the dataset used to convert the data we have on dropbox to the data which we can share publicly. 
*  additional_analysis_R+R.R (associated .Rmd file additional_analysis.Rmd): contains code for the additional analysis we need to do/might do for the R&R (plus a 'bin' of other code scraps from the previous analysis we did not end up using). This file uses the full data loaded by data_prep.R (not the reduced versions of the data stored in the Data folder.

##  Data Folder
The `Data` folder contains two datasets, where the aim is for these datasets to be the versions that we share publicly. These datasets are at the outset of the 'Figs_and_Tables.pdf'.

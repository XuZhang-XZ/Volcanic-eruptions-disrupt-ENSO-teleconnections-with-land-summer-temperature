# Volcanic-eruptions-disrupt-ENSO-teleconnections-on-land-summer-temperature

This respotiry includes all code that generate main results in our manuscript.

## 1. System requirements
R (4.4.1) and RStudio (2024.09.1) are required to run the code.

## 2. Installation guide
Please open the file named "Project.Rproj" using RStudio.

## 3. Instruction
Here we explain the scripts in the "Code" folder.

### 3.1 Load packages and functions.
The codes, entitled "0. library.r" in the “Load packages” folder, aims to load packages we used.

- 0. library.r\
The codes load all packages that are used in this study. Meanwhile, the functions in "functions" folder are sourced for following calculation.\
Then, you can easily identify the source code of any unknown functions by ctrl + left click.

### 3.2 Analyses for data
The code in "Analyses Code" folder generate main results in our main text

-- 11. ENSO and Eruption Data.R\
The code read ENSO and eruption data.\
-- 12. Global Temperature Data.R\
The code read global mean temperature. \
-- 13. Global Correlations.R\
The code calculate the correlations between global temperature and ENSO.\
-- 14. Correlation in moving months.R\
The correlations in moving 3-month periods are calcualte for previous and next years.\
-- 15. Correlation using different Temp and Vol dataset.R\
We validate our results using alternative tempearture and volcanic data. \
-- 21. Gridded Cor_Alternative Temperature.R\
Calculate correlations at gridded resolution.\
-- 41. Reconstruct ENSO_Alternative Temperature.R\
We reconstruct ENSO using land temperature as predictors.\
-- 81. Examine filters.R\
We conduct experiments for pass fitlers at different frequencies.\
-- 91. Plot Divergence.R\
Plot Fig. 1\
-- 92. Correlation Summary.R\
Plot Fig. 2\
-- 94. Plot comparison with CMIP6.R\
Plot Fig. 4\
-- 95. Plot reconstructed ENSO.R\
Plot Fig. 5\

## 4. Notes
4.1. All the figures will be saved to the folder named "Figures"\
4.2. Before running our code, please make sure that all packages specified in the "0. library.R" have been installed.\
4.3. Readers are required to download relevant data in the "Data Availability" section.\

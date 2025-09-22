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

-- 0. library.r\
The codes load all packages that are used in this study. Meanwhile, the functions in "functions" folder are sourced for following calculation. Then, you can easily identify the source code of any unknown functions by ctrl + left click.

### 3.2 Analyses for data
The code in "Analyses Code" folder generate main results in our main text

-- 11. ENSO and Eruption Data.R\
The code read ENSO and eruption data.\
-- 12. Global Temperature Data.R\
The code read global mean temperature. \
-- 13. Global Correlations.R\
The code calculate the correlations between global temperature and ENSO.\
-- 21. Gridded Correlations.R\
Calculate correlations at gridded resolution.\
-- 41. Reconstruct ENSO.R\
We reconstruct ENSO using land temperature as predictors.\
-- 91. Plot Divergence.R\
Plot Fig. 1\
-- 92. Correlation Summary.R\
Plot Fig. 2\
-- 93. Plot Zonal GP.R\
Plot Fig. 3\
-- 94. Plot comparison with CMIP6.R\
Plot Fig. 4\
-- 95. Plot reconstructed ENSO.R\
Plot Fig. 5\

## 4. Notes
4.1. All the figures will be saved to the folder named "Figures"\
4.2. Before running our code, please make sure that all packages specified in the "0. library.R" have been installed.\
4.3. Readers are required to download relevant data in the "Data Availability" section.\
4.4. The source data for figures are available in the "Source Data.xlsx"\

Shield: [![CC BY-NC 4.0][cc-by-nc-shield]][cc-by-nc]

This work is licensed under a
[Creative Commons Attribution-NonCommercial 4.0 International License][cc-by-nc].

[![CC BY-NC 4.0][cc-by-nc-image]][cc-by-nc]

[cc-by-nc]: https://creativecommons.org/licenses/by-nc/4.0/
[cc-by-nc-image]: https://licensebuttons.net/l/by-nc/4.0/88x31.png
[cc-by-nc-shield]: https://img.shields.io/badge/License-CC%20BY--NC%204.0-lightgrey.svg

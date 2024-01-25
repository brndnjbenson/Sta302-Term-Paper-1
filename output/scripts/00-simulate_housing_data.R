---
title: "Simulation on Analysis on Toronto units of housing."
author: "Brandon Benson"
format: html
editor: visual
---

```{r}
#### Preamble ####
# Purpose: Simulate the number of social housing and affordable housing between 2020 and 2022 quarterly.
# Author: Brandon Benson
# Date: January 25 2024
# Contact: brandon.benson@mail.utoronto.ca
# Pre-requisites: -


#### Workspace setup ####
install.packages("opendatatoronto")
install.packages("knitr")
install.packages("janitor")

library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
```


```{r}
#### Simulate data ####
set.seed(250)

# set up the year quarters 
quarters <- rep(c("Q1","Q2", "Q3", "Q4"))
year <- rep((2020:2022), each = 4)


simulated_data <- tibble(
  Quarter = paste(quarters, year, sep = " "),
  Social_Housing_units = round(runif(12, 50000, 100000)),
  Affordable_Housing_units = round(runif(12, 50000, 100000))
)

simulated_data

```

```{r}
# Three tests to determine the validity of the simulated code.

# First test ensures that there are four quarters in a year before advancing to the next year.

unique_quart <- unique(simulated_data$Quarter)

qrtr_per_yr <- sapply(strsplit(unique_quart, " "), function(x) x[1])
yr_per_qrtr <- sapply(strsplit(unique_quart, " "), function(x) x[2])

num_qrtr_in_year <- table(yr_per_qrtr)

if(!all(num_qrtr_in_year == 4)) {
  print("Error: Not all years have four quarters before the next year")
}

# Second test focuses on the structure of the simulated data.

if(!"tbl_df"%in% class(simulated_data)) {
  stop("Error: data is not a tibble")
}

if(ncol(simulated_data) != 3){
  stop("Error: Incorrect number of columns in data")
}

if(nrow(simulated_data) != 12){
  stop("Error: Incorrect number of rows in data")
}

columns_data <- c("Quarter", "Social_Housing_units", "Affordable_Housing_units")
if (!all(columns_data %in% colnames(simulated_data))){
  stop("Error: Missing columns in data")
}
# Third test ensure that the data range is within the minimum and maximum values. 

if (!all(simulated_data$Social_Housing_units >= 50000 & simulated_data$Social_Housing_units <= 100000)) {
  stop("Error: Values in a column is not within the expected range")
}

if (!all(simulated_data$Affordable_Housing_units >= 50000 & simulated_data$Affordable_Housing_units <= 100000)) {
  stop("Error: Values in a column is not within the expected range")
}

```


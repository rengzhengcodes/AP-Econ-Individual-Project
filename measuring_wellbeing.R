# install necessary functions
install.packages(c("readxl", "tidyverse", "reshape2"))
# activates those libraries
library(readxl)
library(tidyverse)
library(reshape2)

# imports the data from Excel file | https://www.core-econ.org/doing-economics/book/text/04-03.html#r-walk-through-41-importing-the-excel-file-xlsx-or-xls-format-into-r
setwd("C:/Users/theni/OneDrive/Documents/Coding/Repos/AP-Econ-Individual-Project")
getwd()
UN = read_excel(# file name
                "./Download-GDPPCgrowth-USD-countries.xlsx",
                # sheet name
                "Download-GrowthRateGDPPC-USD",
                # number of rows to skip
                skip = 2
                )

head(UN)


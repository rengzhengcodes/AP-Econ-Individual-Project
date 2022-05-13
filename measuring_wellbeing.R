# install necessary functions
install.packages(c("readxl", "tidyverse", "reshape2"))
# activates those libraries
library(readxl)
library(tidyverse)

# imports the data from Excel file | https://www.core-econ.org/doing-economics/book/text/04-03.html#r-walk-through-41-importing-the-excel-file-xlsx-or-xls-format-into-r
setwd("C:/Users/theni/OneDrive/Documents/Coding/Repos/AP-Econ-Individual-Project")
getwd()
UN = read_excel(# file name
                "./Download-GDPconstant-USD-countries.xlsx",
                # sheet name
                "Download-GDPconstant-USD-countr",
                # number of rows to skip
                skip = 2
                )

head(UN)

# frequency table, also making data long
library(reshape2)
wide_UN <- UN
# keeping all data except for Country ID
wide_UN = wide_UN[, -1]

# id.vars are the names of the column variables
long_UN = melt(wide_UN, id.vars = c("Country", "IndicatorName"), value.vars = 4:ncol(UN))

head(long_UN)

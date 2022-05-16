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

# changes variable column to Year label
names(long_UN)[names(long_UN) == "variable"] <- "Year"
# extracts only the Final consumption expenditure subfield
cons = subset(long_UN, IndicatorName == "Final consumption expenditure")

# pip operator (%>%) is from tidyverse. It means use the result of the current line as the first argument in the next line's fxn.

# Gets the number of missing years by country
missing_by_country = cons %>%
  group_by(Country) %>%
  summarize(available_years=sum(!is.na(value))) %>%
  print()

# gets num of countries with full data
sum(missing_by_country$available_years == max(missing_by_country$available_years))

# shortens variable name for future readability
long_UN$IndicatorName[long_UN$IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)"] <- "HH.expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == "General government final consumption expenditure"] <-
  "Gov.Expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == "Gross capital formation"] <- "Capital"

long_UN$IndicatorName[long_UN$IndicatorName == "Imports of goods and services"] <- "Imports"

long_UN$IndicatorName[long_UN$IndicatorName == "Exports of goods and services"] <- "Exports"

# reshape long_UN data so there's only one row per country and per year
table_UN <- dcast(long_UN, Country + Year ~ IndicatorName)

# adds a new column for net exports
table_UN$Net.Exports <- table_UN[, "Exports"]-table_UN[, "Imports"]

# check via taking 3 countries
sel_countries = c("Brazil", "United States", "China")

# using long format dataset, we get imports, exports, and year for these countries
sel_UN1 = subset(table_UN,
                 subset = (Country %in% sel_countries),
                 select = c("Country", "Year", "Exports", "Imports", "Net.Exports"))
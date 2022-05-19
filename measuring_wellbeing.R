# https://www.core-econ.org/doing-economics/book/text/04-03.html#r-walk-through-44-plotting-and-annotating-time-series-data

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
long_UN$IndicatorName[long_UN$IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)"] <- "HH.Expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == "General government final consumption expenditure"] <-
  "Gov.Expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == "Final consumption expenditure"] <- "Final.Expenditure"

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

head(sel_UN1)

# Select the chosen countries
comp = subset(long_UN, Country %in% c("United States", "China"))

# value in billion of USD
comp$value = comp$value / 1e9

comp = subset(comp, select = c("Country", "Year", "IndicatorName", "value"),
              subset = IndicatorName %in% c("Gov.Expenditure", "HH.Expenditure", "Capital", "Imports", "Exports"))

library(ggplot2)
# ggplot allows us to build a chart step-by-step.
p1 = ggplot(subset(comp, Country == "United States"),
            # Base chart, defining x (horizontal) and y (vertical)
            # axis variables
            aes(x = Year, y = value))

# Specify a line chart, with a different colour for each indicator name and line size = 1
p1 = p1 + geom_line(aes(group = IndicatorName, color = IndicatorName), size = 1)

#display the chart
p1

# Repeat all steps without subsetting data
p1 = ggplot(comp, aes(x = Year, y = value, color = IndicatorName))
p1 = p1 + geom_line(aes(group = IndicatorName), size = 1)
p1 = p1 + scale_x_discrete(breaks = seq(1970, 2016, by = 10))
p1 = p1 + scale_y_continuous(name = "Billion US$")
p1 = p1 + ggtitle("GDP components over time")
p1 = p1 + scale_colour_discrete(name = "Component")
p1 = p1 + theme_bw()

# Make a separate chart for each country
p1 = p1 + facet_wrap(~Country)
p1 = p1 + scale_colour_discrete(
  name = "Components of GDP",
  labels = c("Gross capital formation",
             "Exports", "Government expenditure", "Household expenditure", "Imports"))
p1

# Reshape the data to wide format (indicators in columns)
comp_wide <- dcast(comp, Country + Year ~ IndicatorName)

head(comp_wide)

# Add the new column for net exports = exports - imports
comp_wide$Net.Exports <- comp_wide[, "Exports"] - comp_wide[, "Imports"]

head(comp_wide)

# Return to long format with the HH.expenditure, Capital, and Net Export variables
comp2_wide <- subset(comp_wide, select = -c(Exports, Imports))

comp2 <- melt(comp2_wide, id.vars = c("Year", "Country"))

props = comp2 %>% group_by(Country, Year) %>% mutate(proportion = value / sum(value))

# Base line chart
p1 = ggplot(props, aes(x = Year, y = proportion, color = variable))

p1 = p1 + geom_line(aes(group = variable), size = 1)

p1 = p1 + scale_x_discrete(breaks = seq(1970, 2016, by = 10))

p1 = p1 + ggtitle("GDP component proportions over time")

p1 = p1 + theme_bw()

# Make a separate chart for each country
p1 = p1 + facet_wrap(~Country)

p1 = p1 + scale_colour_discrete(
  name = "Components of GDP",
  labels = c("Gross capital formation", "Government expenditure", 
             "Household expenditure", "Net Exports"))

p1

# Calculate proportions
table_UN$p_Capital <- table_UN$Capital / 
  (table_UN$Capital
  + table_UN$Final.Expenditure
  + table_UN$Net.Exports)
table_UN$p_FinalExp <- table_UN$Final.Expenditure / 
  (table_UN$Capital
   + table_UN$Final.Expenditure
   + table_UN$Net.Exports) 
table_UN$p_NetExports <- table_UN$Net.Exports / 
  (table_UN$Capital
   + table_UN$Final.Expenditure
   + table_UN$Net.Exports)

sel_countries <- c("Germany", "Japan", "United States", "Albania", "Russian Federation",
                   "Ukraine", "Brazil", "China", "India")

# Using our long format dataset, we select imports, exports, and year for our chosen countries in 2015.
sel_2015 <- subset(table_UN, subset = (Country %in% sel_countries) & (Year == 2015),
                   select = c("Country", "Year", "p_FinalExp", "p_Capital", "p_NetExports"))

# Reshape the table into long format, then use ggplot
sel_2015_m <- melt(sel_2015, id.vars = c("Year", "Country"))

g <- ggplot(sel_2015_m, aes(x = Country, y = value, fill = variable)) + 
  geom_bar(stat="identity") + coord_flip() + ggtitle("GDP component proportions in 2015") +
  scale_fill_discrete(name = "Components of GDP",
                      labels = c("Final expenditure",
                                 "Gross capital formation",
                                 "Net Exports")) +
  theme_bw()

plot(g)

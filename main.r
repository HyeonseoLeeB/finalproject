rm(list = ls())
########################################################
#0. libraries and renv
########################################################
renv::restore() # used to recreate project environment

library(httpgd)
library(readxl)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(data.table)
library(dtplyr)
library(fixest)
library(modelsummary)
library(fredr)

########################################################
#1. Importing and cleaning data using data.table
########################################################
#######1.1 OECD data
proc<-as.data.table(read_excel("data/OECD_cpi.xlsx", sheet = "data_cpi"))
long_cpi <- melt(proc, id.vars = "country", value.name = "cpi", variable.name = "year")

unique_countries <- unique(long_cpi$country)
print(unique_countries)

proc<-as.data.table(read_excel("data/OECD_cpicore.xlsx", sheet="data_cpicore"))
long_cpicore <- melt(proc, id.vars = "country",value.name ="cpicore", variable.name = "year")

proc<-as.data.table(read_excel("data/OECD_exrate.xlsx", sheet = "data_exrate"))
long_exrate <- melt(proc, id.vars = "country", value.name = "exrate", variable.name = "year")

proc<-as.data.table(read_excel("data/OECD_nominalwage.xlsx", sheet = "data_nominalwage"))
long_nominalwage <- melt(proc, id.vars = "country", value.name = "nominalwage", variable.name = "year")

proc<-as.data.table(read_excel("data/OECD_vacancy_unemploy.xlsx", sheet = "data_vacancy"))
long_vacancy <- melt(proc, id.vars = "country", value.name = "vacancy", variable.name = "year")

proc<-as.data.table(read_excel("data/OECD_vacancy_unemploy.xlsx", sheet = "data_unemploy"))
long_unemploy <- melt(proc, id.vars = "country", value.name = "unemploy", variable.name = "year")

master <- merge(long_cpi, long_cpicore, by = c("country", "year"), all = TRUE)
master <- merge(master, long_exrate, by = c("country", "year"), all = TRUE)
master <- merge(master, long_nominalwage, by = c("country", "year"), all = TRUE)
master <- merge(master, long_vacancy, by = c("country", "year"), all.y = TRUE)
master <- merge(master, long_unemploy, by = c("country", "year"), all = TRUE)

master$year<-as.integer(master$year) + as.numeric(min(as.character(master$year)))-1 #make year into integer variable


#######1.2 Importing and merging Fred US unemployment data using api 
#(because in OECD dataset, US unemployment data is missing)
source("apikeys.r")
fredr_set_key(fred_api_key)

usvacancy <- fredr(
  "JTSJOL", #Job Openings: Total Nonfarm, Level in Thousands, Monthly, Seasonally Adjusted
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = "a" 
  )
usvacancy <- as.data.frame(usvacancy[,c(1,3)])
colnames(usvacancy) <- c("date", "vacancy")

usunemploy <- fredr(
  "UNEMPLOY", #Unemployment Level, Thousands of Persons, Monthly, Seasonally Adjusted
  observation_start = as.Date("2000-01-01"),
  observation_end = as.Date("2023-01-01"),
  frequency = "a" #annual average
  )
usunemploy <- as.data.frame(usunemploy[,c(1,3)])
colnames(usunemploy) <- c("date", "unemploy")

#filling in missing US unemployment data
dt <- merge(usvacancy, usunemploy, by = "date", all = TRUE)
library(lubridate)
dt$year <- year(dt$date)
dt$country <- "United States"
dt<-as.data.table(dt)
master <- merge(master, dt, by = c("year", "country"), all.x=TRUE)
master[country == "United States", unemploy.x := unemploy.y*1000]
master[, unemploy.y := NULL]
master[, vacancy.y := NULL]
master[, date := NULL]
setnames(master, "unemploy.x", "unemploy")
setnames(master, "vacancy.x", "vacancy")
master0 <- master

#######1.3 Remove countries with missing values
master <- master0  #in case I have to run code again from here
master <- master[year >= 2000]
master <- master[year < 2023]

##Chose country to drop by looking at the data. 
master <- master[master$country != "Australia", ]
master <- master[master$country != "Belgium", ]
master <- master[master$country != "Denmark", ]
master <- master[master$country != "Estonia", ]
master <- master[master$country != "Greece", ]
master <- master[master$country != "Ireland", ]
master <- master[master$country != "Israel", ]
master <- master[master$country != "Japan", ]
master <- master[master$country != "Lithuania", ]
master <- master[master$country != "Netherlands", ]
master <- master[master$country != "New Zealand", ]
master <- master[master$country != "Slovak Republic", ]
master <- master[master$country != "Slovenia", ]
master <- master[master$country != "Spain", ]
master <- master[master$country != "United Kingdom", ]

unique_countries <- unique(master$country)
print(unique_countries)

#######1.4 Make variables for regression
master$voveru <- master$vacancy/master$unemploy
master$logvoveru <- log(master$voveru)
master$supply <- master$cpi - master$cpicore
master$wage_exadj <- master$nominalwage/master$exrate

# Create lag and growth variables within each country
setorder(master, country, year) # Order the data.table by 'country' and 'year'
master[, nominalwage_lag := shift(nominalwage), by = country]
master[, wagegrowth := log(nominalwage) - log(nominalwage_lag), by = country]
master[, wagegrowth_lag := shift(wagegrowth), by = country]
master[, wage_exadj_lag := shift(wage_exadj), by = country]
master[, wagegrowth2 := log(wage_exadj) - log(wage_exadj_lag), by = country]
master[, wagegrowth2_lag := shift(wagegrowth2), by = country]
master[, cpi_lag := shift(cpi), by = country]

#dummy when voveru is above its mean, by country
master <- master %>%
  group_by(country) %>%
  mutate(voveru_mean = mean(voveru, na.rm = TRUE),
         voveru_tight = ifelse(voveru > voveru_mean, 1, 0)) %>%
  ungroup()

master$voveru_tight_interaction <- master$voveru*master$voveru_tight

# Hodrick-Prescott filter within each country
library(dplyr)
library(mFilter)
frequency <- 1 #annual
master <- master %>%
  group_by(country) %>%
  mutate(voveru_gap = hpfilter(voveru, freq = frequency)$cycle) %>%
  ungroup()


#View(master) #to see dataset

########################################################
#2. Regression
########################################################
#Panel Regressions using feols
library(fixest)

cpi_fe1 <- feols(cpi ~ voveru + supply + year | country, data = master, cluster = "country")
cpi_fe1

cpi_fe2 <- feols(cpi ~ voveru + supply + cpi_lag + year | country, data = master, cluster = "country")
cpi_fe2

cpi_fe3 <- feols(cpi ~ voveru + voveru_tight_interaction + supply + year | country, data = master, cluster = "country")
cpi_fe3

cpi_fe4 <- feols(cpi ~ voveru + voveru_tight_interaction + supply + cpi_lag + year | country, data = master, cluster = "country")
cpi_fe4


wage_fe1 <- feols(wagegrowth ~ voveru + supply + year | country, data = master, cluster = "country")
wage_fe1

wage_fe2 <- feols(wagegrowth ~ voveru + supply + wagegrowth_lag + year | country, data = master, cluster = "country")
wage_fe2

wage_fe3 <- feols(wagegrowth ~ voveru + voveru_tight_interaction + supply + year | country, data = master, cluster = "country")
wage_fe3

wage_fe4 <- feols(wagegrowth ~ voveru + voveru_tight_interaction + supply + wagegrowth_lag + year | country, data = master, cluster = "country")
wage_fe4

etable(cpi_fe1, cpi_fe2, cpi_fe3, cpi_fe4)
etable(wage_fe1, wage_fe2, wage_fe3, wage_fe4)


#Export result to latex
library(texreg)
models_list <- list(cpi_fe1, cpi_fe2, cpi_fe3, cpi_fe4)
texreg(models_list, file = "forlatex/output_cpi.tex")

models_list <- list(wage_fe1, wage_fe2, wage_fe3, wage_fe4)
texreg(models_list, file = "forlatex/output_wage.tex")


########################################################
#3. Graphs
########################################################
library(dplyr)
library(ggplot2)
library(purrr)
library(patchwork)

# Graph of voveru and cpi for each countries: generate and export
countries <- c("Austria", "Czechia", "Finland", "France", "Germany", "Hungary", "Luxembourg", "Norway",
               "Poland", "Portugal", "Sweden", "Switzerland", "Türkiye", "United States")

generate_graph <- function(country_name) {
  master_country <- master %>%
    filter(country == country_name)
  
  ggplot(data = master_country, aes(x = voveru, y = cpi, label = year)) +
    geom_point() +  
    geom_smooth(method = "lm", se = FALSE, color = "blue") +  
    geom_text(size = 3, hjust = -0.2, vjust = 0.5) +  # Add text labels for "year"
    labs(x = "voveru", y = "CPI", title = paste("voveru vs. CPI for", country_name)) +  
    theme_minimal()
}

map(countries, function(country_name) {
  # Generate the graph
  p <- generate_graph(country_name)
  ggsave(filename = file.path("graph", paste(country_name, ".jpeg", sep = "")), plot = p, width = 8, height = 6, dpi = 300)
})
graphs <- map(countries, generate_graph)
composite_graph <- wrap_plots(graphs, ncol = 4)
ggsave(filename = "graph/All_graphs.jpeg", plot = composite_graph, width = 12, height = 10, dpi = 300)


# Graph of voveru and wage inflation for each countries: generate and export
countries <- c("Austria", "Czechia", "Finland", "France", "Germany", "Hungary", "Luxembourg", "Norway",
               "Poland", "Portugal", "Sweden", "Switzerland", "Türkiye", "United States")

generate_graph <- function(country_name) {
  master_country <- master %>%
    filter(country == country_name)
  
  ggplot(data = master_country, aes(x = voveru, y = wagegrowth, label = year)) +
    geom_point() +  
    geom_smooth(method = "lm", se = FALSE, color = "#119311dd") +  
    geom_text(size = 3, hjust = -0.2, vjust = 0.5) +  # Add text labels for "year"
    labs(x = "voveru", y = "wagegrowth", title = paste(country_name)) +  
    theme_minimal()
}

map(countries, function(country_name) {
  # Generate the graph
  p <- generate_graph(country_name)
  ggsave(filename = file.path("graph", paste(country_name, "_wage.jpeg", sep = "")), plot = p, width = 8, height = 6, dpi = 300)
})
graphs <- map(countries, generate_graph)
composite_graph <- wrap_plots(graphs, ncol = 4)
ggsave(filename = "graph/All_graphs_wage.jpeg", plot = composite_graph, width = 12, height = 10, dpi = 300)


########################################################
#4.Unit test
########################################################
library(testthat)
testthat::local_edition(3)
testthat::test_dir("tests")

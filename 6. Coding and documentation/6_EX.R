# Libraries required
check_and_install_packages <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package)
      library(package, character.only = TRUE)
    }
  }
}
packages_needed <- c("readxl", "stringr", "dplyr", "Hmisc")
check_and_install_packages(packages_needed)

# Load data
setwd("C:\\Users\\dziub\\RRcourse2023\\6. Coding and documentation")
task_data = read.csv("Data\\onet_tasks.csv")
file_path <- "Data\\Eurostat_employment_isco.xlsx"

# Define variables and task items / This is where we define which countries we do research for and what kind of research we do (items).
countries <- c("Belgium", "Spain", "Poland")
isco_list <- list()
task_items <- c("t_4A2a4", "t_4A2b2", "t_4A4a1")

# I. Read employment data from Eurostat
for(i in 1:9) {
  sheet_name <- paste("ISCO", i, sep="")
  isco_list[[sheet_name]] <- read_excel(file_path, sheet=sheet_name)
}

#II. sum()
summarize_country_quarterly <- function(country, isco_list) {
  total <- isco_list[[1]][[country]]
  for(i in 2:length(isco_list)) {
    total <- total + isco_list[[i]][[country]]
  }
  return(total)
}

totals_list <- lapply(countries, function(country) {
  summarize_country_quarterly(country, isco_list)
})

names(totals_list) <- paste0("total_", countries)
list2env(totals_list, envir = globalenv())

# III. Merge - auto
for (i in 1:length(isco_list)) {
  isco_list[[i]]$ISCO <- i
}

all_data <- do.call(rbind, isco_list)

# IV adding a vector that is 9 times the previously calculated totals
for (country in countries) {
  total_value <- get(paste0("total_", country))
  rep_total <- rep(total_value, times = 9)
  all_data[[paste0("total_", country)]] <- rep_total
}

#V And this will give us shares of each occupation among all workers in a period-country
for (country in countries) {
  all_data[[paste0("share_", country)]] <- all_data[[country]] / all_data[[paste0("total_", country)]]
}

# Task - no change
task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()
aggdata <-aggregate(task_data, by=list(task_data$isco08_1dig),
                    FUN=mean, na.rm=TRUE)
aggdata$isco08 <- NULL

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# VI first/second/third task item -> Trying to automate functions

standardize_for_country <- function(data, task_item, country) {
  share_column <- paste0("share_", country)
  standardized_column <- paste0("std_", country, "_", task_item)
  
  temp_mean <- wtd.mean(data[[task_item]], data[[share_column]])
  temp_sd <- sqrt(wtd.var(data[[task_item]], data[[share_column]]))
  
  data[[standardized_column]] <- (data[[task_item]] - temp_mean) / temp_sd
  
  return(data)
}

for (item in task_items) {
  for (country in countries) {
    combined <- standardize_for_country(combined, item, country)
  }
}

# VII. The next step is to calculate the `classic` task content intensity, i.e.
for (country in countries) {
  nrca_column <- paste0(country, "_NRCA")
  combined[[nrca_column]] <- 0
  
  for (item in task_items) {
    standardized_column <- paste0("std_", country, "_", item)
    combined[[nrca_column]] <- combined[[nrca_column]] + combined[[standardized_column]]
  }
}

# VIII. And we standardise NRCA in a similar way.
for (country in countries) {
  nrca_column <- paste0(country, "_NRCA")
  std_nrca_column <- paste0("std_", country, "_NRCA")
  share_column <- paste0("share_", country)
  
  temp_mean <- wtd.mean(combined[[nrca_column]], combined[[share_column]])
  temp_sd <- wtd.var(combined[[nrca_column]], combined[[share_column]]) %>% sqrt()
  
  combined[[std_nrca_column]] <- (combined[[nrca_column]] - temp_mean) / temp_sd
}

# IX. Finally, to track the changes over time, we have to calculate a country-level mean. 
agg_list <- list()

for (country in countries) {
  std_nrca_column <- paste0("std_", country, "_NRCA")
  share_column <- paste0("share_", country)
  multip_nrca_column <- paste0("multip_", country, "_NRCA")
  
  combined[[multip_nrca_column]] <- (combined[[std_nrca_column]] * combined[[share_column]])
  
  agg_list[[country]] <- aggregate(combined[[multip_nrca_column]], by=list(combined$TIME),
                                   FUN=sum, na.rm=TRUE)
}

# X. Charts
num_countries <- length(countries)
rows <- ceiling(sqrt(num_countries))     
cols <- ceiling(num_countries/rows)      
par(mfrow=c(rows, cols))


for (country in countries) {
  plot(agg_list[[country]]$x, xaxt="n", main=paste("Aggregated Data for", country))
  axis(1, at=seq(1, 40, 3), labels=agg_list[[country]]$Group.1[seq(1, 40, 3)])
}

### Sum up. It is now possible to add new countries (all kinds of modifications) and variables to the test, 
### so it seems to me that the code has been significantly cleaned up and normalised, 
### making it possible to work on data in different areas.


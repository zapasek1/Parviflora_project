'
---
  title: "Parviflora Project"
subtitle: "Group project for Introduction to R classes"
authors: "Jakub Zapasnik (38401), Maciej Golik (46827), Paweł Jędrzejczak (46471), Daniel Lilla (38963), Michał Kloska (46341)"
---
'
  
Sys.setlocale("LC_CTYPE", "Polish")
Sys.setenv("LANGUAGE"="PL")
library(tidyverse)
library(readxl)
library(ggplot2)

#### Read in STORES ####
source(list.files(pattern = "*process_Stores*.R$", recursive = TRUE))
# Find Stores data
stores_file <- list.files(pattern="^Stores.*xlsx$", recursive = TRUE)
df_stores <- read_stores(stores_file)

#### SUMMARY OF SALES ####

source(list.files(pattern = "*process_sales_summaries*", recursive = TRUE))

# create a list of all Summary of Sales ... .csv files
summary_files_list <- list.files(pattern="^Summary.*csv$", recursive = TRUE)
# create data.frame of total sales of all flowers other than Daffodils (for all months combined)
df_sales <- union_sales_data(summary_files_list)
# Fix the stores' names based on Stores.xlsx file - the new name is in column store_loc_fixed
df_sales <- fix_store_names(df_sales, correct_names = df_stores$store_location)

df_sales_totals <- get_sales(df_sales, "total")
#df_sales_gross <- get_sales(df_sales, "gross")

### DAFFODILS ####
# notice that only functions appear when sourcing this script - no variables are defined there
source(list.files(pattern = "*process_Daffodils*.r$", recursive = TRUE))

# use recursive = TRUE to get matching files in subdirectories
daffodils_paths <- list.files(pattern = "^Daffodils.*xls$", recursive = TRUE)

# For now we only have one year - thus one file [1] but what in the future?
# NOT NECESSARY in this project - Create a loop to iterate through these files - [1] indicates this possibility as paths would be a vector if there are many files
# summaries from the top of Daffodils file
df_summary_daffodils <- merge_summaries(daffodils_paths[1]) 

# data.frame with revenue and count of Daffodils at per store, month and year basis
df_daffodils <- combine_tables(daffodils_paths[1], totals_only = TRUE)


#### JOINING ####
# change stores data.frame - it is now a Carthesian product of all stores with all possible months and years
df_stores <- get_carthprod_of_periods(df_stores, df_sales_totals, df_daffodils)

# Left join to Stores (by Fixed name) - all stores will be present at this stage! 
# Even those with all NULLs are present - meaning no sales of other flowers in these stores in this month
# (i.e. later join Daffodil data so we NEED them)
df_stores_sales <- df_stores %>% full_join(df_sales_totals, 
                                        by = c("store_location" = "store_loc_fixed",
                                               "month" = "month_id", "year" = "year_id"))

### JOIN Daffodils to Sales Summaries ###
# on store_id, month and year
df_complete <- df_stores_sales %>% full_join(select(df_daffodils, -id), 
                                   by = c("store_id" = "store_id", 
                                          "month" = "month", "year" = "year")) %>%
                                   arrange('store_id', "month")

# Add totals from Daffodils file to their place and drop these columns
df_complete <- df_complete %>% mutate(rev_Daffodil = trans_amount,
                                      count_Daffodil = trans_count) %>%
                               select(-trans_amount, -trans_count) %>%
  # flag stores with missing name as "unknown" 
                               mutate(store_name = ifelse(is.na(store_name), "unknown", store_name)) %>%
  # However now the rev_total is not a really a total... should be updated
                               mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>% # first replace NAs (NA + 1 = NA)
                               mutate(rev_total = rev_total + rev_Daffodil, 
                                      count_total = count_total + count_Daffodil)

                
# Drop NAs from store_name and report them separately - flagged as "unknown"
df_analysis <- df_complete %>% filter(store_name != "unknown")

# This data.frame is better when dealing with flower types analysis
df_flower_analysis <- df_analysis %>%
  tidyr::pivot_longer(count_Azalea:rev_Daffodil, names_to = 'flower', values_to = 'value') %>%  #pivoting data to make plotting possible
  separate(col = flower, into = c("value_lab", "flower"), sep = "\\_") %>% 
  pivot_wider(names_from = value_lab, values_from = value) %>% 
  mutate(flower = as.factor(flower)) %>% # flower column as factor
  select(-count_total, -rev_total) # drop unnecessary columns - we can obtain them by summarise() anyway


# Save checkpoint file to /processed folder after data from both systems was integrated
# this file may as well be used in .Rmd file
tryCatch( # Just an experiment with tryCatch in R ... ugly syntax
    expr = {
      saveRDS(df_analysis, file = "data/processed/integrated_data.rds")
      saveRDS(df_flower_analysis, file = "data/processed/tidy_data.rds") 
    }, # in case the above doesn't work - use recommended "here" library
    error = function(e){
      saveRDS(df_analysis, here::here("data", "processed", "integrated_data.rds"))
      message("File saved using here package, couldn't resolve path with '/'")
    }
  )

# save .csv file as well in case anyone not familiar with R would like to make own analysis on raw data e.g. in Excel
write.csv2(df_complete, 'output/integrated_data.csv')


### ANALYSIS ###
# output plots to output/ directory
source(list.files(pattern = "4_analysis*.R$", recursive = TRUE))

p <- get_period_header(df_analysis)

# Plots referring to stores revenue
plt1 <- horizontal_bar_stores(df_analysis, period = p)
plt2 <- diverging_bar_stores(df_analysis)
plt3 <- horizontal_bar_stores_counts(df_analysis, p)

# plots referring to flowers
plt4 <- flower_composition(df_flower_analysis, p)
# First totals
plt5 <- bar_tot_flower(df_flower_analysis, p, what = "rev")
plt6 <- bar_tot_flower(df_flower_analysis, p, what = "count")
plt7 <- scatter_count_rev(df_flower_analysis, p)
# Monthly revenue
plt8 <- Kuba_plot(df_analysis, p)
plt9 <- bar_flower_month(df_flower_analysis, p)
plt10 <- sep_flow_count(df_flower_analysis, p)
plt11 <- hist_mean_order(df_flower_analysis)
plt12 <- bar_order_flower(df_flower_analysis)


# Render the .Rmd file to create .html output document
rmarkdown::render("Parviflora_report.Rmd", output_dir = 'output/')


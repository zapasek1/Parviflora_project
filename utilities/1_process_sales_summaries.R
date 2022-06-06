### SUMMARY OF SALES ####

# define constant inputs
# expected columns order and names to be assigned
sales_cols <- c("store_name", "store_number", "count_Azalea", "rev_Azalea", 
             "count_Begonia", "rev_Begonia", "count_Carnation", "rev_Carnation",
             "count_Daffodil", "rev_Daffodil", "count_total", "rev_total")


add_month_year <- function(file_name, df){
  '
  Function which extracts month and year from file name (the only place where we have this info)
  Extracts Store ID information from store_number column (last 3 digits of long numbers)
  It adds this information as columns to identify the data and be able to join it with other data
  The function also splits data into used part (totals) and part which is not of primary focus for us (rest)
  THINK - The last step could be moved to separate function
  inputs:
    -> file_name (string) - relative path to processed file
    -> df (data.frame) - data.frame obtained by reading in the file
  output:
    <- ls_df (list) - the list of 2 data.frames: totals (used data) and rest (GROSS / RETAIL etc.)
  '
  # In this step we extract the month and the year - they should appear after Sales phrase
  month_year <- stringr::str_match(file_name, "Sales\\s*(.*?)\\s*.csv")[,2]
  # splitted by space 
  month_year <- strsplit(month_year, split=' ')[[1]]
  
  # month is the first element and year is the second one
  mnth <- match(month_year[1], month.name)
  yr <- as.numeric(month_year[2]) 
  
  # adding them as columns of integer data type
  df <- df %>% mutate(month_id = mnth, year_id = yr)
  
  return(df)
  ### ----------------- WRONG PART - WE NEED ALL DATA HERE ---------------- ###
  
  # # Data.frame z GROSS / RETAIL / OTHER 
  # # maybe we will use it later 
  # df_scam <- df %>% filter(!str_length(as.character(store_number)) == 10)
  # 
  # # Filter only the data we need (totals)
  # df <- df %>% filter(str_length(as.character(store_number)) == 10) %>% 
  #   mutate(store_id = as.numeric(substr(as.character(store_number),8, 10)))
  # 
  # # R doesn't support returning multiple objects - returns list
  # ls_df = list("totals" = df, "rest" = df_scam)
  # return(ls_df)
}


union_sales_data <- function(file_paths){
  '
  Function which loops over the list of "Summary of Sales ...csv" files and applies transformations by add_month_year() 
  It creates a list of data.frames (one for each month - as files)and binds them together later
  inputs:
    -> file_paths (string) - relative path to processed file
  output:
    <- df_totals (data.frame) - all combined "Summary ... .csv" files with information on month and year added
  '
  # initialize lists of data.frames and a pointer
  sales_dflist <- list()
  i <- 1 
  
  for (file in file_paths)
    {
    # read data.frame with monthly data
    df_mth <- read_delim(file, delim = ",", col_names = cauntries_cols, skip = 1)
    print(paste("wczytano plik", file))
    
    # Add information about month year and extracted store ID (this is necessary for later join)
    ls_trans_df <- add_month_year(file, df_mth)
    
    sales_dflist[[i]] <- ls_trans_df
    # # increase the pointer
    i <- i + 1
    }
  
  # make a UNION - bind list of data.frames together
  df_sales <- dplyr::bind_rows(sales_dflist)
  
  # columns with count and revenue are read as character - change them to numeric using the fact that names are meaningful 
  # colnames as list -> selection str_start for "count" and "rev", so only columns for stores counts and revenues are selected 
  clmns <- colnames(df_sales)
  clmns_to_cast <- clmns[str_starts(clmns, "count") | str_starts(clmns, "rev")]
  # previously selected counts and revenues converted to numeric - across() syntax used (mutate_at is superseded)
  df_sales <- df_sales %>% mutate(across(clmns_to_cast, as.numeric), 
                                  # the label on what type of sales it is added
                                  sales_label = case_when(str_detect(store_name, 'GROSS') ~ "gross",
                                                          str_detect(store_name, 'RETAIL') ~ "retail",
                                                          str_detect(store_name, 'OTHER') ~ "other",
                                                          TRUE ~ 'no label')) # or just NA_character_
  
  return(df_sales)
}


compare_stores <- function(csv_name, stores_name, polish_chars){
  csv_len <- str_length(csv_name)
  same_stores <- TRUE
  '
  Function which loops over the characters in store name from csv file and tries to match it with correct name from Stores.xlsx file
  It assumes that "?" which are in the place of polish-specific character are correct which can lead to some missclassifications!
  inputs:
    -> csv_name (string) - store name from csv file
    -> stores_name (string) - store name from Stores.xlsx file this name is assumed to be correct
    -> csv_name (character vector) - vector of polish characters which are not present in store name
  output:
    <- same_stores (boolean) - returns TRUE if stores are the same and FALSE if they are different
  '
  # first condition - strings must be equal length
  if(csv_len == str_length(stores_name)){
    
    v_csv <- str_split(csv_name,boundary("character"))[[1]]
    # Stores are in lower case - so to compare we have to convert them to upper
    v_stores <- str_to_upper(str_split(stores_name,boundary("character"))[[1]])
    
    for (l in 1:csv_len){
      # if letters are the same OR the true letter is one of polish characters
      # TODO This will not work if the strings are the same except for polish characters e.g. "wĄchock" = "wĘchock" ...
      if (v_csv[l] == v_stores[l] | v_stores[l] %in% polish_chars) {
        
        same_stores = TRUE
        
      } else { 
        same_stores = FALSE 
        break
        }
    } 
  } else {same_stores = FALSE}
  
  return(same_stores)
}



fix_store_names <- function(df_sales, correct_names) {
  '
  This function iterates over names from csv files and matches them with correct names from Stores file.
  inputs:
    -> df_sales (data.frame) - data.frame of all sales of other flowers
    -> correct_names (character vector) - vector of all correct names from Stores file (without "Parviflora" prefix)
  output:
    <- df_sales (data.frame) - all combined "Summary ... .csv" files with correct location mae added as store_loc_fixed column
  '
  
  df_sales <- df_sales %>% mutate(store_loc = str_replace_all(store_name, c('PARVIFLORA ' = '', ' GROSS' = '',' RETAIL' = '', ' OTHER' = '')))
  # tricky part - not sure if that would work in all cases
  df_sales <- df_sales %>% mutate(store_loc = str_replace_all(store_loc, '�', "Ó" ) )
  
  csv_names <- df_sales$store_loc
  polish_syf <- c('Ą', 'Ę', 'Ć', 'Ł', 'Ń', 'Ó', 'Ś', 'Ż', 'Ź')
  new_names = c()
  
  for (csv_store in csv_names) {
    found <- FALSE
    
    for(store in correct_names){
      if(compare_stores(csv_store, store, polish_syf)) {
        new_names <- append(new_names, store)
        found <- TRUE
        break
      } 
    }
    if (found == FALSE){new_names <- append(new_names, NA_character_)}
    
  }
  # add vector of new_names as a new column "store_loc_fixed"
  df_sales <- df_sales %>% mutate(store_loc_fixed = new_names)
  
  return(df_sales)
}


get_sales <- function(df_sales, type_of_sales = 'total') {
  '
  Function which returns either data.frame of total sales which is the sum of all sales present
  or the data.frame with specific types of sails recorded - it is not grouped in this case 
  as we assume there is only one "GROSS" position per store per month.
  '
  if(type_of_sales == 'total'){
    grouped_total_sales <- df_sales %>% group_by(store_loc_fixed, month_id, year_id) %>% 
                                  summarise(across(!starts_with("store_") & !c('sales_label'), sum)) %>%
                                  ungroup()
    return(grouped_total_sales)
    
  } else {
    subgroup <- df_sales %>% filter(sales_label == type_of_sales)
    return(subgroup)
  }
}

# TODO Try with FuzzyJoin
#library(fuzzyjoin)
#join1 <-  stringdist_inner_join(words, by = c(misspelling = "word"), max_dist = 1)

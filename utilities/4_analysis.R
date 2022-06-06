### Analysis 
# This script defines functions used to perform analysis of Parvidlora sales
# create plots etc.
# please use ggplot2 library

get_period_header <- function(df){
  '
  This function creates a string that can be attached to any title and informs about the period which was analyzed.
  returns string e.g: "from January to March 2020"
  '
  first_month <- month.name[select(df, month) %>% min()]
  last_month <- month.name[select(df, month) %>% max()]
  year <- df %>% select(year) %>% max() %>% as.character()
  period_header <- paste("from", first_month, "to", last_month, year)
  
  return(period_header)
}


save_plot <- function(name) {
  '
  Function that saves the LAST GENERATED plot to output/plots directory
  use this function to safely save the plot (in case the path with "/" fails)
  '
  #TODO Is there an option that would make saved plots scale automatically?
  tryCatch(
    expr = {
      ggsave(paste("output", "plots", name, sep = '/'))
    }, # in case the above doesn't work - use recommended "here" library
    error = function(e){
      saveRDS(df_complete, here::here("output", "plots", name))
      message("File saved using here package, couldn't resolve path with '/'")
    }
  )
  
}

horizontal_bar_stores <- function(df, period, save = TRUE) {
  '
  This function creates the horizontal bar chart of Total Revenue by store sorted best to worse.
  Same plot for count is rather not necessary - counts are interesting on per flower basis
  '
  # create plot regarding total revenue
  # TODO make it more pretty (delete "Parviflora" maybe?) and add some space on the right side 
  plt <- ggplot(df, aes(x = reorder(store_name, rev_total) , y = rev_total)) + 
    geom_bar(stat="identity", fill = '#26329C') + coord_flip() + 
    xlab("") + ylab("Total Revenue") + ggtitle(paste("Total Revenue of Parviflora stores", period)) + scale_y_reverse() + 
    theme(axis.text.y=element_blank(),  #remove y axis labels 
          axis.ticks.y=element_blank()) # remove ticks
  
  # If the argument is TRUE then save the plot, intended to save it inside main script, but don't do it when running .Rmd file
  if (save) {
    save_plot("tot_rev_stores.png")
  }
  
  return(plt)
}

bar_tot_flower <- function(df, period, what, save = TRUE){
  if (what == 'count') {
    
    plt_count <- df %>% ggplot(aes(x = count, y = reorder(flower, count), fill = flower)) +
      geom_bar(stat='identity') + xlab("Count") + ylab("Flower Types") + ggtitle(paste("Total Count by Flower", period))
    
    if (save) {
      save_plot("bar_tot_flower_count.png")
    }
    
    return(plt_count)
  
  } else {
  
  plt_rev <- df %>% ggplot(aes(x = rev, y = reorder(flower, rev), fill = flower)) +
    geom_bar(stat='identity') + xlab("Revenue") + ylab("Flower Types") + ggtitle(paste("Total Revenue by Flower", period)) + 
    scale_x_continuous(labels = scales::comma)
  
    if (save) {
      save_plot("bar_tot_flower_rev.png")
    }
    
  return(plt_rev)  
  }
}
 
 
sep_flow_count <- function(df_analysis, period, save = TRUE){
  'Scatterplot rof ount by flower'

  tot_count_mo_flow <- df_flower_analysis %>% dplyr::mutate(month_new = factor(month, levels = c(min(month):max(month))))
  
  plt <- tot_count_mo_flow %>% ggplot(aes(x = reorder(flower, -count), y = count, fill = flower)) +
    geom_bar(stat='identity') + facet_grid(.~reorder(month.name[month_new], month)) +
    xlab("Flower Type") + ylab("Total Count") + ggtitle(paste("Total count of flowers by month", period)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5))
  
  if (save) {
    save_plot("flowers_by_month_count.png")
  }
  
  return(plt)
  }


horizontal_bar_stores_counts <- function(df_analysis, period, save = TRUE){
  'Amount of flowers sold in total over the three month period. Weirdly, high discrepancy in revenue and counts for Katowice or Rzesz?w'
  
  plt <- ggplot(df_analysis, aes(x = reorder(store_name, rev_total) , y = count_total)) + 
    geom_bar(stat="identity", fill = '#209880') + coord_flip() + 
    xlab("") + ylab("Total Count") + ggtitle(paste("Total Flower Counts of Parviflora stores", period))
  
  if (save) {
    save_plot("tot_count_stores.png")
  }
  
  return(plt)
}


diverging_bar_stores <- function(df, save = TRUE) {
  '
  Function creates Diverging bar chart as shown here: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
  Input:
    -> Parviflora dataframe in its final form
  Output:
    <- png. file with the chart
  '
  
  #preparing data
  df_plot_prep <- df %>%
    dplyr::group_by(store_name) %>% 
    dplyr::summarise(revenue=sum(rev_total)) %>% #grouping by store_name and summarizing with sum of total revenues for the stores
    mutate(revenue_norm = round((revenue - mean(revenue))/sd(revenue), 2)) %>% #calculating normalized revenue equation and rounding to two decimal places
    mutate(flag = case_when(revenue_norm < 0 ~ "below", revenue_norm > 0 ~ "above")) %>% #creating flag column with "above" for stores with higher revenue than average and "below" for lower
    dplyr::arrange(revenue_norm) %>% #sorting by revenue_norm
    dplyr::mutate(store_name = factor(store_name, level=store_name)) #converting store_name to factor, because the plot doesn't sort bars properly without it
  
  
  # Diverging Barcharts
  plt1 <- ggplot(df_plot_prep, aes(x=store_name, y=revenue_norm, label=revenue_norm)) + 
    geom_bar(stat='identity', aes(fill=flag), width=.5)  +
    scale_fill_manual(name="Revenue", 
                      labels = c("Above Average", "Below Average"), 
                      values = c("above"="#53AD70", "below"="#FA9391")) + 
    labs(subtitle="Normalized Revenue per Store (0 = average revenue)", 
         title= "Revenues") + 
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))  +
    coord_flip()
  
  if (save) {
    save_plot("Normalized_Revenue_per_Store.png")
  }
  
  return(plt1)
}


bar_order_flower <- function(df, save = TRUE) {
  '
  Function first creates data with mean order value for each flower (revenues / count), then plots it
  NIE MAM ZBYTNIO POMYSŁU NA TEN WYKRES, BO NA TĄ CHWILĘ WYGLĄDA ŚREDNIO XD
  Input:
    -> df_flower_analysis object
  Output:
    <- png. file with the chart
  '
  
  df_prep <- df %>%
    dplyr::mutate(order_value = rev / count) %>%
    dplyr::group_by(flower) %>%
    dplyr::summarize(mean_order_value = mean(order_value, na.rm = TRUE))
  
  p1 <- ggplot(df_prep, aes(x = flower, y = mean_order_value, fill = flower)) + 
    geom_bar(stat = "identity") +
    ylab('Average Order Value') + ggtitle("Mean order value of flowers") +
    theme(legend.position="none")
  
   if (save) {
     save_plot("Mean_Order_per_Flower.png")
   }
  
  return(p1)
}



#total revenue of stores by month
Kuba_plot <- function(df_analysis, period, save = TRUE){
  pltKuba <- df_analysis %>%
    select(store_name, month, year, rev_total) %>% 
    mutate(rev_total / 1000) 
  pltKuba1 <- pltKuba[-c(1,4)]
  pltKuba2 <- aggregate(x = pltKuba1$"rev_total/1000", by = list(pltKuba1$month), FUN = sum)
  colnames(pltKuba2) <- c("month", "revenue")
  
  wykresiq <- ggplot(pltKuba2, aes(x = month, y = revenue)) +
    geom_bar(stat="identity", fill = "#53AD70") + ggtitle(paste("Total Revenue of Parviflora stores in 1000zł"))
  
  if (save) {
    save_plot("Rev_by_month.png")
  }
  return(wykresiq)
}


bar_flower_month <- function(df, period, daf = TRUE,  save=TRUE) {

  '
  Function creates a facet grid of bar charts with flower revenue for separate month in each plot.
  Inputs:
    -> df - df_flower_analysis object
    -> daf - wheter or not to show Daffodil data, cause in revenue it is a very big outlier and so we may want to show chart without daffodil data
  Output:
    <- ggplot chart
  '
  
  df_prep <- df %>% 
    dplyr::filter(!flower == ifelse(daf == FALSE, 'Daffodil', ''))
  
  plt <- ggplot(df_prep, aes(x = reorder(flower, -rev), y = rev, fill = flower)) + geom_bar(stat = "identity") +
    facet_grid(.~reorder(month.name[month], month)) + #creating a facet grid 
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) + ggtitle(paste("Total revenue of flowers by month", period)) +
    xlab('') + ylab('Revenue') +
    theme(legend.position="none") +
    scale_y_continuous(labels = scales::comma)
  
  if (save) {
    save_plot("flowers_by_month.png")
  }
  return(plt)
}


hist_mean_order <- function(df, save = TRUE){
  '
  Próbna funkcja możemy ustalić czy się przyda
  '
  
  df_prep <- df %>%
    dplyr::mutate(order_value = rev / count)
    
  plt <- ggplot(df_prep, aes(x = order_value, fill = flower)) + geom_histogram(bins = 8) + facet_grid(~flower, scales = "free_x") 
  
  if (save) {
    save_plot("histogram_mean_order.png")
  }
  return(plt)
}


scatter_count_rev <- function(df, period, save=TRUE){
  '
  Function creates scatter plot of relation between sales count and revenue generated
  The dots are distinguished by flower type through the use of colors.
  '
  plt <- df %>% ggplot(aes(x=count, y = rev, colour = flower)) + geom_point() +
         scale_colour_discrete(drop=TRUE, limits = levels(df$flower)) + 
         xlab("Flower sales count") + ylab("Revenue") + 
         labs(subtitle=paste("Parviflora stores", period), 
            title= "Association between sales count and revenue per flower type") + 
         theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
         scale_y_continuous(labels = scales::comma)
  
  if (save) {
    save_plot("scatter_count_rev.png")
  }
  return(plt)
}



flower_composition <- function(df, period, save=TRUE){
  '
  Function creates stacked percent bar charts to show what is the composition of sales among stores with respect to flower type.
  '
  plt <- df %>% ggplot(aes(x = reorder(store_name, rev) , y = count, fill = flower)) + geom_bar(stat="identity", position="fill") + 
         ylab("Percentage of flower sales") + xlab("Store Name") + 
         labs(subtitle=paste("Parviflora stores", period), 
         title= "Composition of sales by flower type") + 
         theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
         coord_flip() 
  
  if (save) {
    save_plot("bar_composition.png")
  }
  return(plt)
}



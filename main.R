#### Libraries ####

library(rvest)
library(xml2)
library(tidyverse)
library(zoo)

#### 1. Download data from CBR.RU ####

cbr_currencies_download <- function(date_from = "10.12.2019", 
                                    date_to = "10.12.2019") {
  
  url <- "https://www.cbr.ru/currency_base/daily/?date_req="
  
  # create a range to add to URL
  daterange <- seq(from = as.Date(date_from, format = "%d.%m.%Y"), 
                   to = as.Date(date_to, format = "%d.%m.%Y"),
                   by = "day") %>% 
    format("%d.%m.%Y")
  
  # initialize empty list
  list_cur <- list()
  
  # start cycle
  for (date in daterange) {
    
    # for debug - see progress
    if (which(daterange == date) %% 30 == 0) cat("Next month\n")
    
    # combine URL + DATE to get address, and download page
    page <- read_html(paste0(url, date))
    
    # get table from page
    table_tr <- xml_find_all(page, xpath = ".//tr")
    
    # initialize empty DF
    table_td <- data.frame(Date = NA, 
                           NCode = NA,
                           TCode = NA,
                           Units = NA,
                           Name = NA,
                           Rate = NA)
    
    # decompose HTML table into dataframe
    for (tr in 2:length(table_tr)) {
      row <- xml_find_all(table_tr[tr], xpath = ".//td")
      table_td[tr, ] <- c(date, row %>% map_chr(~xml_text(xml_contents(.x))))
    }
    
    # add dataframe to list
    list_cur[[date]] <- table_td
    
  }
  
  return(list_cur)
  
}

#### 2. Clean Data ####

clean_currencies <- function(currecies_list) {
  
  # remove NA created at initialisation
  currencies_df <- currecies_list %>% 
    map_df(~.x) %>% 
    na.omit()
  
  # transform rates to numbers
  currencies_df$Rate <- currencies_df$Rate %>% 
    str_replace(pattern = ",", replacement = ".") %>% # replace commas with dots
    as.numeric() # convert to number
  
  # correct dates
  currencies_df$Date <- as.Date(currencies_df$Date, format = "%d.%m.%Y")
  
  return(currencies_df)
  
}


#### 3. Visualisation ####

make_plots <- function(currencies_df) {
  
  # Simple plot
  gg1 <- currencies_df %>% 
    filter(TCode == "USD" | TCode == "EUR" | TCode == "JPY") %>% 
    ggplot(aes(x = Date, y = Rate, color = TCode)) +
    geom_line() +
    labs(x = "Date", y = "Exchange Rate",
         title = "FX Rate for USD, EUR and JPY",
         subtitle = paste("From", head(currencies_df$Date, 1), 
                          "to", tail(currencies_df$Date, 1)),
         caption = "Data from CBR")
  print(gg1)
  
  gg2 <- currencies_df %>% 
    group_by(TCode) %>% 
    mutate(med = median(Rate)) %>% 
    ungroup() %>% 
    ggplot(aes(x = reorder(TCode, -med), y = Rate, 
               fill = TCode)) +
    geom_boxplot() +
    labs(x = "Currency", y = "Rate distribution", 
         title = "Distribution of currency rates", 
         subtitle = paste("From", head(currencies_df$Date, 1), 
                          "to", tail(currencies_df$Date, 1)),
         caption = "Data from CBR")
  print(gg2)
  
  gg3 <- currencies_df %>% 
    select(Date, TCode, Rate) %>% 
    spread(TCode, Rate) %>% 
    select(-1) %>% 
    cor() %>% 
    as.data.frame() %>% 
    rownames_to_column("Cur2") %>% 
    gather(key = Cur1, value, -Cur2) %>% 
    ggplot(aes(x = Cur1, y = Cur2, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = "darkblue", high = "darkred", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Pearson\nCorrelation") +
    labs(x = "Currency 1", y = "Currency 2", 
         title = "Correlation matrix for currencies", 
         subtitle = paste("From", head(currencies_df$Date, 1), 
                          "to", tail(currencies_df$Date, 1)),
         caption = "Data from CBR")
  print(gg3)
  
  gg4 <- currencies_df %>% 
    mutate(logRate = log(Rate)) %>% 
    group_by(TCode) %>%
    mutate(logret = diff(zoo(logRate), na.pad = TRUE)) %>% 
    ungroup() %>% 
    na.omit() %>% 
    filter(TCode == "EUR" | TCode == "USD" | TCode == "JPY") %>% 
    ggplot(aes(x = Date, y = logret, color = TCode)) +
    geom_line() +
    facet_grid(TCode~.) +
    labs(x = "Date", y = "Log returns", 
         title = "Log returns of EUR, USD and JPY",
         subtitle = paste("From", head(currencies_df$Date, 1), 
                          "to", tail(currencies_df$Date, 1)),
         caption = "Data from CBR")
  print(gg4)
  
}



#### RUN ####

# Download
currencies <- cbr_currencies_download(date_from = "01.01.2019", 
                                      date_to = "10.12.2019")

# Clean
currencies_df <- clean_currencies(currencies)

# Plots
make_plots(currencies_df)
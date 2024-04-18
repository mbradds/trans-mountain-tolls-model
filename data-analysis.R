library(tidyverse)
library(here)


get_data <- function(file_name) {
  # Construct the path to the CSV file relative to the project root directory
  file_path <- here("trans-mountain-tolls-model", file_name)
  
  # Read the CSV file into a dataframe
  data <- read.csv(file_path)
  
  return(data)
}

prepare_prices <- function(df) {
  df$Dates <- as.Date(paste("01", df$Dates), format = "%d %b %Y")
  df$VCR_EDM_Diff <- df$Vancouver - df$Edmonton
  return(df)
}

prepare_tolls <- function(df) {
  df <- subset(df, select = -c(B, C))
  return(df)
}


# gasoline prices
price_data <- get_data("Edmonton and Vancouver Unleaded Wholesale Prices from.csv")
price_data = prepare_prices(price_data)
# print(str(price_data))

tolls_data <- get_data("trans-mountain-tolls.csv")
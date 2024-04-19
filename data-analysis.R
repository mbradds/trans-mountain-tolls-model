library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
source("trans-mountain-tolls-model/util.R")


prepare_prices <- function(df) {
  df$Dates <- as.Date(paste("01", df$Dates), format = "%d %b %Y")
  df$VCR_EDM_Diff <- df$Vancouver - df$Edmonton
  colnames(df)[which(colnames(df) == "Dates")] <- "Date"
  return(df)
}

prepare_tolls <- function(df) {
  df <- subset(df, select = -c(Tariff.Number,
                               Replaces.Tariff.Number,
                               REGDOCS.Folder,
                               REGDOCS.Download.Link,
                               Corporate.Entity,
                               Pipeline.Name))
  df <- df |> filter(Service == "Tank Metered", Unit == "CN$/m3")
  df <- subset(df, select = -c(Service, Unit))
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  return(df)
}

chart_tolls_by_path <- function(df, product = "light") {
  chart <- df |>
    filter(Product == product) |>
    filter(Path %in% c("Edmonton to Sumas", "Edmonton to Burnaby", "Edmonton to Westridge")) |>
  ggplot(aes(x = Date, y = Toll, color = Path)) +
    geom_line() +
    labs(x = "X Axis Label",
         y = "Y Axis Label",
         color = "Path") +
    cer_theme()
  save_charts("1_tolls", chart)
}

chart_scatter <- function(df_tolls, df_price, product="light", path="Edmonton to Burnaby") {
  chart <- df_tolls |> 
    filter(Product == product) |>
    filter(Path == path) |>
    inner_join(df_price, by = "Date") |>
    ggplot(aes(x = Toll, y = VCR_EDM_Diff)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  save_charts("2_scatter", chart)
  return(df)
}


# gasoline prices
price_data <- get_data("Edmonton and Vancouver Unleaded Wholesale Prices from.csv")
price_data = prepare_prices(price_data)
# print(str(price_data))

# tolls data
tolls_data <- get_data("trans-mountain-tolls.csv")
tolls_data <- prepare_tolls(tolls_data)
# print(str(tolls_data))

# charts
chart_tolls_by_path(tolls_data)
chart_scatter(tolls_data, price_data)


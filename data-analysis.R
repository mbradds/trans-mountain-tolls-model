library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(lmtest)
source("trans-mountain-tolls-model/util.R")


prepare_model_data <- function(df_tolls, df_price, product="light", path="Edmonton to Burnaby") {
  df <- df_tolls |> 
    filter(Product == product) |>
    filter(Path == path) |>
    inner_join(df_price, by = "Date")
  
  df <- df |>
    mutate(lag_toll_1 = lag(Toll, n = 1, default = NA))
  
  df$toll_change <- df$Toll - df$lag_toll_1
  
  df <- df |>
    mutate(lead_toll_change = lead(toll_change, n = 1, default = NA))
  
  df <- df |>
    mutate(lag_toll_change = lag(toll_change, n = 1, default = NA))
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

chart_scatter <- function(df, product="light", path="Edmonton to Burnaby") {
  chart <- ggplot(df, aes(x = Toll, y = VCR_EDM_Diff)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  save_charts("2_scatter", chart)
}

prepare_throughputs <- function(df) {
  
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

# throughput data
traffic_data <- get_data("trans-mountain-throughput-and-capacity.csv")
traffic_data <- prepare_throughputs(traffic_data)

# data for model and charts
df <- prepare_model_data(tolls_data, price_data)

# charts
chart_tolls_by_path(tolls_data)
chart_scatter(df)

# model
model <- lm(VCR_EDM_Diff ~ lead_toll_change + Toll,  data = df)
# Print the model summary
summary(model)
# Perform Durbin-Watson test for autocorrelation in residuals
dwtest(model)
plot(model, which = 1)

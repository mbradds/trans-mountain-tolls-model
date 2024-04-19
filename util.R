cer_palette <- c("#054169","#FFBE4B","#5FBEE6","#559B37","#FF821E","#871455","#8c8c96","#5c78ad","#42464B","#CC37B0","#FA86AC",
                 "#054169","#FFBE4B","#5FBEE6","#330000","#333300","#666633","#990033")
# line size for all the line charts made with geom_line
line_size = 0.55

cer_theme <- function(){
  theme_bw()+
    theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 5),
      legend.title = element_blank(),
      legend.key.size = unit(2, 'mm'),
      legend.spacing.x = unit(0.40, "mm"),
      legend.spacing.y = unit(0.10, "mm"),
      panel.border = element_blank(), axis.line.y = element_line(),
      plot.subtitle = element_blank(),
      plot.margin = margin(.25, .75, .25, .75, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", size = 0.5),
      axis.title.x = element_blank(),
      axis.text.x = element_text(margin = margin(t = .1, unit = "cm")),
      axis.ticks.length = unit(0.1,"cm"),
      axis.line.x = element_line(color = "black"),
      axis.title.y = element_text(size = 5, colour="black",margin = margin(r = 10)),
      axis.text = element_text(size = 5, colour="black",margin = margin(t = 10, b = 10)),
      strip.background = element_blank()
    )
}

get_data <- function(file_name) {
  # Construct the path to the CSV file relative to the project root directory
  file_path <- here("trans-mountain-tolls-model", "data", file_name)
  # Read the CSV file into a dataframe
  data <- read.csv(file_path)
  return(data)
}

save_charts <- function(chart_name, chart, c_height=2.5, c_width=6) {
  # formats <- list(".png", ".svg")
  formats <- list(".png")
  for (format in formats) {
    ggsave(here("trans-mountain-tolls-model", "charts", paste(chart_name, format, sep="")),
           plot=chart,
           width = c_width,
           height = c_height,
           units = "in",
           limitsize = TRUE)
  }
}

delete_files_with_extensions <- function(folder_path) {
  file_list <- list.files(folder_path, pattern = "\\.png$|\\.svg$", full.names = TRUE)
  if (length(file_list) > 0) {
    for (file in file_list) {
      unlink(file)
    }
    print("Files with .png or .svg extension deleted successfully.")
  } else {
    print("No files with .png or .svg extension found in the folder.")
  }
}

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
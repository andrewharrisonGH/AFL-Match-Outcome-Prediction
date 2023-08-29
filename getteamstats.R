library(rvest)
library(dplyr)

# TO GET STATS FROM SPECIFIC YEAR CHANGE URL AND FILE NAME
years = c(16,17,18,19,21,22)

for (year in years) {
  # Specify the URL of the webpage containing the table
  url <- paste("https://afltables.com/afl/stats/20", year, "t.html", sep='')
  
  # Read the HTML content of the webpage
  webpage <- read_html(url)
  
  # Extract the tables based on their CSS selector
  tables <- html_nodes(webpage, "table")
  
  # Initialize an empty list to store the data frames
  table_dfs <- list()
  
  # Convert each table to a data frame
  for (table in tables) {
    table_df <- html_table(table, fill = TRUE)
    table_df <- as.data.frame(table_df, stringsAsFactors = FALSE)
    colnames(table_df) <- unname(table_df[1, ])  # Use first row as column names
    table_df <- table_df[-1, ]  # Remove the first row
    table_df <- table_df[, colSums(is.na(table_df)) == 0]  # Remove columns with NAs
    table_dfs <- append(table_dfs, list(table_df))
  }
  
  # Create function to separate values into 'For' and 'Against'
  separate_columns <- function(data) {
    new_data <- data.frame()
    for (col in names(data)) {
      if (is.character(data[[col]])) {
        split_values <- strsplit(data[[col]], "-")
        left_column <- paste0(col, "F")
        right_column <- paste0(col, "A")
        if (nrow(new_data) == 0) {
          new_data <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
        }
        new_data[[left_column]] <- sapply(split_values, function(x) as.integer(x[1]))
        new_data[[right_column]] <- sapply(split_values, function(x) as.integer(x[2]))
      }
    }
    return(new_data)
  }
  
  # Generate a sequence of numbers
  sequence <- 1:36
  
  # Separate the sequence into individual pairs
  pair_list <- split(sequence, rep(1:(length(sequence)/2), each = 2))
  
  teams <- c("Adelaide", "BrisbaneLions", "Carlton", "Collingwood", "Essendon",
             "Fremantle", "Geelong", "GoldCoast", "GreaterWesternSydney",
             "Hawthorn", "Melbourne", "NorthMelbourne", "PortAdelaide",
             "Richmond", "StKilda", "Sydney", "WestCoast", "WesternBulldogs")
  
  # There are initially two tables of stats loaded for each team
  for (pair in pair_list) {
    team_stats <- data.frame()
    # Merge stats
    team_stats <- merge(table_dfs[[pair[1]]], table_dfs[[pair[2]]], 
                        by=c("#", "Opponent"))
    team_stats <- head(team_stats, -1)
    team_stats['#'] <- lapply(team_stats["#"], function(x) gsub('R', '', x))
    team_stats['#'] <- lapply(team_stats["#"], function(x) as.numeric(x))
    team_stats <- team_stats[order(team_stats['#']),]
    
    # Separate values into 'For' and 'Against'
    
    new_team_stats <- separate_columns(team_stats[3:length(team_stats)])
    new_team_stats <- as.data.frame(cbind(new_team_stats, team_stats['#']))

    # Write to csv file
    write.csv(new_team_stats, paste(teams[pair[2]/2], year,".csv", sep=''), row.names=FALSE)
  }
}

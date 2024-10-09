## Helper functions to get the data
library(tidyverse)
library(rvest)

get_abex <- function(){
  # Get latest ABEX data # nolint: indentation_linter.
  abex_site <- read_html("https://www.abex.be/nl/indexen-abex/")
  df_abex_raw <- abex_site  %>% 
    html_element('.tablesaw') %>% 
    html_table()

  # Parse data in right format
  names(df_abex_raw) <- c("Year", "01-01-", "01-07-")
  df_abex  <- df_abex_raw  %>% 
    pivot_longer(cols = c("01-01-", "01-07-"), names_to = "month", values_to = "ABEX")  %>% 
    mutate(Date = dmy(paste0(month, Year)))  %>% 
    arrange(Date)  %>% 
    select(Date, ABEX)

  return(df_abex)
}

# Visualisation
df_abex <- get_abex()
df_abex  %>% 
ggplot(aes(x=Date, y=ABEX)) + geom_line() + ggtitle('Evolution of ABEX-index') + theme_bw()

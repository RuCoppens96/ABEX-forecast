## Helper functions to get the data

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

get_cpi_last_13_months <- function(){
  # Get data via API
  res  <-  GET("https://bestat.statbel.fgov.be/bestat/api/views/876acb9d-4eae-408e-93d9-88eae4ad1eaf/result/JSON")
  df_cpi_raw  <-  fromJSON(rawToChar(res$content))[[1]]

  # Parse data in right format
  df_cpi <- df_cpi_raw  %>% 
    mutate(Month_char = str_extract(Maand, '\\D+'),
    month_prefix = case_match(
      Month_char,
      'Januari ' ~ '01-01-',
      'Februari ' ~ '01-02-',
      'Maart ' ~ '01-03-',
      'April ' ~ '01-04-',
      'Mei ' ~ '01-05-',
      'Juni ' ~ '01-06-',
      'Juli ' ~ '01-07-',
      'Augustus ' ~ '01-08-',
      'September ' ~ '01-09-',
      'Oktober ' ~ '01-10-',
      'November ' ~ '01-11-',
      'December ' ~ '01-12-'
    ),
    Date = dmy(paste0(month_prefix, Jaar)))  %>% 
    select(Date, Consumptieprijsindex, Gezondheidsindex)

    return(df_cpi)
}


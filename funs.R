# needs some tidying up, but provided you have the necessary packages from render_app_data.R, these should work

convert_dates <- function(.data, datecol) {
  res <- .data %>% 
    mutate({{datecol}} := lubridate::date({{datecol}})) %>% 
    mutate(week_starting := lubridate::floor_date({{datecol}},'week', 1)) %>% 
    mutate(isoweek := lubridate::isoweek(week_starting), 
           year := lubridate::year(week_starting),
           {{datecol}} := NULL)
  return(res)
}



convert_dates_string <- function(.df, datecol) {
  .df %>%
    mutate(!!datecol := lubridate::date(!!rlang::sym(datecol))) %>%
    mutate(week_starting := lubridate::floor_date(!!rlang::sym(datecol),'week', 1)) %>%
    mutate(isoweek := lubridate::isoweek(week_starting),
           year := lubridate::year(week_starting),
           !!rlang::sym(datecol) := NULL)
}


get_cols_to_plot <- function(df) {
  cols_of_interest <- names(df)
  names_to_drop <- c('week_starting', 'isoweek', 'year')
  cols_to_plot <- setdiff(cols_of_interest, names_to_drop)
  return(cols_to_plot)
}

count_plotter <- function(DT, x, nweeks = 13){
  p <- DT[column == x, .N, .(week_starting, value)] %>% 
    ggplot(aes(week_starting, N)) + 
    geom_col() + 
    facet_wrap(~ value) +
    theme_minimal() + 
    theme(legend.position = 'bottom') +
    ggtitle(paste0("Number of records per non-null " , x, " value in the previous ", nweeks, " weeks")) + 
    labs(x = "", y = "")
  print(p)
}

percent_plotter <- function(DT, x, option = "C", nweeks = 13){
  p <- DT[column == x & !is.na(value),.N,.(week_starting, value)] %>% 

    ggplot(aes(week_starting, N, group = value, fill = value, tooltip = value, data_id = value)) + 
    geom_bar_interactive(stat = 'identity', position = 'fill', na.rm = TRUE) +
    theme_minimal() +
    scale_fill_viridis(discrete = TRUE, option = option) + 
    scale_y_continuous(labels = scales::percent_format()) +
    theme(legend.position = 'bottom') +
    ggtitle(paste0("Percentage of non-null variable values in the previous", nweeks, " weeks")) + 
    labs(x  = '', y = '%')
  print(p)
}






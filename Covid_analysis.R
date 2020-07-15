library(git2r)
library(dplyr)
library(zoo)
library(ggplot2)
library(ggthemes)
library(flexdashboard)
library(kableExtra)
library(DT)

############
# SETTINGS #
############

options(dplyr.summarise.inform = FALSE)

git_url<- "https://github.com/CSSEGISandData/COVID-19"
git_branch <- "web-data"
git_remote <- "origin"
local_path <- "E:\\Projects\\06_Covid_analysis\\DATA"
df_path <- "data\\cases_time.csv"

#############
# FUNCTIONS #
#############

create_repository <- function(local_path, git_url, git_branch){
  clone(url = git_url,
        local_path = local_path,
        bare = FALSE,
        branch = git_branch,
        checkout = TRUE,
        credentials = NULL,
        progress = TRUE)
}

update_repository <- function(local_path, remote_name, branch_name){
  repo <- repository(local_path)
  fetch(repo = repo, name = remote_name)
  checkout(repo, branch_name)
  merge(repo, paste0(remote_name, "/", branch_name))
}

read_data <- function(absolute_path){
  df <- read.csv(absolute_path)
  names(df) <- tolower(names(df))  
  return(df)
}

preprocess_data <- function(df){

  df <- df %>%
    rename(country = country_region, infected = confirmed, infected_new = delta_confirmed, infected_rate = incident_rate) %>%
    mutate(date = as.Date(report_date_string)) %>%
    mutate(weeks_ago = floor(abs(as.numeric(date - max(date))) / 6.9999)) %>%
    filter(province_state == "") %>%
    select(country, date, weeks_ago, infected, infected_new, infected_rate, deaths)
  
  df_pop <- df %>%
    group_by(country) %>%
    filter(infected_rate > 0) %>%
    summarise(population = max(infected / infected_rate * 100000, na.rm = TRUE), .groups = 'drop') %>%
    ungroup %>%
    group_by(country) %>%
    mutate(population = max(population, 0)) %>%
    ungroup
  
  df <- left_join(df, df_pop, by = "country")

  
  for(col_name in names(df)){
    if (is.numeric(df[[col_name]])) {
      df[[col_name]] <- ifelse(is.na(df[[col_name]]), 0, df[[col_name]])
    }
  }
  
  return(df)
}


get_country_new_infections_weeks_before <- function(df, country_name, weeks_before){
  date_interval <- get_weeks_before(max(df$date), weeks_before)
  result <- df %>%
    filter(date >= date_interval[1], date <= date_interval[2], country == country_name) %>%
    summarise(new_infections = sum(delta_confirmed), .groups = 'drop')
  return(result)
}

plot_daily_infections <- function(df_plot){
  df_moving_average <- df_plot %>%
    arrange(date) %>%
    mutate('New infections moving average' = as.integer(rollapply(infected_new, 7, mean,align='right',fill=0))) %>%
    rename('New infections' = infected_new) %>%
    filter(date >= "2020-03-01")
  
  p <- ggplot(data = df_moving_average, aes(x = date)) +
    geom_bar(aes(y = `New infections`), stat="identity", fill="steelblue") + 
    geom_line(aes(y = `New infections moving average`), size = 2) +
    theme_minimal()+ scale_x_date(date_breaks = "2 weeks", date_labels = "%Y-%m-%d") + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0)
    )
  
  ggplotly(p)
}

plot_valuebox_rg <- function(value, caption, icon = "fa-user-plus", green_positive = FALSE){
  if (green_positive == TRUE){
    if (value >= 0){
      color <- "green"
    } else {
      color <- "red"
    }
  } else { # green_positive == FALSE
    if (value >= 0) {
      color <- "red"
    } else {
      color <- "green"
    }
  }
  print(valueBox(value = value, icon = icon, caption = caption, color = color))
}

markdown_header <- function(level = 3, caption = ""){
  tag <- paste0(rep("#", level), collapse = "")
  cat(paste0("  \n  \n", tag, " ", caption, "  \n  \n"))
}

markdown_row <- function(height = NA){
  if (anyNA(height) == TRUE){
    tag <- "Row\n"
  } else {
    tag <- paste0("Row {data-height=", height, "}\n")
  }
  cat(paste0("\n\n", tag, "-----------------------------------------------------------------------\n\n"))
}

markdown_text <- function(text, tags = NA){
  if (anyNA(tags) == FALSE){
    open_tags <- paste0("<", tags, ">", collapse = "")
    close_tags <- paste0("</", tags, ">", collapse = "")
    text <- paste0(open_tags, text, close_tags)
  }
  cat(text)
}

markdown_page <- function(page_name, page_menu){
  if (is.na(page_menu) == TRUE){
    cat_page_name <- page_name
  } else {
    cat_page_name <- paste0(page_name, "{data-navmenu=\"", page_menu, "\"}")
  }
  cat(paste0("\n\n", cat_page_name, "\n", "=====================================\n\n"))
}

print_region_valueboxes <- function(df_region){
  
  # Calculation
  
  infected_total <- df_region %>%
    summarise(value = sum(infected_new), .groups = 'drop') %>%
    {{round(.$value, 0)}}  
  
  new_infected_today <- df_region %>%
    filter(date == max(df_region$date)) %>%
    summarise(value = sum(infected_new), .groups = 'drop') %>%
    {{round(.$value, 0)}}
  
  new_infected_average_this_week <- df_region %>%
    filter(weeks_ago == 0) %>%
    summarise(value = mean(infected_new), .groups = 'drop') %>%
    {{round(.$value, 0)}}  
  
  new_infected_average_last_week <- df_region %>%
    filter(weeks_ago == 1) %>%
    summarise(value = mean(infected_new), .groups = 'drop') %>%
    {{round(.$value, 0)}}  
  
  new_infected_trend <- new_infected_average_this_week - new_infected_average_last_week
  
  # Generation of RMarkdown
  
  markdown_header(level = 3, caption = "Infected total")
  
  print(valueBox(value = infected_total, icon = "fa-user-plus", caption = "Infections total", color = "grey"))
  
  markdown_header(level = 3, caption = "New infected today")
  
  print(valueBox(value = new_infected_today, icon = "fa-user-plus", caption = "New infections last day", color = "grey"))
  
  markdown_header(level = 3, caption = "New infected 7 days average")
  
  print(valueBox(value = new_infected_average_this_week, icon = "fa-user-plus", caption = "New infections weekly average", color = "grey"))
  
  markdown_header(level = 3, caption = "New infected trend")
  
  plot_valuebox_rg(value = new_infected_trend, caption = "New infections trend", green_positive = FALSE)
}

print_region <- function(region_name, section){
  df_region <- df %>%
    filter(country == region_name) %>%
    group_by(weeks_ago, date) %>%
    summarise(infected_new = sum(infected_new),
              population = sum(population),
              .groups = 'drop') %>%
    ungroup()
  
  markdown_page(page_name = region_name, page_menu = section)
  
  markdown_text(text = region_name, tags = c("center", "h1", "b"))
  
  markdown_row(height = NA) 
  
  print_region_valueboxes(df_region)
  
  markdown_row(height = 50) 
  
  cat("<div style=\"width:100%;\"><center><h1>New infections</h1></center></div>")
  
  markdown_row()
  
  markdown_header(level = 3, caption = "")
  
  plot_daily_infections(df_region)
}


#############
# MAIN CODE #
#############

#create_repository(local_path = local_path, git_url = git_url, git_branch = git_branch)
update_repository(local_path = local_path, remote_name = git_remote, branch_name = git_branch)


absolute_path <- paste0(local_path, "\\", df_path)
df <- read_data(absolute_path = absolute_path)
df <- preprocess_data(df)


dict_countries <- read.csv("dict_countries.csv", sep = ";")
names(dict_countries) <- tolower(names(dict_countries))

df <- inner_join(df, dict_countries, by = c("country"))

df <- df %>%
  mutate(country = as.character(country),
         continent = as.character(continent))

df_world <- df %>%
  mutate(country = 'World') %>%
  group_by(country, date, weeks_ago) %>%
  summarise(infected = sum(infected),
            infected_new = sum(infected_new),
            infected_rate = sum(infected) / max(population) * 100000,
            deaths = sum(deaths),
            population = sum(population),
            continent = 'Region',
            is_eu = 0) %>%
  ungroup %>%
  as.data.frame()

df_continent <- df %>%
  filter(continent != "None") %>%
  mutate(country = continent) %>%
  group_by(country, date, weeks_ago) %>%
  summarise(infected = sum(infected),
            infected_new = sum(infected_new),
            infected_rate = sum(infected) / max(population) * 100000,
            deaths = sum(deaths),
            population = sum(population),
            continent = 'Region',
            is_eu = 0) %>%
  ungroup %>%
  as.data.frame()

df_eu <- df %>%
  filter(is_eu == 1) %>%
  mutate(country = "European Union") %>%
  group_by(country, date, weeks_ago) %>%
  summarise(infected = sum(infected),
            infected_new = sum(infected_new),
            infected_rate = sum(infected) / max(population) * 100000,
            deaths = sum(deaths),
            population = sum(population),
            continent = 'Region',
            is_eu = 0) %>%
  ungroup %>%
  as.data.frame()

df_orginal <- df
df <- rbind(df, df_world, df_continent, df_eu)

print_region_string <- df %>% 
  filter(population >= 1000000) %>% 
  group_by(country, continent) %>% 
  summarise(infected = max(infected)) %>% 
  filter(infected >= 1000) %>% 
  select(country, continent) %>%
  mutate(sort_column =ifelse(continent == "Region", 0, 1)) %>%
  mutate(country = as.character(country),
         continent = as.character(continent)) %>%
  arrange(sort_column, continent, country) %>%
  mutate(print_region = paste0("print_region(\"", country, "\", \"", continent, "\")", collapse = "\n")) %>%
  {{.$print_region}} %>%
  as.character

#writeLines(print_region_string)
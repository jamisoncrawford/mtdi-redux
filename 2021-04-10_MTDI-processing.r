
# SETUP ----

# R Version: 4.0.1
# RStudio Version: 1.3.959
# Date: 2021-04-10

setwd("~/CNYCF/Measuring the Dream Index - Overhaul")

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)



# READ IN COUNTY HEALTH DATA ----

p <- "Pared Data"                                                               #' The /Pared Data directory should 
                                                                                #' contain only pared-down files
d <- as.character(dput(dir(p)))

files <- paste(p, d, sep = "/")

chr_files <- files[grepl(pattern = "County Health Rankings", x = files)]

chr_names <- paste("chr", str_extract(string = chr_files, 
                                      pattern = "[0-9]{4}"), sep = "_")

for (i in seq_along(chr_files)){
  
  assign(paste(chr_names[i], "ranked", sep = "_"), 
         read_excel(path = chr_files[i], sheet = "Ranked Measure Data"))
  
  assign(paste(chr_names[i], "added", sep = "_"), 
         read_excel(path = chr_files[i], sheet = "Additional Measure Data"))
  
}

rm(chr_files, chr_names, d, p)

names(chr_2017_added) <- make.unique(as.character(chr_2017_added[1, ]))
names(chr_2018_added) <- make.unique(as.character(chr_2018_added[1, ]))
names(chr_2019_added) <- make.unique(as.character(chr_2019_added[1, ]))
names(chr_2020_added) <- make.unique(as.character(chr_2020_added[1, ]))
names(chr_2021_added) <- make.unique(as.character(chr_2021_added[1, ]))

names(chr_2017_ranked) <- make.unique(as.character(chr_2017_ranked[1, ]))
names(chr_2018_ranked) <- make.unique(as.character(chr_2018_ranked[1, ]))
names(chr_2019_ranked) <- make.unique(as.character(chr_2019_ranked[1, ]))
names(chr_2020_ranked) <- make.unique(as.character(chr_2020_ranked[1, ]))
names(chr_2021_ranked) <- make.unique(as.character(chr_2021_ranked[1, ]))

chr_2017_added <- chr_2017_added %>% filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `Household Income (white alone)`, 
         `Household Income (black alone)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, -County) %>% 
  mutate(year = 2017)

chr_2018_added <- chr_2018_added %>% filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `Age-Adjusted Mortality (Black)`, 
         `Age-Adjusted Mortality (White)`, 
         `Child Mortality Rate (Black)`, 
         `Child Mortality Rate (White)`, 
         `Infant Mortality Rate (Black)`, 
         `Infant Mortality Rate (White)`, 
         `Household income (Black)`, 
         `Household income (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, -County) %>% 
  mutate(year = 2018)

chr_2019_added <- chr_2019_added %>% filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `Life Expectancy (Black)`, 
         `Life Expectancy (White)`, 
         `Age-Adjusted Mortality (Black)`, 
         `Age-Adjusted Mortality (White)`, 
         `Child Mortality Rate (Black)`, 
         `Child Mortality Rate (White)`, 
         `Infant Mortality Rate (Black)`, 
         `Infant Mortality Rate (White)`, 
         `Household income (Black)`, 
         `Household income (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, -County) %>% 
  mutate(year = 2019)

chr_2020_added <- chr_2020_added %>% filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `Life Expectancy (Black)`, 
         `Life Expectancy (White)`, 
         `Age-Adjusted Mortality (Black)`, 
         `Age-Adjusted Mortality (White)`, 
         `Child Mortality Rate (Black)`, 
         `Child Mortality Rate (White)`, 
         `Infant Mortality Rate (Black)`, 
         `Infant Mortality Rate (White)`, 
         `Drug Overdose Mortality Rate (Black)`, 
         `Drug Overdose Mortality Rate (White)`, 
         `Household Income (Black)`, 
         `Household Income (White)`, 
         `Homicide Rate (Black)`, 
         `Homicide Rate (White)`, 
         `Firearm Fatalities Rate (Black)`, 
         `Firearm Fatalities Rate (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, -County) %>% 
  mutate(year = 2020)

chr_2021_added <- chr_2021_added %>% filter(County %in% "Onondaga") %>%
  select(State, County, 
         `Life Expectancy (Black)`, 
         `Life Expectancy (White)`, 
         `Age-Adjusted Mortality (Black)`, 
         `Age-Adjusted Mortality (White)`, 
         `Child Mortality Rate (Black)`,
         `Child Mortality Rate (White)`, 
         `Infant Mortality Rate (Black)`, 
         `Infant Mortality Rate (White)`, 
         `Drug Overdose Mortality Rate (Black)`,
         `Drug Overdose Mortality Rate (White)`, 
         `Household Income (Black)`,
         `Household Income (White)`, 
         `Homicide Rate (Black)`,
         `Homicide Rate (White)`, 
         `Firearm Fatalities Rate (Black)`, 
         `Firearm Fatalities Rate (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, -County) %>% 
  mutate(year = 2021)

added <- bind_rows(chr_2017_added,
                   chr_2018_added,
                   chr_2019_added,
                   chr_2020_added,
                   chr_2021_added) %>% 
  rename(Value = value,
         Year = year)

chr_2017_ranked <- chr_2017_ranked %>% 
  filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `% Children in Poverty - Black`, 
         `% Children in Poverty - White`, 
         `% Drive Alone - Black`, 
         `% Drive Alone - Black`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, 
         -County) %>% 
  mutate(Year = 2017)

chr_2018_ranked <- chr_2018_ranked %>% 
  filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `Years of Potential Life Lost Rate (Black)`, 
         `Years of Potential Life Lost Rate (White)`, 
         `Teen Birth Rate (Black)`, 
         `Teen Birth Rate (White)`, 
         `% Children in Poverty (Black)`, 
         `% Children in Poverty (White)`, 
         `% Drive Alone (Black)`, 
         `% Drive Alone (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, 
         -County) %>% 
  mutate(Year = 2018)

chr_2019_ranked <- chr_2019_ranked %>% 
  filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `YPLL Rate (Black)`, 
         `YPLL Rate (White)`, 
         `Teen Birth Rate (Black)`, 
         `Teen Birth Rate (White)`, 
         `% Children in Poverty (Black)`, 
         `% Children in Poverty (White)`, 
         `% Drive Alone (Black)`, 
         `% Drive Alone (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, 
         -County) %>% 
  mutate(Year = 2019)

chr_2020_ranked <- chr_2020_ranked %>% 
  filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `YPLL Rate (Black)`, 
         `YPLL Rate (White)`, 
         `% LBW (Black)`, 
         `% LBW (White)`, 
         `Teen Birth Rate (Black)`, 
         `Teen Birth Rate (White)`, 
         `% Children in Poverty (Black)`, 
         `% Children in Poverty (White)`, 
         `% Drive Alone (Black)`, 
         `% Drive Alone (White)`) %>% 
  gather(key = Indicator, 
         value = value, 
         -State, 
         -County) %>% 
  mutate(Year = 2020)

chr_2021_ranked <- chr_2021_ranked %>% 
  filter(County %in% "Onondaga") %>% 
  select(State, County, 
         `YPLL Rate (Black)`, 
         `YPLL Rate (White)`, 
         `% LBW (Black)`, 
         `% LBW (White)`, 
         `Teen Birth Rate (Black)`, 
         `Teen Birth Rate (White)`, 
         `% Children in Poverty (Black)`, 
         `% Children in Poverty (White)`, 
         `% Drive Alone (Black)`, 
         `% Drive Alone (White)`) %>% 
  gather(key = Indicator, 
         value = value,
         -State, 
         -County) %>% 
  mutate(Year = 2021)

ranked <- bind_rows(chr_2017_ranked,
                    chr_2018_ranked,
                    chr_2019_ranked,
                    chr_2020_ranked,
                    chr_2021_ranked) %>% 
  rename(Value = value)

rm(chr_2017_ranked,
   chr_2018_ranked,
   chr_2019_ranked,
   chr_2020_ranked,
   chr_2021_ranked,
   chr_2017_added,
   chr_2018_added,
   chr_2019_added,
   chr_2020_added,
   chr_2021_added)





# READ IN ELA & MATH 3 & 8 ASSESSMENT DATA ----

ela_files <- files[grepl(pattern = "3-8 ELA|3-8_ELA", x = files)]

ela_names <- paste("ela_mat", str_extract(string = ela_files, 
                                          pattern = "[0-9]{4}"), sep = "_")

for (i in seq_along(ela_files)){
  
  if (grepl(pattern = "csv$", x = ela_files[i])){
    
    assign(ela_names[i], read_csv(ela_files[i]))
    
  }
  
  if (grepl(pattern = "xls$|xlsx$", x = ela_files[i])){
    
    assign(ela_names[i], read_excel(ela_files[i]))
    
  }
  
}; rm(ela_files, ela_names)

is.na(ela_mat_2016) <- 0

vars <- c("ITEM_DESC", "SUBGROUP_NAME", "NAME", "L3-L4_PCT")

assessment <- bind_rows(list(ela_mat_2016[ , vars] %>% mutate(year = "2016"), 
                             ela_mat_2017[ , vars] %>% mutate(year = "2017"), 
                             ela_mat_2018[ , vars] %>% mutate(year = "2018"), 
                             ela_mat_2019[ , vars] %>% mutate(year = "2019"))) %>% 
  rename(race = SUBGROUP_NAME,
         item = ITEM_DESC,
         name = NAME,
         lvl_3_to_4_pct =`L3-L4_PCT`) %>% 
  filter(race %in% c("Black or African American", "White"),
         item %in% c("Grade 4 ELA", "Grade 8 Math"),
         name == "ONONDAGA COUNTY",
         year != "2019") %>%                                                    # 2019 data corrupt?
  unique() %>% 
  rename(Indicator = item,
         Race = race,
         County = name,
         Value = lvl_3_to_4_pct,
         Year = year) %>% 
  mutate(Indicator = paste(Indicator, "L3 to L4", sep = "-"))

rm(ela_mat_2016,
   ela_mat_2017,
   ela_mat_2018,
   ela_mat_2019,
   vars)



# READ IN GRADUATION RATE FILES ----

grad_files <- files[grepl(pattern = "GRAD_RATE", x = files)]

grad_names <- paste("grad", str_extract(string = grad_files, 
                                        pattern = "[0-9]{4}"), sep = "_")

grad <- list()

for (i in seq_along(grad_files)){
  
  x <- read.csv(grad_files[i])
  
  names(x) <- str_to_lower(string = names(x))
  
  file_year <- str_extract(string = grad_files[i], 
              pattern = "[0-9]{4}")
  
  grad[[i]] <- x %>% 
    select(aggregation_type, county_name, membership_desc, subgroup_name, grad_pct, reg_adv_pct) %>% 
    filter(aggregation_type == "County",
           county_name %in% c("Onondaga", "ONONDAGA"),
           subgroup_name %in% c("Black or African American", "White"),
           grepl(pattern = " Total Cohort - 4 Year Outcome ", x = membership_desc)) %>% 
    mutate(year = file_year)
  
}; rm(grad_files, grad_names, x)

graduation <- bind_rows(grad) %>% 
  select(year, 
         subgroup_name, 
         grad_pct, 
         reg_adv_pct) %>% 
  rename(Year = year,
         Race = subgroup_name,
         `Graduation (%)` = grad_pct,
         `Advanced Regents (%)` = reg_adv_pct)

rm(file_year, grad)



# READ IN INCARCERATION TRENDS FILE ----

incarceration <- read_excel("Pared Data/incarceration_trends.xlsx") %>% 
  filter(county_name == "Onondaga County",
         state == "NY",
         year >= 2010) %>% 
  rename(County = county_name,
         State = state,
         Year = year) %>% 
  select(Year, State, County, 
         black_jail_pop_rate, 
         white_jail_pop_rate, 
         black_prison_pop_rate, 
         white_prison_pop_rate) %>% 
  gather(key = Indicator, 
         value = Value, 
         -Year, 
         -State, 
         -County)


# OUTGO ----

added <- added %>% 
  mutate(Race = NA,
         Race = ifelse(test = grepl(pattern = "black|Black", x = Indicator),
                       yes = "Black",
                       no = Race),
         Race = ifelse(test = grepl(pattern = "white|White", x = Indicator),
                       yes = "White",
                       no = Race),
         Year = as.numeric(Year),
         Value = as.numeric(Value)) %>% 
  select(Year, Race, Indicator, Value)

assessment <- assessment %>% 
  mutate(Race = gsub(x = Race, 
                     pattern = "^Black.*", 
                     replacement = "Black"),
         Value = gsub(x = Value, 
                      pattern = "%", 
                      replacement = ""),
         Value = as.numeric(Value),
         Indicator = gsub(x = Indicator,
                          pattern = "-L3 to L4", 
                          replacement = " L3-L4"),
         Year = as.numeric(Year)) %>% 
  select(Year, Race, Indicator, Value)

incarceration <- incarceration %>% 
  mutate(Race = NA,
         Race = ifelse(test = grepl(pattern = "black|Black", x = Indicator),
                       yes = "Black",
                       no = Race),
         Race = ifelse(test = grepl(pattern = "white|White", x = Indicator),
                       yes = "White",
                       no = Race),
         Year = as.numeric(Year)) %>% 
  select(Year, Race, Indicator, Value)

graduation <- graduation %>% 
  gather(key = Indicator, 
         value = Value, 
         -Year, -Race) %>% 
  mutate(Race = gsub(x = Race, 
                     pattern = "^Black.*", 
                     replacement = "Black"),
         Value = gsub(x = Value, 
                      pattern = "%", 
                      replacement = ""),
         Value = as.numeric(Value),
         Year = as.numeric(Year))

ranked <- ranked %>% 
  mutate(Race = NA,
         Race = ifelse(test = grepl(pattern = "white|White", 
                                    x = Indicator), 
                       yes = "White",
                       no = Race),
         Race = ifelse(test = grepl(pattern = "black|Black", 
                                    x = Indicator), 
                       yes = "Black",
                       no = Race),
         Year = as.numeric(Year),
         Value = as.numeric(Value)) %>% 
  select(Year, Race, Indicator, Value)



# DISPOSITIONS DATA





# MASTER MERGE

added <- added %>% 
  mutate(Source = "CHR Additional Measures") %>% 
  select(Source, Year:Value) %>% 
  arrange(Source, Indicator, Race, desc(Year))

assessment <- assessment %>% 
  mutate(Source = "NYSED 3-8 Assessment") %>% 
  select(Source, Year:Value) %>% 
  arrange(Source, Indicator, Race, desc(Year))

graduation <- graduation %>% 
  mutate(Source = "NYSED Graduation Database") %>% 
  select(Source, Year:Value) %>% 
  arrange(Source, Indicator, Race, desc(Year))

incarceration <- incarceration %>% 
  mutate(Source = "Vera Incarceration Trends") %>% 
  select(Source, Year:Value) %>% 
  arrange(Source, Indicator, Race, desc(Year))

ranked <- ranked %>% 
  mutate(Source = "CHR Ranked Measures") %>% 
  select(Source, Year:Value) %>% 
  arrange(Source, Indicator, Race, desc(Year))

master <- bind_rows(added, 
                    assessment, 
                    graduation, 
                    incarceration, 
                    ranked)

rm(added, assessment, graduation, incarceration, ranked, files, i)

master <- master %>% 
  mutate(Value = round(x = Value, 
                       digits = 3),
         Indicator = gsub(pattern = " \\(.*\\)", 
                          replacement = "", 
                          x = Indicator),
         Indicator = ifelse(test = grepl(x = Indicator, pattern = "Advanced Regents|Graduation") 
                                & !grepl(x = Indicator, pattern = "\\%"),
                            yes = paste("%", Indicator, sep = " "),
                            no = Indicator))

master[master$Indicator == c("white_prison_pop_rate"), "Indicator"] <- "Prison Population Rate"
master[master$Indicator == c("black_prison_pop_rate"), "Indicator"] <- "Prison Population Rate"
master[master$Indicator == c("black_jail_pop_rate"), "Indicator"] <- "Jail Population Rate"
master[master$Indicator == c("white_jail_pop_rate"), "Indicator"] <- "Jail Population Rate"

write_csv(master, "2021-04-11_mtdi-values_tidy.csv", na = "-")

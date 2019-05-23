# Performance and Geography
# Evan Kramer
# 5/23/2019

# Set up
options(java.parameters = "-Xmx16G")
library(tidyverse)
library(lubridate)
library(haven)
library(RJDBC)
library(rgdal)
setwd("N:/ORP_accountability")

DATA = F
FOCUS = F
PRIORITY = F
REWARD = T
SCALE = F
TVAAS = F
ANALYSIS = F

# Data
if(DATA) {
  # Connect to database
  eis_con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$EIS_MGR_CXN_STR,
    "EIS_MGR",
    readRegistry("Environment", "HCU")$EIS_MGR_PWD
  )
  sde_con = dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA19130/Downloads/ojdbc6.jar"), 
    readRegistry("Environment", "HCU")$SDE_DIR_CXN_STR,
    "SDE_DIR",
    readRegistry("Environment", "HCU")$SDE_DIR_PWD
  )
  
  # Map
  tn_counties = readOGR(
    dsn = "C:/Users/CA19130/Downloads/TN_counties",
    layer = "TN_counties",
    stringsAsFactors = F
  )
  geo = readOGR(
    dsn = "C:/Users/CA19130/Downloads/EDGE_GEOCODE_PUBLICSCH_1617/EDGE_GEOCODE_PUBLICSCH_1617",
    layer = "EDGE_GEOCODE_PUBLICSCH_1617", 
    stringsAsFactors = F
  )
  tn_geo = geo[geo@data$STATE == "TN", ]
} else {
  rm(DATA)
}

# Focus schools
if(FOCUS) {
  focus = bind_rows(
    # 2012
    readxl::read_excel("data/2012/raw/school_designations_simple2012.xlsx") %>% 
      filter(str_detect(Status, "Focus")) %>%
      transmute(year = 2012, system = DistrictNumber, school = SchoolNumber, focus = T),
    # 2013
    # 2014
    readxl::read_excel("projects/2014/2014_Final_school_accountability/Accountability App/focus14_output_081814_mb_FINAL_081814.xlsx") %>% 
      filter(focus_any_pathway == 1) %>% 
      transmute(year = 2014, system = system_number, school = school_number, focus = T),
    # 2015
    read_csv("projects/2015/2015_school_coding/Output/focus_schools_not_exiting_ap.csv") %>% 
      transmute(year = 2015, system, school, focus = T),
    # 2016
    # 2017
    read_csv("projects/2017_school_accountability/focus_schools_not_exiting.csv") %>% 
      transmute(year = 2017, system, school, focus = T),
    # 2018
    readxl::read_excel("data/2018_final_accountability_files/school_designations_file.xlsx") %>% 
      filter(str_detect(designation, "Targeted")) %>% 
      transmute(year = 2018, system, school, focus = T)
  ) %>% 
    # Crosswalk Shelby County and municipals
    left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
              by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
    mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
           school = ifelse(is.na(new_sch_id), school, new_sch_id)) %>% 
    # Check if the school is currently active
    select(-ends_with("_id")) %>% 
    left_join(
      dbGetQuery(
        sde_con,
        "select
        bu.nces_identifier,
        cast(district_number as int) as system,
        cast(school_number as int) as school,
        bu.bu_name as school_name,
        bu.status,
        op.op_end_date as close_date
        from school s
        join business_unit bu on bu.bu_id = s.bu_id
        join operational_period op on op.bu_id = bu.bu_id"
    ) %>% 
      janitor::clean_names(), by = c("system", "school")
  ) %>% 
  mutate(close_date = case_when(
    status == "I" ~ ymd_hms(close_date)
  )) %>% 
  group_by(year, system, school, school_name, focus) %>% 
  summarize(status = first(status), 
            nces_identifier = max(nces_identifier, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(
    bind_rows(
      # 2012
      readxl::read_excel("C:/Users/CA19130/Downloads/data_2012_school_profile.xlsx") %>% 
        transmute(year = 2012, system = district, school, adm = as.numeric(`Average Daily Membership`)),
      # 2014
      readxl::read_excel("C:/Users/CA19130/Downloads/data_2014_school_profile.xlsx") %>% 
        transmute(year = 2014, system = district, school = SCHOOL_ID, adm = Average_Daily_Membership),
      # 2015
      readxl::read_excel("C:/Users/CA19130/Downloads/data_2015_school_profile.xlsx") %>% 
        transmute(year = 2015, system = DISTRICT, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
      # 2017
      readxl::read_excel("C:/Users/CA19130/Downloads/data_2016-17_school_profile.xlsx") %>% 
        transmute(year = 2017, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
      # 2018
      readxl::read_excel("C:/Users/CA19130/Downloads/school_profile_2017-18.xlsx") %>% 
        transmute(year = 2018, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP)
    ) %>%
      left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
                by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
      mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
             school = ifelse(is.na(new_sch_id), school, new_sch_id)), by = c("system", "school", "year")
  )
  
  # Map loop
  for(yr in sort(unique(focus$year))) {
    ggplot(data = filter(focus, year == yr) %>% 
             left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk.dta") %>%
                         transmute(system, system_name), by = "system") %>%
             mutate(system_name = case_when(
               system_name == "Franklin" ~ "Franklin SSD",
               system_name == "H Rock Bruceton" ~ "Hollow Rock - Bruceton",
               !is.na(system_name) ~ system_name
             )) %>%
             inner_join(fortify(tn_geo@data), by = c("nces_identifier" = "NCESSCH")),
           aes(x = LON, y = LAT)) + 
      geom_polygon(data = fortify(tn_counties, region = "CNTY_FIPS"), 
                   aes(x = long, y = lat, group = group), fill = NA, color = "black") +
      geom_point(color = "red", alpha = 0.2, aes(size = adm)) + 
      theme_void() +
      scale_fill_discrete(guide = FALSE) +
      scale_size_continuous(name = "ADM") + 
      coord_map() + 
      ggtitle(str_c("Focus Schools, ", yr - 1, "-", str_sub(yr, -2, -1))) + 
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(str_c("projects/Evan/Projects/Performance and Geography/Visuals/Map of Focus Schools and Size, ", yr - 1, "-", str_sub(yr, -2, -1), ".png"), 
           units = "in", width = 9.17, height = 4.95)
  } 
} else {
  rm(FOCUS)
}

# Priority schools
if(PRIORITY) {
  priority = bind_rows(
    # 2012
    readxl::read_excel("data/2012/raw/school_designations_simple2012.xlsx") %>% 
      filter(str_detect(Status, "Priority")) %>%
      transmute(year = 2012, system = DistrictNumber, school = SchoolNumber, priority = T),
    # 2013
    # 2014
    readxl::read_excel("projects/2014/2014_Final_school_accountability/Accountability App/focus14_output_081814_mb_FINAL_081814.xlsx") %>% 
      filter(priority == 1) %>% 
      transmute(year = 2014, system = system_number, school = school_number, priority = T),
    # 2015
    read_csv("projects/2015/2015_school_coding/Output/priority_schools_not_exiting_ap.csv") %>% 
      transmute(year = 2015, system, school, priority = T),
    # 2016
    # 2017
    read_csv("projects/2017_school_accountability/priority_schools_not_exiting.csv") %>% 
      transmute(year = 2017, system, school, priority = T),
    # 2018
    readxl::read_excel("data/2018_final_accountability_files/school_designations_file.xlsx") %>% 
      filter(str_detect(designation, "Comprehensive")) %>% 
      transmute(year = 2018, system, school, priority = T)
  ) %>% 
    # Crosswalk Shelby County and municipals
    left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
              by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
    mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
           school = ifelse(is.na(new_sch_id), school, new_sch_id)) %>% 
    # Check if the school is currently active
    select(-ends_with("_id")) %>% 
    left_join(
      dbGetQuery(
        sde_con,
        "select
        bu.nces_identifier,
        cast(district_number as int) as system,
        cast(school_number as int) as school,
        bu.bu_name as school_name,
        bu.status,
        op.op_end_date as close_date
        from school s
        join business_unit bu on bu.bu_id = s.bu_id
        join operational_period op on op.bu_id = bu.bu_id"
      ) %>% 
        janitor::clean_names(), by = c("system", "school")
      ) %>% 
    mutate(close_date = case_when(
      status == "I" ~ ymd_hms(close_date)
    )) %>% 
    group_by(year, system, school, school_name, priority) %>% 
    summarize(status = first(status), 
              nces_identifier = max(nces_identifier, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(
      bind_rows(
        # 2012
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2012_school_profile.xlsx") %>% 
          transmute(year = 2012, system = district, school, adm = as.numeric(`Average Daily Membership`)),
        # 2014
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2014_school_profile.xlsx") %>% 
          transmute(year = 2014, system = district, school = SCHOOL_ID, adm = Average_Daily_Membership),
        # 2015
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2015_school_profile.xlsx") %>% 
          transmute(year = 2015, system = DISTRICT, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
        # 2017
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2016-17_school_profile.xlsx") %>% 
          transmute(year = 2017, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
        # 2018
        readxl::read_excel("C:/Users/CA19130/Downloads/school_profile_2017-18.xlsx") %>% 
          transmute(year = 2018, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP)
      ) %>%
        left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
                  by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
        mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
               school = ifelse(is.na(new_sch_id), school, new_sch_id)), by = c("system", "school", "year")
    )
  
  for(yr in sort(unique(priority$year))) {
    ggplot(data = filter(priority, year == yr) %>% 
             left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk.dta") %>%
                         transmute(system, system_name), by = "system") %>%
             mutate(system_name = case_when(
               system_name == "Franklin" ~ "Franklin SSD",
               system_name == "H Rock Bruceton" ~ "Hollow Rock - Bruceton",
               !is.na(system_name) ~ system_name
             )) %>%
             inner_join(fortify(tn_geo@data), by = c("nces_identifier" = "NCESSCH")),
           aes(x = LON, y = LAT)) + 
      geom_polygon(data = fortify(tn_counties, region = "CNTY_FIPS"), 
                   aes(x = long, y = lat, group = group), fill = NA, color = "black") +
      # geom_point(color = "red", alpha = 0.2, aes(size = adm)) + 
      geom_point(color = 'red', alpha = 0.4) + 
      theme_void() +
      scale_fill_discrete(guide = FALSE) +
      # scale_size_continuous(name = "ADM") + 
      coord_map() + 
      ggtitle(str_c("Priority Schools, ", yr - 1, "-", str_sub(yr, -2, -1))) + 
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(str_c("projects/Evan/Projects/Performance and Geography/Visuals/Map of Priority Schools, ", yr - 1, "-", str_sub(yr, -2, -1), ".png"), 
           units = "in", width = 9.17, height = 4.95)
  } 
} else {
  rm(PRIORITY)
}

# Reward schools 
if(REWARD) {
  reward = bind_rows(
    # 2012
    readxl::read_excel("data/2012/raw/school_designations_simple2012.xlsx") %>% 
      filter(str_detect(Status, "Reward")) %>%
      transmute(year = 2012, system = DistrictNumber, school = SchoolNumber, reward = T),
    # 2013
    # 2014
    readxl::read_excel("projects/2014/2014_Final_school_accountability/Accountability App/reward14_output_081814_mb_FINAL_081814.xlsx") %>% 
      filter(reward_performance == 1 | reward_progress == 1) %>% 
      transmute(year = 2014, system = system_number, school = school_number, reward = T),
    # 2015
    read_dta("projects/2015/2015_school_coding/Output/reward_2015_ap.dta") %>% 
      filter(reward == 1) %>% 
      transmute(year = 2015, system, school, reward = T),
    # 2016
    # 2017
    read_csv("projects/2017_school_accountability/reward.csv") %>% 
      filter(reward_performance == 1 | reward_progress == 1 | reward_progress_new == 1) %>% 
      transmute(year = 2017, system, school, reward = T),
    # 2018
    readxl::read_excel("data/2018_final_accountability_files/school_designations_file.xlsx") %>% 
      filter(str_detect(designation, "Reward")) %>% 
      transmute(year = 2018, system, school, reward = T)
  ) %>% 
    # Crosswalk Shelby County and municipals
    left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
              by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
    mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
           school = ifelse(is.na(new_sch_id), school, new_sch_id)) %>% 
    # Check if the school is currently active
    select(-ends_with("_id")) %>% 
    left_join(
      dbGetQuery(
        sde_con,
        "select
        bu.nces_identifier,
        cast(district_number as int) as system,
        cast(school_number as int) as school,
        bu.bu_name as school_name,
        bu.status,
        op.op_end_date as close_date
        from school s
        join business_unit bu on bu.bu_id = s.bu_id
        join operational_period op on op.bu_id = bu.bu_id"
      ) %>% 
        janitor::clean_names(), by = c("system", "school")
      ) %>% 
    mutate(close_date = case_when(
      status == "I" ~ ymd_hms(close_date)
    )) %>% 
    group_by(year, system, school, school_name, reward) %>% 
    summarize(status = first(status), 
              nces_identifier = max(nces_identifier, na.rm = T)) %>% 
    ungroup() %>% 
    left_join(
      bind_rows(
        # 2012
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2012_school_profile.xlsx") %>% 
          transmute(year = 2012, system = district, school, adm = as.numeric(`Average Daily Membership`)),
        # 2014
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2014_school_profile.xlsx") %>% 
          transmute(year = 2014, system = district, school = SCHOOL_ID, adm = Average_Daily_Membership),
        # 2015
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2015_school_profile.xlsx") %>% 
          transmute(year = 2015, system = DISTRICT, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
        # 2017
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2016-17_school_profile.xlsx") %>% 
          transmute(year = 2017, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
        # 2018
        readxl::read_excel("C:/Users/CA19130/Downloads/school_profile_2017-18.xlsx") %>% 
          transmute(year = 2018, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP)
      ) %>%
        left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
                  by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
        mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
               school = ifelse(is.na(new_sch_id), school, new_sch_id)), by = c("system", "school", "year")
    )
  
  # Urbanicity
  
  # Map loop
  for(yr in sort(unique(reward$year))) {
    ggplot(data = filter(reward, year == yr) %>% 
             left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk.dta") %>%
                         transmute(system, system_name), by = "system") %>%
             mutate(system_name = case_when(
               system_name == "Franklin" ~ "Franklin SSD",
               system_name == "H Rock Bruceton" ~ "Hollow Rock - Bruceton",
               !is.na(system_name) ~ system_name
             )) %>%
             inner_join(fortify(tn_geo@data), by = c("nces_identifier" = "NCESSCH")),
           aes(x = LON, y = LAT)) + 
      geom_polygon(data = fortify(tn_counties, region = "CNTY_FIPS"), 
                   aes(x = long, y = lat, group = group), fill = NA, color = "black") +
      # geom_point(color = "red", alpha = 0.2, aes(size = adm)) + 
      geom_point(color = 'red', alpha = 0.4) + 
      theme_void() +
      scale_fill_discrete(guide = FALSE) +
      # scale_size_continuous(name = "ADM") + 
      coord_map() + 
      ggtitle(str_c("Reward Schools, ", yr - 1, "-", str_sub(yr, -2, -1))) + 
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(str_c("N:/ORP_accountability/projects/Evan/School Improvement/Map of Reward Schools, ", yr - 1, "-", str_sub(yr, -2, -1), ".png"), 
           units = "in", width = 9.17, height = 4.95)
  } 
} else {
  rm(REWARD)
}

# Landscape analysis - TCAP
if(SCALE) {
  landscape = bind_rows(
    # 2012
    read_dta("data/2012/stata/school_accountability_2012.dta") %>%
      rename(system = District, school = School, year = SchoolYear) %>% 
      filter(year == 2012) %>% 
      mutate(school = as.numeric(school)) %>%
      group_by(year, system, school) %>% 
      summarize(success_rate = round(100 * sum(AllProficientAdvanced, na.rm = T) / sum(AllTested, na.rm = T), 1)) %>% 
      ungroup() %>% 
      mutate(percentile = round(100 * percent_rank(success_rate), 1)),
    # 2013
    read_csv("data/2013/2013_sas_accountability/raw/school_numeric_2013_05aug2013.csv") %>% 
      filter(year == 2013 & subgroup == "All Students" & str_detect(grade, "through") & 
               subject %in% c("Math", "RLA", "Algebra I", "Algebra II", "English II", "English III")) %>%
      transmute(year, system = as.numeric(system), school = as.numeric(school), n_prof_adv = n_prof + n_adv, valid_tests) %>% 
      group_by(year, system, school) %>% 
      summarize(success_rate = round(100 * sum(n_prof_adv, na.rm = T) / sum(valid_tests, na.rm = T), 1)) %>% 
      ungroup() %>% 
      mutate(percentile = round(100 * percent_rank(success_rate), 1)),
    # 2014
    readxl::read_excel("projects/2014/2014_Final_school_accountability/Accountability App/reward14_output_081814_mb_FINAL_081814.xlsx") %>% 
      transmute(year = 2014, system = system_number, school = school_number, success_rate = one_year_success_rate2014,
                percentile = round(100 * percent_rank(one_year_success_rate2014), 1)),
    # 2015
    read_dta("projects/2015/2015_school_coding/Output/reward_2015_ap.dta") %>% 
      transmute(year = 2015, system, school, success_rate = pct_PA, 
                percentile = round(100 * percent_rank(pct_PA), 1)),
    # 2016
    # 2017
    read_csv("projects/2017_school_accountability/reward_schools_10112017.csv") %>% 
      arrange(system, school) %>% 
      transmute(year, system, school, success_rate, percentile = round(100 * percent_rank(success_rate), 1)),
    # 2018
    read_csv("data/2018_final_accountability_files/2018_school_accountability_file.csv") %>% 
      filter(indicator == "Achievement" & subgroup == "All Students") %>%
      transmute(year = 2018, system, school, success_rate = metric, 
                percentile = round(100 * percent_rank(metric), 1))
  ) %>% 
    # Crosswalk Shelby County and municipals
    left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
              by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
    mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
           school = ifelse(is.na(new_sch_id), school, new_sch_id)) %>% 
    # Check if the school is currently active
    select(-ends_with("_id")) %>% 
    left_join(
      dbGetQuery(
        sde_con,
        "select
        bu.nces_identifier,
        cast(district_number as int) as system,
        cast(school_number as int) as school,
        bu.bu_name as school_name,
        bu.status,
        op.op_end_date as close_date
        from school s
        join business_unit bu on bu.bu_id = s.bu_id
        join operational_period op on op.bu_id = bu.bu_id"
      ) %>% 
        janitor::clean_names(), by = c("system", "school")
      ) %>% 
    mutate(close_date = case_when(
      status == "I" ~ ymd_hms(close_date)
    )) %>% 
    group_by(year, system, school, school_name) %>% 
    summarize_at(vars(success_rate:nces_identifier), funs(max(., na.rm = T))) %>% 
    ungroup() %>% 
    left_join(
      bind_rows(
        # 2012
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2012_school_profile.xlsx") %>% 
          transmute(year = 2012, system = district, school, adm = as.numeric(`Average Daily Membership`)),
        # 2013 
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2013_school_profile.xlsx") %>% 
          transmute(year = 2013, system = as.numeric(DISTRICT), school = as.numeric(`SCHOOL NO`), 
                    adm = as.numeric(`Average Daily Membership`)),
        # 2014
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2014_school_profile.xlsx") %>% 
          transmute(year = 2014, system = district, school = SCHOOL_ID, adm = Average_Daily_Membership),
        # 2015
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2015_school_profile.xlsx") %>% 
          transmute(year = 2015, system = DISTRICT, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
        # 2017
        readxl::read_excel("C:/Users/CA19130/Downloads/data_2016-17_school_profile.xlsx") %>% 
          transmute(year = 2017, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP),
        # 2018
        readxl::read_excel("C:/Users/CA19130/Downloads/school_profile_2017-18.xlsx") %>% 
          transmute(year = 2018, system = DISTRICT_ID, school = SCHOOL_ID, adm = AVERAGE_DAILY_MEMBERSHIP)
      ) %>%
        left_join(read_dta("N:/Research and Policy/ORP_Data/Resources/Crosswalks/Raw_Files/Shelby county district numbers/c2shelbycw.dta"),
                  by = c("system" = "old_sys_id", "school" = "old_sch_id")) %>% 
        mutate(system = ifelse(is.na(new_sys_id), system, new_sys_id),
               school = ifelse(is.na(new_sch_id), school, new_sch_id)
      ), by = c("system", "school", "year")
  )
  
  # Map loop
  for(yr in sort(unique(landscape$year))) {
    ggplot(data = filter(landscape, year == yr) %>% 
             left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk.dta") %>%
                         transmute(system, system_name), by = "system") %>%
             mutate(system_name = case_when(
               system_name == "Franklin" ~ "Franklin SSD",
               system_name == "H Rock Bruceton" ~ "Hollow Rock - Bruceton",
               !is.na(system_name) ~ system_name
             )) %>%
             inner_join(fortify(tn_geo@data), by = c("nces_identifier" = "NCESSCH")),
           aes(x = LON, y = LAT)) + 
      geom_polygon(data = fortify(tn_counties, region = "CNTY_FIPS"), 
                   aes(x = long, y = lat, group = group), fill = NA, color = "black") +
      # geom_point(color = "red", alpha = 0.2, aes(size = adm)) + 
      geom_point(aes(color = percentile)) + 
      theme_void() +
      scale_fill_discrete(guide = FALSE) +
      scale_color_continuous(name = "Percentile", low = "#af8dc3", high = "#7fbf7b", limits = c(0, 100)) + 
      # scale_size_continuous(name = "ADM") + 
      coord_map() + 
      ggtitle(str_c("School Performance (TCAP Percentile), ", yr - 1, "-", str_sub(yr, -2, -1))) + 
      theme(plot.title = element_text(hjust = 0.5))
    ggsave(str_c("N:/ORP_accountability/projects/Evan/School Improvement/Map of School Performance, ", yr - 1, "-", str_sub(yr, -2, -1), ".png"),
           units = "in", width = 9.17, height = 4.95)
  } 
} else {
  rm(SCALE)
}

# Analysis
if(ANALYSIS) {
  # How many schools have ever been focus? In how many districts? 
  n_distinct(focus$system, focus$school); n_distinct(focus$system)
  
  # Current status 
  n_distinct(focus$system[focus$year == year(today()) - 1], focus$school[focus$year == year(today()) - 1])
  n_distinct(focus$system[focus$year == year(today()) - 1])
  filter(focus, year == year(today()) - 1) %>% 
    group_by(system) %>% 
    summarize(n_schools = n_distinct(school)) %>% 
    left_join(read_dta("C:/Users/CA19130/Documents/Data/Crosswalks/core_region_crosswalk.dta") %>%
                transmute(system, system_name), by = "system") 
  
  # Number of years identified
  group_by(focus, system, school) %>% 
    summarize(max_years_identified = max(n_distinct(year), na.rm = T)) %>% 
    group_by(max_years_identified) %>% 
    summarize(n = n())
} else {
  rm(ANALYSIS)
}

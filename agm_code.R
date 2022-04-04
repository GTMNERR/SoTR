
# THIS RUNS ALL CODE - FROM READ IN TO GRAPHING 
#NEW CODE FOR AGM MEANS using the 4yr xlsx instead of clip so I can get 2021 data 
## ???
here::here('data')

## :: means look in this package to use this function, 
## %>%  pass the left hand side of the operator to the first argument 
## of the right hand side of the operator

###clip data file cliped out about 1 yr of NO23 based on reporting at MDL
##WORKS
dat <- readxl::read_xlsx(here::here('data', 'Guana_MD2021_DEPclip.xlsx'), 
                         sheet = 'Sheet1') %>% 
  janitor::clean_names()

## change column name to work with previously written code
dat <- rename(dat, date_sampled = sample_date)

# data dictionary with site-specific information
dict <- readr::read_csv(here::here('data', 'guana_data_dictionary.csv')) %>%
  janitor::clean_names()


# inspect the data file
head(dat)
str(dat)
dplyr::glimpse(dat) # this one is my favorite to use

## remove dup samples
## cleaning up data, selecting columns we want 
# removing all but wind and secchi, all component toupper (not sure why)
dat2 <- dat %>%
  dplyr::filter(station_code != "GTMOLNUT_dup") %>%  # remove the 'duplicate' station that was only sampled for a short while
  dplyr::select(unit,
                station_code,
                date_sampled,
                component_short,
                component_long,
                result,
                remark,
                flag) %>%
  dplyr::filter(!(component_short %in% c("WIND_D", "SECCHI"))) %>% # remove wind direction and secchi
  dplyr::mutate(component_long = toupper(component_long),
                component_short = toupper(component_short))

# CAUTION: rewrites over dat2 dataframe created in previous lines
# keeps 'data' as ORIGINAL dat that you read in
dat2 <- dplyr::left_join(dat2, dict, by = "station_code")

# make sure both short and long have the same number of entries (~69)
# should be the same number of entries!
unique(dat2$component_short) # will pull out all the unique component names
unique(dat2$component_long) # will pull out the unique component names

##check for duplicates
janitor::get_dupes(dat2)

##check dat for NA or BLANKS ?? I have quite a few??
View(dat2 %>% dplyr::filter(is.na(result)))

## remove NA files
dat2 <- dat2 %>% dplyr::filter(result != "NA")



##rewrite data2, formatting time and sample and char

dat2 <- dat2 %>%
  dplyr::mutate(date_sampled = as.POSIXct(date_sampled,
                                          format = "%m/%d/%Y %H:%M",
                                          tz = 'America/Regina'),
                result = as.numeric(result),
                month = month(date_sampled),
                day = day(date_sampled),
                year = as.character(year(date_sampled)), # set year as a character
                site = factor(site,
                              levels = c("MICKLERS",
                                         "DEPGL1",
                                         "DEPGL2",
                                         "LAKE MIDDLE",
                                         "DEPGL4",
                                         "LAKE SOUTH",
                                         "DEPGR1",
                                         "GUANA RIVER",
                                         "DEPGR3")),
                site_friendly = factor(site_friendly,
                                       levels = c("Micklers",
                                                  "GL1",
                                                  "GL2",
                                                  "Lake Middle",
                                                  "GL4",
                                                  "Lake South",
                                                  "GR1",
                                                  "Guana River",
                                                  "GR3"))
  )

## Load Coloring Script
## Load plotting script



#remove bad data - flagged data
dat3 <- dat2 %>% dplyr::filter(!grepl("-3", flag))

#BETTER WAY TO DO MULTI FILTER ABOVE THAN BELOW


# dplyr::filter(flag != "<-3> (CHB)(SUL)") %>%
#   dplyr::filter(flag != "<-3> (CSM)") %>% 
#     dplyr::filter(flag != "<-3> [GCM]")


##Couldn't alter data on the same row with results so had to pull out each parameter and pivot
## those independently, to the 'left join' the charts together at the end. 

#This code is filtering my main data set for ONE param then selecting the columns I want to keep
# then mutating the data with a ROW column to give each value a unique identifier (code wouldn't run
# without it) then using that newly formed ONE param data frame and pivoting it, then converting the 
# results of that param into their uM miromule format for better comparison to other data.Then deleting the
# ROW column cuz I don't need it.

NH4 <- dat3 %>% 
  filter(component_short == "NH4F") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  mutate(NH4uM = NH4F * (1000/14.01))  %>% 
  select(-row)


ggplot(NH4, mapping = aes(x = date_sampled, y = NH4uM)) +
  geom_point()


NO23 <- dat3 %>% 
  filter(component_short == "NO23F") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  mutate(NO23uM = NO23F * (1000/14.01)) %>% 
  select(-row)

ggplotly(ggplot(NO23, mapping = aes(x = date_sampled, y = NO23uM)) +
           geom_point())

TKN <- dat3 %>% 
  filter(component_short == "TKN") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  mutate(TKNuM = TKN * (1000/14.01)) %>% 
  select(-row)

ggplot(TKN, mapping = aes(x = date_sampled, y = TKNuM)) +
  geom_point()

TKNF <- dat3 %>% 
  filter(component_short == "TKNF") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  mutate(TKNFuM = TKNF * (1000/14.01)) %>% 
  select(-row)

ggplot(TKNF, mapping = aes(x = date_sampled, y = TKNFuM)) +
  geom_point()

CHLA <- dat3 %>% 
  filter(component_short == "CHLA_C") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  select(-row)

ggplot(CHLA, mapping = aes(x = date_sampled, y = CHLA_C)) +
  geom_point()



TSS <- dat3 %>% 
  filter(component_short == "TSS") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  select(-row)

ggplot(TSS, mapping = aes(x = date_sampled, y = TSS)) +
  geom_point()

DIP <- dat3 %>% 
  filter(component_short == "PO4F") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  rename(DIP = PO4F) %>%
  mutate(DIPuM = DIP * (1000/30.97)) %>%
  select(-row)

ggplot(DIP, mapping = aes(x = date_sampled, y = DIPuM)) +
  geom_point()

TP <- dat3 %>% 
  filter(component_short == "TP") %>% 
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  mutate(TPuM = TP * (1000/30.97)) %>%
  select(-row)

ggplot(TP, mapping = aes(x = date_sampled, y = TPuM)) +
  geom_point()

SALT <- dat3 %>%
  filter(component_short == "SALT") %>%
  dplyr::select(date_sampled,
                site,
                wbid,
                component_short,
                result) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = component_short, 
              values_from = result) %>% 
  select(-row)

#This code is left joining all the dataframes I just created back into one! 
stats <- NH4 %>% 
  left_join(NO23, by = c("site", "wbid", "date_sampled")) %>% 
  left_join(TKN, by = c("site", "wbid", "date_sampled")) %>% 
  left_join(TKNF, by = c("site", "wbid", "date_sampled")) %>%
  left_join(CHLA, by = c("site", "wbid", "date_sampled")) %>%
  left_join(TSS, by = c("site", "wbid", "date_sampled")) %>%
  left_join(DIP, by = c("site", "wbid", "date_sampled")) %>%
  left_join(TP, by = c("site", "wbid", "date_sampled")) %>%
  left_join(SALT, by = c("site", "wbid", "date_sampled"))

# clean the work space environment
rm(NH4, NO23, TKN, TKNF, TSS, CHLA, TP, DIP, SALT)





#----------- GEOMETRIC MEAN ------------------------
#Trying to determine geometric mena for GUana River to see if it exceeds 0.65 more than once 
#over a 3 year period
## Mutate dataframe to pull out year and month and just TN values by average by month I think

meanstats <- stats %>%
  mutate(TN = TKN + NO23F)

mean_monthly <- meanstats %>%
  dplyr::group_by(site, wbid, date_sampled) %>%
  dplyr::summarise(TN_avg = mean(TN, na.rm = TRUE),
                   TP_avg = mean(TP, na.rm = TRUE),
                   CHLA_avg = mean(CHLA_C, na.rm = TRUE),
                   .groups = "keep") %>%
  dplyr::mutate(YEAR = lubridate::year(date_sampled),
                MONTH_abb = lubridate::month(date_sampled, label = TRUE, abbr = TRUE),
                MONTH = lubridate::month(date_sampled),
                site = factor(site,
                              levels = c("GUANA RIVER",
                                         "LAKE MIDDLE",
                                         "LAKE SOUTH",
                                         "MICKLERS",
                                         "DEPGL4",
                                         "DEPGL2",
                                         "DEPGL1",
                                         "DEPGR3",
                                         "DEPGR1"))) 


## Monthly averages to yearly 
# annual geometric mean function 
# gmean <- function(x) exp(mean(log(x), na.rm = TRUE))
# or use the psych::geometric.mean() function 


mean_yearly <- mean_monthly %>% 
  dplyr::group_by(wbid, YEAR) %>%
  dplyr::summarise(TN_agm = psych::geometric.mean(TN_avg, na.rm = T),
                   TP_agm = psych::geometric.mean(TP_avg, na.rm = T),
                   CHLA_agm = psych::geometric.mean(CHLA_avg, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(YEAR = forcats::as_factor(YEAR))


## NEED TO PIVOT longer for graphing purposes

mean_yearly_pivot <- mean_yearly %>%
  pivot_longer(cols = 3:5,
               names_to = "nut",
               values_to = "agm")


##Need to graph each AGM by year by parameter and by site?? 

agm_year <- function(param, space, axis_title) {
  # param - use component_short parameter name in quotes
  
  
  p <- mean_yearly_pivot %>%
    dplyr::filter(param == nut, space == wbid) %>%
    ggplot(aes(x = YEAR, y = agm)) +
    geom_line(size = 0.5, group = 1) +
    geom_point(aes(color = agm <= 0.65), size = 3) +
    geom_hline(yintercept = 0.65) +
    scale_color_manual(name = "State \nThreshold \n0.65 mg/L \nNitrogen",
                       labels = c("Above", "Below"),
                       values = c('brown3', 'yellow')) +
    coord_cartesian(ylim = c(0,2)) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(angle = 0, vjust=0.3, size=12, color='black')) +
    labs(y = axis_title,
         x = "",
         title = paste(space))
  
  p
}

agm_year("TN_agm", "Lake", "Geometric Mean Nitrogen (mg/L)")
agm <- agm_year("TN_agm", "River", "Geometric Mean Nitrogen (mg/L)")

ggsave(agm,
       filename = here('output', 'agm_year_riverclip.png'))




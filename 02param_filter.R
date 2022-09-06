#BETTER PIVOT OF DATA 
## EASIER FOR ANALYSIS 

# --->> Run Load_pivot first 

#remove bad data - flagged data
dat3 <- dat2 %>% dplyr::filter(!grepl("-3", flag))
dat3 <- dat3 %>% dplyr::filter(!grepl("GTMGL1.5NUT", station_code))
dat3 <- dat3 %>% dplyr::filter(!grepl("GTMGL2.5NUT", station_code))
dat3 <- dat3 %>% dplyr::filter(!grepl("GTMGL3.5NUT", station_code))
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
                             site_friendly,
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
                      site_friendly,
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
                      site_friendly,
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
                      site_friendly,
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
                      site_friendly,
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
                      site_friendly,
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
                      site_friendly,
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
                    site_friendly,
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
                      site_friendly,
                      wbid,
                      component_short,
                      result) %>%
        mutate(row = row_number()) %>%
        pivot_wider(names_from = component_short, 
              values_from = result) %>% 
        select(-row)

#This code is left joining all the dataframes I just created back into one! 
stats <- NH4 %>% 
  left_join(NO23, by = c("site_friendly", "wbid", "date_sampled")) %>% 
  left_join(TKN, by = c("site_friendly", "wbid", "date_sampled")) %>% 
  left_join(TKNF, by = c("site_friendly", "wbid", "date_sampled")) %>%
  left_join(CHLA, by = c("site_friendly", "wbid", "date_sampled")) %>%
  left_join(TSS, by = c("site_friendly", "wbid", "date_sampled")) %>%
  left_join(DIP, by = c("site_friendly", "wbid", "date_sampled")) %>%
  left_join(TP, by = c("site_friendly", "wbid", "date_sampled")) %>%
  left_join(SALT, by = c("site_friendly", "wbid", "date_sampled"))

# clean the work space environment
rm(NH4, NO23, TKN, TKNF, TSS, CHLA, TP, DIP, SALT)



#calculate for our other parameters 
# mutate creates a new column and fills with 
# ifelse translates NaN to Na
stats2 <- stats %>%
          mutate(DATE = as.Date(date_sampled)) %>%
          mutate(DIN = NH4F + NO23F,
                 TN = TKN + NO23F,
                 DON = TKNF - NH4F,
                 PN = TN - (DIN + DON),
                 DINuM = NH4uM + NO23uM,
                 TNuM = TKNuM + NO23uM,
                 DONuM = TKNFuM - NH4uM,
                 PNuM = TNuM - (DINuM + DONuM)) %>%
          #mutate_all(~ifelse(is.nan(.), NA, .)) %>%
          mutate(DATE = lubridate::as_date(DATE))


                 
#linear regression model 
#separate out by site - River and Lake and run lm 
NOT USING LM SINCE MY VALUES ARE NEGATIVE AND I DONT HAVE MUCH TSS DATA...
# 
# # LAKE - filtering for just lake sites
# lake <- stats2 %>% filter(wbid == "Lake" & PNuM > 0)
# 
# #running lm looking at Particulare Nitrogen as a function of TSS and CHLA_C
# ## then asking for summary stats and tidy data
# lake_fit <- lm(PNuM ~ TSS + CHLA_C, data = na.exclude(lake))
# summary(lake_fit)
# broom::tidy(lake_fit)
# 
# #using est coefficients to determine Nsed and Nphyt
# 
# lake <- lake %>%
#         mutate(N_sed = 0.136*TSS,
#                N_phyto = -0.314*CHLA_C)
#         
# ##RIVER
#         
# river <- stats2 %>% filter(wbid == "River" & PNuM > 0)
# 
# river_fit <- lm(PNuM ~ TSS + CHLA_C, data = river)
# summary(river_fit)
# broom::tidy(river_fit)
# 
# 
# river <- river %>% 
#          mutate(N_sed = -0.476*TSS,
#                 N_phyto = 1.48*CHLA_C)
#          



#merge all back into one dataframe and calculating for nutrient limitation indicies
# also running my stoichiometry comparisions 
# also converting Nsed and Nphyto back into mg/L

stats3 <- stats2 %>%
         mutate(N_limit = 100*(DINuM/(DINuM + 1.6)),
                P_limit = 100*(DIPuM/(DIPuM + 0.24)),
                TN_TPuM = TNuM/TPuM,
                DIN_DIPuM = DINuM/DIPuM)





#calculate min, max and mean of each parameter
sites_calc <- stats3 %>%
    group_by(wbid) %>%
    summarise(across(where(is.numeric), list(min = min, max = max, median = median, mean = mean), na.rm = TRUE))


count <- stats3 %>%
    group_by(wbid) %>%
    summarise(across(everything(), ~ n()))

#replace columns in 'count' with a _N to identify them as a count
#formula for the nutrient utilization efficeincy ratios! 

colnames(count) <- paste(colnames(count), sep = "_", "N")
count <- count %>% rename(wbid = wbid_N) %>% select(-DATE_N)
all <- sites_calc %>% 
    left_join(count, by = "wbid") %>%
    mutate (Nitrogen_eff = CHLA_C_mean/DIN_mean,
            NItrogen_effuM = CHLA_C_mean/DINuM_mean,
            Phosphorous_eff = CHLA_C_mean/DIP_mean,
            Phosphorous_effuM = CHLA_C_mean/DIPuM_mean)


write.xlsx(all, here::here("output", "N_and_P_statistics.xlsx"))











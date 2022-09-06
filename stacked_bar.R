## STACKED BAR GRAPHS 

## RUn Load and param_filter before

#  AVERAGE per sit3

avg_sitebar <- stats2 %>% 
 group_by(site_friendly, wbid) %>%
  summarise(ave_DON = mean(DON, na.rm = TRUE),
            ave_DIN = mean(DIN, na.rm = TRUE),
            ave_PN = mean(PN, na.rm = TRUE))

#DONT NEED THIS CODE SINCE I GOT RID OF THE .5 SITES EARLIER 
avg_sitebar <- avg_sitebar %>% dplyr::filter(site != "NA")
stats2 <- stats2 %>% dplyr::filter(site != "NA")


sitespivot <- avg_sitebar %>% 
  #filter(ave_PN > 0) %>%
  pivot_longer(cols = 3:5,
               names_to = "nitro_source",
               values_to = "conc")

GTMpi <- sitespivot%>%
  full_join(PIave, by =c("site_friendly", 'conc', "nitro_source"))

GTMpi$site_friendly <- factor(GTMpi$site_friendly, levels=c("Micklers", "GL1", "GL2", "Lake Middle", "GL4", "Lake South","River North", "GR1", "Guana River", "GR3", "PI"))

N_spp <- GTMpi%>%
  ggplot(aes(x = site_friendly, y = conc, fill = nitro_source)) +
  geom_col()+
  #facet_wrap(~wbid) + 
  scale_fill_okabeito(labels = c('DIN', 'DON', 'PN')) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1.0, hjust = 1.0)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Nitrogen (mg/L)",
       fill = "Nitrogen Source")

ggsave(plot = N_spp, filename = here('output', 'TN_PI_spp.png'), dpi=120)
  
## ----- Average by year and site over time----------
## NO FINISHED?
  
ave_byYear <- stats2 %>% 
  filter(PN > 0) %>%
  select(wbid,
         site,
         DATE,
         DON, 
         DIN, 
         PN) %>%
dplyr::mutate(month = month(DATE),
              day = day(DATE),
              year = as.character(year(DATE)))

year1 <- ave_byYear %>%
  filter(year == 2017) %>% 
  group_by(site, wbid) %>%
  summarise(ave_DON = mean(DON, na.rm = TRUE),
            ave_DIN = mean(DIN, na.rm = TRUE),
            ave_PN = mean(PN, na.rm = TRUE))%>%
  select(site, ave_DON, ave_DIN, ave_PN, wbid)

 




  pivot_longer(cols = 4:6,
               names_to = "nitro_source",
               values_to = "conc") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col()+
  facet_wrap(~wbid) + 
  scale_fill_okabeito() +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Nitrogen (mg/L)",
       fill = "Nitrogen Source")

  

  
#---------# nitrogen stacked graph in mg.L
 sitespivot <- stats2 %>% 
   filter(PN > 0) %>%
   select(wbid,
           site,
           DATE,
           DON, 
           DIN, 
           PN) %>%
   pivot_longer(cols = 4:6,
                 names_to = "nitro_source",
                 values_to = "conc") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col()+
  facet_wrap(~wbid) + 
  scale_fill_okabeito() +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Nitrogen (mg/L)",
       fill = "Nitrogen Source")
 


ggsave(plot = b, 
       filename = here("output", "NStackBarmgL.png"), dpi = 120)


#nitrogen uM

stats2 %>% 
  filter(PN > 0) %>%
  select(wbid,
         DATE,
         DONuM, 
         DINuM, 
         PNuM) %>%
  pivot_longer(cols = 3:5,
               names_to = "nitro_source",
               values_to = "conc") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col()+
  facet_wrap(~wbid) +
  scale_fill_discrete(name = "") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Nitrogen (uM)")


# FAKING PI SITE JUST SETTING TO SAME AXIS TO PUT NEXT TO MY STACKED BAR
Pine <- PI2 %>% 
  filter(PN > 0) %>%
  select(WBID,
         DATE,
         DON, 
         DIN, 
         PN) %>%
  pivot_longer(cols = 3:5,
               names_to = "nitro_source",
               values_to = "conc") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col()+
  facet_wrap(~WBID) + 
  scale_fill_okabeito() +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, 17)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Nitrogen (mg/L)",
       fill = "Nitrogen Source")

ggsave(plot = Pine, 
       filename = here("output", "PIstack.png"), dpi = 120)

#TRYING WITH P
## can wrap the whole thing in ggplotly (in plotly libary) to get interactive graph!!!!

PHO <- sites %>%
  select(wbid, DATE, DIP, TP) %>%
  ggplot(aes(x = DATE)) +
  #geom_col(aes(y = DIP)) +
  geom_point(aes(y = TP)) +
  geom_line(aes(y = TP)) +
  facet_wrap(~wbid) +
  scale_fill_discrete(name ="") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Phosphorus (mg/L)")

ggsave(plot = PHO, filename = here("output", "PHOsplit.png"), dpi = 120)


 # #TP uM
sites %>%
  select(wbid, DATE, DIPuM, TPuM) %>%
  ggplot(aes(x = DATE)) +
  #geom_col(aes(y = DIP)) +
  geom_point(aes(y = TPuM)) +
  geom_line(aes(y = TPuM)) +
  facet_wrap(~wbid) +
  scale_fill_discrete(name ="") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = '',
       y = "Phosphorus (uM)")



## Chlorophyll mgL

CHL <- stats2 %>%
  select(wbid, DATE, CHLA_C) %>%
  ggplot(aes(x = DATE)) +
  #geom_col(aes(y = DIP)) +
  geom_point(aes(y = CHLA_C)) +
  geom_line(aes(y = CHLA_C)) +
  facet_wrap(~wbid) +
  scale_fill_discrete(name ="") +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Chlorophyll a (\U3BCg/L)")

ggsave(plot = CHL, filename = here("output", "CHLsplit.png"), dpi = 120)






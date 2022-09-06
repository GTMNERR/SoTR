

# READ IN PI DATA FOR THE STACKED N BARCHART 
## DATA ALREADY RUN THROUGH STATS AND CALCULATIONS 

PI <- readxl::read_xlsx(here::here('data', '2022-08-29_PI-data-mo-OR_clip.xlsx'))



#all col names to upper
names(PI)[2:40] <- toupper(names(PI)[2:40])

PI <- rename(PI, DATE = DATETIMESTAMP)
                
PI2 <- PI %>%
  select(DATE, DON, DIN, PN) %>% 
  mutate(wbid = "Pine Island",
         DATE = as.Date(DATE),
         DON = as.numeric(DON),
         DIN = as.numeric(DIN),
         PN = as.numeric(PN)) %>%
  #filter(PN > 0) %>%
pivot_longer(cols = 2:4,
             names_to = "nitro_source",
             values_to = "conc") %>%
  
PIave <- PI%>%
  group_by(station_code) %>%
summarise(ave_DON = mean(DON, na.rm = TRUE),
          ave_DIN = mean(DIN, na.rm = TRUE),
          ave_PN = mean(PN, na.rm = TRUE))

PIave <- rename(PIave, site_friendly = station_code)

PIave <- PIave %>%
  pivot_longer(cols = 2:4,
               names_to = "nitro_source",
               values_to = "conc")
  


  ggplot(aes(x = wbid, y = conc, fill = nitro_source)) +
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



# 
# PI2$DON <- as.numeric(PI2$DON)
# PI2$DIN <- as.numeric(PI2$DIN)
# PI2$PN <- as.numeric(PI2$PN)



S2bind <- sites %>% 
  filter(PN > 0) %>%
  select(wbid,
         DATE,
         DON, 
         DIN, 
         PN)


#BIND WITH MY DATA

GUANASWMP <- bind_rows(PI2, S2bind, id = NULL)


GS <- GUANASWMP %>%
  select(wbid, DATE, DON, DIN, PN) %>% 
  pivot_longer(cols = 3:5,
               names_to = "nitro_source",
               values_to = "conc") %>%
  mutate(wbid = factor(wbid, 
                       levels = c("Lake", "River", "Pine Island")))

# GS$WBID <- factor(GS$WBID, levels = c("Lake", "River", "Pine Island Ref"))
# #GS$WBID2 <- reorder(GS$WBID, GS$nitro_source)

GS %>%
  # filter(wbid == "Pine Island") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col() +
  scale_fill_okabeito() +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "Nitrogen (mg/L)",
       fill = "Nitrogen Source")




lake_stacked <- GS %>%
    filter(wbid == "Lake") %>%
    ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
    geom_col() +
    scale_fill_okabeito() +
    scale_y_continuous(expand = c(0,0)) +
    coord_cartesian(ylim = c(0,17)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
    theme(axis.text = element_text(color = 'black')) +
    labs(x = '',
         y = "Nitrogen (mg/L)",
         fill = "Nitrogen Source")

river_stacked <- GS %>%
  filter(wbid == "River") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col() +
  scale_fill_okabeito() +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,17)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "",
       fill = "Nitrogen Source")

PI_stacked <- GS %>%
  filter(wbid == "Pine Island") %>%
  ggplot(aes(x = DATE, y = conc, fill = nitro_source)) +
  geom_col() +
  scale_fill_okabeito() +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,17)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.0, hjust = 1)) +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = '',
       y = "",
       fill = "Nitrogen Source")

TripleStack <- (lake_stacked + theme(legend.position = "none") +
    labs(title = "Lake")) + 
(river_stacked + theme(legend.position = "none") +
    labs(title = "River")) +
(PI_stacked + labs(title = "Pine Island"))

ggsave(plot = TripleStack, 
       filename = here("output", "L_R_PI_stack.png"), dpi = 120)

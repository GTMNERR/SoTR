

# READ IN PI DATA FOR THE STACKED N BARCHART 
## DATA ALREADY RUN THROUGH STATS AND CALCULATIONS 

PI <- readxl::read_xlsx(here::here('data', '2022-02-16_PI-data-OR-clip.xlsx'))


PI2 <- PI %>%
  select(DATE, DON, DIN, PN) %>% 
  mutate(wbid = "Pine Island",
         DATE = as.Date(DATE),
         DON = as.numeric(DON),
         DIN = as.numeric(DIN),
         PN = as.numeric(PN)) %>%
  filter(PN > 0)
  

# #all col names to upper
# names(PI2)[1:42] <- toupper(names(PI2)[1:42])
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
# names(S2bind)[1] <- toupper(names(S2bind)[1])




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

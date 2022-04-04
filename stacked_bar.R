## STACKED BAR GRAPHS 

## RUn Load and param_filter before

# nitrogen stacked graph in mg.L
sitespivot <- sites %>% 
  filter(PN > 0) %>%
  select(wbid,
         DATE,
         DON, 
         DIN, 
         PN) %>%
  pivot_longer(cols = 3:5,
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

sites %>% 
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

CHL <- sites %>%
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



## TRYING FOR simple bar charts ...... ?????
  
dat4 <- dat3 %>%
    dplyr::select(site_friendly,
                  component_short,
                  result) %>%
    dplyr::filter(component_short == "CHLA_C") %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = component_short, values_from = result) %>%
  
  
    ggplot(aes(site_friendly)) +
    geom_bar() +
    scale_y_continuous(expand = c(0,0)) +
    labs(y = "",
         x = "",
         title = paste("Chlorophyll-a"))
  


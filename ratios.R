## redfield ratios 
## 16 mol N per 1 mol P
## 7.23 : 1 N:P mg/L



## Trying to write my own code not edit previous code to graph the redfield ratio!!!!

Ratio <- stats3 %>%
  dplyr::select(DATE,
                wbid,
                TP,
                TN) %>%
  mutate(ratio = (TN/TP)) %>%
  gf_point(TN ~ TP, color = ~ wbid) +
  geom_abline(slope = 7.23, size = 0.5) +
  scale_color_manual(name = "Site",
                     values = c("Lake" = "#0072B2",
                                "River" = "#E69F00")) +
  coord_cartesian(ylim = c(0,6.5)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = "Total Phosphrous (mg/L)",
       y = "Total Nitrogen (mg/L)")



ggsave(plot = Ratio, filename = here("output", "TNTP_ratio.png"), dpi = 120)


#-----------------##GRAPHING DIN TO DON RATIO IN THE LAKE --------------------------

# adding values for site colors
lakecolours <- c(
  `MICKLERS` = "#F8766D",
  `DEPGL1` = "#D89000",
  `DEPGL2` = "#A3A500",
  `LAKE MIDDLE` = "#39B600",
  `DEPGL4` = "#00BF7D",
  `LAKE SOUTH` = "#00BFC4")

rivercolours <- c(
  `RIVER NORTH` = "#00B0F6",
  `DEPGR1` = "#9590FF",
  `GUANA RIVER` = "#E76BF3",
  `DEPGR3` = "#FF62BC")

## converting site column to factor so I can use it in the graph
stats3$site <- factor(stats3$sites)

## selecting just the Lake points

lakesites <- stats3 %>%
  dplyr::filter(wbid == "Lake")

riversites <- stats3 %>%
  dplyr::filter(wbid == "River")
  
 


## selcting parameters that I want out of already pivotted and set 
## up data set called sites

DON_DIN_Lakeratio <- lakesites %>%
    dplyr::select(date_sampled,
                  site,
                  wbid,
                  DIN,
                  DON) %>%
    mutate(ratio = (DIN/DON)) %>%
  gf_point(DIN ~ DON, color = ~ site) +
  scale_colour_manual(name = "Site", values = lakecolours) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = "DON (mg/L)",
       y = "DIN (mg/L)")
  


ggsave(plot = DON_DIN_Lakeratio, filename = here("output", "DON_DIN_Lakeratio.png"), dpi = 120)


# DIN DON RATIO FOR RIVER

DON_DIN_Riverratio <- riversites %>%
  dplyr::select(date_sampled,
                site,
                wbid,
                DIN,
                DON) %>%
  mutate(ratio = (DIN/DON)) %>%
  gf_point(DIN ~ DON, color = ~ site) +
  scale_colour_manual(name = "Site", values = rivercolours) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) +
  theme_classic() +
  theme(axis.text = element_text(color = 'black')) +
  labs(x = "DON (mg/L)",
       y = "DIN (mg/L)")

ggsave(plot = DON_DIN_Lakeratio, filename = here("output", "DON_DIN_Riverratio.png"), dpi = 120)


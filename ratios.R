## redfield ratios 
## 16 mol N per 1 mol P
## 7.23 : 1 N:P mg/L



## Trying to write my own code not edit previous code to graph the redfield ratio!!!!

Ratio <- sites %>%
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


ggsave(plot = Ratio, filename = here("output", "TNTPratio.png"), dpi = 120)


#-----------------##GRAPHING DIN TO DON RATIO IN THE LAKE --------------------------

# adding values for site colors
lakecolours <- c(
  `1` = "#F8766D",
  `2` = "#D89000",
  `3` = "#A3A500",
  `4` = "#39B600",
  `5` = "#00BF7D",
  `6` = "#00BFC4")

## converting site column to factor so I can use it in the graph
sites$site <- factor(sites$sites)

## selecting just the Lake points

lakesites <- sites %>%
  #dplyr::mutate(wbid = wbid(wbid)) %>%
  dplyr::filter(wbid == "Lake") %>%
  
 


## selcting parameters that I want out of already pivotted and set 
## up data set called sites

OMratio <- lakesites %>%
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
  




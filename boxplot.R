##Need to keep SITE name from STATS dataframe - altering code to do this

stats3 <- stats %>%
  mutate(DIN = NH4F + NO23F,
         TN = TKN + NO23F,
         DON = TKNF - NH4F,
         PN = TN - (DIN + DON),
         DINuM = NH4uM + NO23uM,
         TNuM = TKNuM + NO23uM,
         DONuM = TKNFuM - NH4uM,
         TNTP = TN/TP,
         PNuM = TNuM - (DINuM + DONuM))


## need to pivot data so values are in column not by row in order to graph in a boxplot
statslong <- stats3 %>%
pivot_longer(cols = 4:27,
             names_to = "nut", 
             values_to = "count")




# ---- 03 boxplots of all sites -----------------------------------
##Need to run new coloring script from 4_yr to relate to my current dataframe

### NEED TO RUN COLORING SCRIPT FROM 4yr TO GET TITLE CODE ###

sitecolours <- c(
  MICKLERS = "#F8766D",
  `DEPGL1` = "#D89000",
  `DEPGL2` = "#A3A500",
  `LAKE MIDDLE` = "#39B600",
  `DEPGL4` = "#00BF7D",
  `LAKE SOUTH` = "#00BFC4",
  `RIVER NORTH` = "#00B0F6",
  `DEPGR1` = "#9590FF",
  `GUANA RIVER` = "#E76BF3",
  `DEPGR3` = "#FF62BC"
)


boxplot_all_sites <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  
  p <- statslong %>%
    dplyr::filter(nut == param) %>%
    ggplot(aes(x = site, y = count, fill = site)) +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(size=12, color='black'),
          legend.position = "none") +
    scale_x_discrete(labels = c("Micklers",
                                "GL1",
                                "GL2",
                                "Lake\nMiddle",
                                "GL4",
                                "Lake\nSouth",
                                "River\nNorth",
                                "GR1",
                                "Guana\nRiver",
                                "GR3")) +
    labs(y = axis_title,
         x = "",
         title = paste(param))
  
  p
}


boxplot_all_sites("TN", nitro_y_title)
boxplot_all_sites("NO23F", nitro_y_title)
boxplot_all_sites("CHLA_C", chla_y_title)
boxplot_all_sites("TP", phos_y_title)
boxplot_all_sites("NH4F", "Ammonium (mg/L)")





#### Edited boxplot for redfield ratio, using limit for y axis and using threshold for WB 
### ratio of 7.23:1 N:P


boxplot_all_sites <- function(param, threshold, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  
  p <- statslong %>%
    dplyr::filter(nut == param) %>%
    ggplot(aes(x = site, y = count, fill = site)) +
    geom_boxplot(alpha = 0.8) +
    geom_hline(yintercept = threshold, linetype = 'longdash', color = 'gray18', size = 1.5) +
    scale_fill_manual(name = "Site", values = sitecolours) +
    coord_cartesian(ylim = c(0, 150)) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(size=12, color='black'),
          legend.position = "none") +
    scale_x_discrete(labels = c("Micklers",
                                "GL1",
                                "GL2",
                                "Lake\nMiddle",
                                "GL4",
                                "Lake\nSouth",
                                "River\nNorth",
                                "GR1",
                                "Guana\nRiver",
                                "GR3")) +
    labs(y = axis_title,
         x = "")
  
  p
}

#doestn work 
TNTP <- boxplot_all_sites("TNTP", "7.23", "Redfield Ratio (mg/L)")



# ---- 04 boxplots by waterbody -----------------------------------

boxplot_wbid <- function(param, axis_title) {
  # param - use component_short parameter name in quotes
  # axis_title - use axis title value from 00_vis_custom.R, no quotes
  
  p <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    dplyr::filter(wbid == "Lake") %>%
    ggplot(aes(x = site_friendly, y = result, fill = site_friendly)) +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(size=12, color='black'),
          legend.position = "none") +
    scale_x_discrete(labels = c("Micklers",
                                "GL1",
                                "GL2",
                                "Lake\nMiddle",
                                "GL4",
                                "Lake\nSouth",
                                "River\nNorth",
                                "GR1",
                                "Guana\nRiver",
                                "GR3")) +
    labs(y = axis_title,
         x = "",
         title = paste(param),
         subtitle = "Guana Lake Sites")
  
  q <- dat2 %>%
    dplyr::filter(component_short == param & end == "N") %>%
    dplyr::filter(wbid == "River") %>%
    ggplot(aes(x = site_friendly, y = result, fill = site_friendly)) +
    geom_boxplot(alpha = 0.8) +
    scale_fill_manual(name = "Site", values = sitecolours) +
    cowplot::theme_cowplot() +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.text.x = element_text(size=12, color='black'),
          legend.position = "none") +
    scale_x_discrete(labels = c("Micklers",
                                "GL1",
                                "GL2",
                                "Lake\nMiddle",
                                "GL4",
                                "Lake\nSouth",
                                "River\nNorth",
                                "GR1",
                                "Guana\nRiver",
                                "GR3")) +
    labs(y = axis_title,
         x = "",
         title = paste(param),
         subtitle = "Guana River Sites")
  
  p / q
  
}


# use the function to create box plots of whatever parameter you want, examples below
boxplot_wbid("CHLA_C", chla_y_title)


# or different parameters without designated title
boxplot_wbid("SALT", "Salinity (psu)")

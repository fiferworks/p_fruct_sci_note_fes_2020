#####LOADING PACKAGES####
pkgs <-
  c('tidyverse',
    'readxl',
    'ggthemes',
    'showtext',
    'extrafont',
    'Cairo',
    'writexl',
    'sf',
    'ggplot2',
    'leaflet',
    'dplyr',
    'mapview',
    'htmlwidgets',
    'viridisLite')

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packages, character.only = TRUE)

lapply(pkgs, library, character.only = T)


####GETTING REQUIRED FONTS####
#telling R the path where the fonts are located
font_paths('../fonts')

#imports the font Gill Sans MT as the font family 'gill_sans'
font_add(
  family = 'gill_sans',
  regular = "GIL_____.TTF",
  bold = "GILB____.TTF",
  italic = "GILBI___.TTF",
  bolditalic = "GILI____.TTF"
)

#imports the font Garamond MT as the font family 'garamond'
font_add(
  family = 'garamond',
  regular = "AGaramondPro-Regular.otf",
  bold = "AGaramondPro-Bold.otf",
  italic = "AGaramondPro-BoldItalic.otf",
  bolditalic = "AGaramondPro-Italic.otf"
)

# #uncommment and run to make sure it worked
# #it should list "gill_sans" and "garamond"
# font_families()

showtext_auto()


####READING IN THE REQUIRED FILES####
df <- read_xlsx('all_mites_survey.xlsx')

#making sure certain columns are considered as factors
df$p_fructiphilus <- as.factor(df$p_fructiphilus)
df$month <- as.factor(df$month)

#focusing on mites in Florida
#filters out 'NAs'
df_fl <- filter(df, df$eriophyoids >= '0')

#filters out eriophyoids which aren't P. fructiphilus
df_fl <- filter(df_fl, df_fl$p_fructiphilus == 'Yes')

#filters data to only show Florida sites
df_fl <- filter(df_fl, df_fl$state == 'FL')

#filters data to only show Tallahassee
df_fl <- filter(df_fl, df_fl$county == 'Leon')

####DROPS A SPECIFIC SITE WHICH MIGHT BE AN OUTLIER####
####DOUBLE CHECK SITE BEFORE USING FOR ANALYSIS####
df_fl <- filter(df_fl, df_fl$id != 'James 114')

####SUMMARY STATS####
df_fl <- df_fl %>% group_by(month) %>% mutate(
  totals = sum(eriophyoids),
  per_plant = mean(eriophyoids),
  sd = sd(eriophyoids),
  se = sd(eriophyoids) / sqrt(n()),
  totals = log(mean(eriophyoids)),
  se = log(sd(eriophyoids) / sqrt(n())),
) %>%
  ungroup()

#makes a list of totals for each month to display on the graphs
surveys <- df_fl %>% group_by(month) %>% summarize(
  totals = sum(eriophyoids),
  per_plant = mean(eriophyoids),
  sd = sd(eriophyoids),
  se = sd(eriophyoids) / sqrt(n()),
  totals = log(mean(eriophyoids)),
  se = log(sd(eriophyoids) / sqrt(n())),
  n_samples = n()
) %>%
  ungroup()

surveys$per_plant <- round(surveys$per_plant, digits = 1)

#renaming columns for nicer graph titles on axes
surveys <-
  rename(surveys, Month = month, 'Mites per Plant' = per_plant)
df_fl <- rename(df_fl, Month = month, 'Mites per Plant' = per_plant)

#renaming months for better labels on graph
surveys$Month <- gsub('Feb', 'February', surveys$Month)
surveys$Month <- gsub('Jul', 'July', surveys$Month)
df_fl$Month <- gsub('Feb', 'February', df_fl$Month)
df_fl$Month <- gsub('Jul', 'July', df_fl$Month)

####GRAPHS####
#####graphs of the different tests####
#plots the data
ggplot(data = surveys,
       mapping = aes(x = Month, y = `Mites per Plant`, fill = Month)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(
    aes(ymin = `Mites per Plant` - se, ymax = `Mites per Plant` + se),
    width = 0.5,
    size = 2.5,
    position = position_dodge(.9)
  ) +
  coord_cartesian(ylim = c(-0.9, 210)) +
  geom_text(
    aes(Month, `Mites per Plant`, label = `Mites per Plant`, fill = NULL),
    size = 40,
    nudge_y = 50
  ) +
  geom_text(
    stat = "count",
    aes(label = paste0("n = ", ..count..), y = ..count..),
    size = 40,
    nudge_y = 30,
    data = df_fl
  ) +
  theme_tufte(base_size = 100, base_family = "gill_sans") +
  # ggtitle((expression(
  #   paste(
  #     "The Number of ",
  #     italic("P. fructiphilus"),
  #     " Found on Roses in Northern Florida"
  #   )
  # ))) +
  theme(legend.position = "none") +
  theme(
    plot.title = element_text(
      size = 100,
      face = "bold",
      hjust = 0.1,
      color = "grey20",
      family = "garamond"
    ),
    axis.text.x = element_text(
      color = "grey20",
      size = 140,
      angle = 0,
      hjust = .5,
      vjust = .5,
      face = "plain"
    ),
    axis.text.y = element_text(
      color = "grey20",
      size = 80,
      angle = 0,
      hjust = 1,
      vjust = 0,
      face = "bold"
    )
  ) +
  annotate(
    geom = "text",
    size = 60,
    x = 2,
    y = 200,
    label = "***",
    color = "black"
  ) +
  # annotate(
  #   geom = "text",
  #   size = 35,
  #   x = 2,
  #   y = 200,
  #   label = "p-value = 0.001",
  #   color = "black"
  # ) +
  scale_fill_manual(values = c("gray", "gray50")) +
  
  #saving the file
  ggsave(
    '../images/survey_graph_pub.png',
    plot = last_plot(),
    type = "cairo",
    width = 16,
    height = 9,
    scale = 1,
    dpi = 300
  )


####MAPS####
df_map <- read_xlsx('all_mites_survey.xlsx')

#making sure certain columns are considered as factors
df_map$p_fructiphilus <- as.factor(df_map$p_fructiphilus)
df_map$month <- as.factor(df_map$month)

#focusing on mites in Florida
#filters out 'NAs'
df_map <- filter(df_map, df_map$eriophyoids >= '0')

#filters data to only show Florida sites
df_map <- filter(df_map, df_map$state == 'FL')

#filters data to only show Tallahassee
df_map <- filter(df_map, df_map$county == 'Leon')

####DROPS A SPECIFIC SITE WHICH MIGHT BE AN OUTLIER####
####DOUBLE CHECK SITE BEFORE USING FOR ANALYSIS####
df_map <- filter(df_map, df_map$id != 'James 114')

# #correct year
# df_map <- filter(df_map, df_map$year == '2019')
# 
# #focuses on the months when we found P. fructiphilus
# df_map <- filter(df_map, df_map$month == 'Feb' | df_map$month == 'Jul')

####GRAPHING ALL SAMPLING LOCATIONS####
#makes the dataframe into a spatial dataframe
mites <-
  st_as_sf(df_map,
           coords = c("longitude", "latitude"),
           crs = 4326)

mites_map <-
  mapview(
    mites,
    col.regions = c("#FFFFFF", "#E28F41"),
    alpha.regions = 2,
    zcol = 'p_fructiphilus',
    layer.name = 'P. fructiphilus Present?',
    map.types = 'Stamen.TonerLite'
  )

#saves the map
mapshot(mites_map, file = '../images/p_fruct_map_all_FES_Fife_et_al_2020.png')

####GRAPHING P. FRUCTIPHILUS LOCATIONS IN FLORIDA####
#makes the dataframe into a spatial dataframe
emites <-
  st_as_sf(df_map,
           coords = c("longitude", "latitude"),
           crs = 4326)

####COMPARING POPULATIONS OVER TIME#####
#subsetting the data for different months to compare trends
e_feb <- filter(emites, month == 'Feb')
e_jul <- filter(emites, month == 'Jul')

#map for february
m_feb <-
  mapview(
    e_feb,
    col.regions = c("#DEDEDE", "#3F3F3F"),
    alpha.regions = 1,
    zcol = 'p_fructiphilus',
    layer.name = 'February',
    map.types = 'CartoDB.Positron'
  )

mapshot(m_feb, file = '../images/feb_p_fruct_map_fl_FES_Fife_et_al_2020.png')

#map for july
m_jul <- mapview(
  e_jul,
  col.regions = c("#DEDEDE", "#3F3F3F"),
  alpha.regions = 1,
  zcol = 'p_fructiphilus',
  layer.name = 'July',
  map.types = 'CartoDB.Positron'
)

mapshot(m_jul, file = '../images/jul_p_fruct_map_fl_FES_Fife_et_al_2020.png')

#a way to look at both maps at the same time
leafsync::sync(m_feb, m_jul, ncol = 2, no.initial.sync = F)
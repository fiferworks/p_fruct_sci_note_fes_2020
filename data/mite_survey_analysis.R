####PACKAGES####
pkgs <- c('dplyr', 'lubridate', 'readxl')
#installs missing packages
nu_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(nu_pkgs))
  install.packages(nu_pkgs)

#loading required packages
lapply(pkgs, library, character.only = TRUE)
rm(pkgs, nu_pkgs)

####MITE SURVEY ANALYSIS####

####READING IN THE REQUIRED FILES####
df <- read_xlsx('rrv_all_mites_survey.xlsx')

df$month 

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

####DROPS A SPECIFIC SITE WHERE MITES COULD NOT BE VERIFIED####
df_fl <- filter(df_fl, df_fl$id != 'James 114')

#filtering out the original sites
feb_sites <- filter(df_fl, df_fl$month == 'Feb')

#and the second survey
j_1 <- filter(df_fl, df_fl$id == 'FSU 4')
j_2 <- filter(df_fl, df_fl$id == 'FSU 14')
j_3 <- filter(df_fl, df_fl$id == 'FSU 15')
j_4 <- filter(df_fl, df_fl$id == 'FSU 33')
j_5 <- filter(df_fl, df_fl$id == 'FSU 17')

#combining july sites
jul_sites <- bind_rows(j_1, j_2, j_3, j_4, j_5)

#t-test
p <- t.test(feb_sites$eriophyoids, jul_sites$eriophyoids, paired = T)
print(p)

#cleanup
rm(list = ls())
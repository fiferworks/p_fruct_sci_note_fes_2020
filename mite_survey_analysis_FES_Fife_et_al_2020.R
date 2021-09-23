####PACKAGES####
pkgs <- c('tidyverse', 'lme4', 'car', 'multcomp', 'readxl')
lapply(pkgs, library, character.only = T)
rm(pkgs)

# #installs the packages if you don't have them already installed
# lapply(pkgs, install.packa

####MITE SURVEY ANALYSIS####

####READING IN THE REQUIRED FILES####
df <- read_xlsx('all_mites_survey.xlsx')

#making sure certain columns are considered as factors
df$p_fructiphilus <- as.factor(df$p_fructiphilus)
df$month <- as.factor(df$month)
df$collector <- as.factor(df$collector)

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

#filtered by year
sites <- filter(df_fl, df_fl$year == 2019)

#filtering out the original sites
feb_sites <- filter(df_fl, df_fl$month == 'Feb')

#and the second survey
j_1 <- filter(sites, sites$id == 'FSU 4')
j_2 <- filter(sites, sites$id == 'FSU 14')
j_3 <- filter(sites, sites$id == 'FSU 15')
j_4 <- filter(sites, sites$id == 'FSU 33')
j_5 <- filter(sites, sites$id == 'FSU 17')

#combining july sites
jul_sites <- bind_rows(j_1, j_2, j_3, j_4, j_5)

#both sites combined
feb_jul <- bind_rows(feb_sites, jul_sites)

#t-testing
t.test(feb_sites$eriophyoids, jul_sites$eriophyoids, paired = T)

#the following tests are just a lark, so future editing will be easier
#anova
glm_1 <-
  glmer(eriophyoids ~ month + (1 |
                                 id), family = 'poisson', data = df_fl)

#ANOVA
Anova(glm_1)

#multiple comparisions
#suggests that both Actigard and Kontos treatments are significantly different than the water treatment
summary(glht(glm_1, linfct = mcp(month = "Tukey")), test = adjusted("holm"))

#making a compact letter display for each treatment
q <- glht(glm_1, linfct = mcp(month = "Tukey"))

#cleanup
rm(list = ls())
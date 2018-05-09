# clean working directory
#rm(list = ls(all = TRUE))

# Set working directory
setwd("~/Desktop/Final paper")

# Load libraries
library(tidyverse)
library(lme4)
library(lmerTest)

# Load and recode data
ajt_df1 <- read_csv("./data/ajt_raw.csv") %>% 
  na.omit(.) %>% 
  mutate(., groupCon = recode(group, co = 0.5, hs = -0.5),
         gramCon = recode(gram, g = 0.5, u = -0.5))

ajt_df2 <- read_csv("./data/ajt_additional_data.csv") %>% 
  na.omit(.) %>% 
  mutate(., groupCon = recode(group, co = 0.5, hs = -0.5),
         gramCon = recode(gram, g = 0.5, u = -0.5))
# if I change frecuency to a continuous variable, do i still need to recode frequency to 
# 0.5 vs -0.5?

ajt_df_temp <- rbind(ajt_df1, ajt_df2)

# Create vector of verb names
verbs <- c('fg1' = 'cuidar', 
           'fg2' = 'encontrar', 
           'fg3' = 'tocar', 
           'fg4' = 'aceptar', 
           'fg5' = 'medir', 
           'fg6' = 'parar', 
           'fg7' = 'llevar', 
           'fg8' = 'cambiar',
           'fu1' = 'cuidar', 
           'fu2' = 'encontrar', 
           'fu3' = 'tocar', 
           'fu4' = 'aceptar', 
           'fu5' = 'medir', 
           'fu6' = 'parar', 
           'fu7' = 'llevar', 
           'fu8' = 'cambiar',
           'ug1' = 'vigilar',
           'ug2' = 'hallar', 
           'ug3' = 'acariciar', 
           'ug4' = 'acoger', 
           'ug5' = 'abrigar', 
           'ug6' = 'detener', 
           'ug7' = 'trasladar', 
           'ug8' = 'reemplazar', 
           'uu1' = 'vigilar',
           'uu2' = 'hallar', 
           'uu3' = 'acariciar', 
           'uu4' = 'acoger', 
           'uu5' = 'abrigar', 
           'uu6' = 'detener', 
           'uu7' = 'trasladar', 
           'uu8' = 'reemplazar')

# append column to data frame of verb names
ajt_df_temp$verbs <- verbs[ajt_df_temp$item]

#create a columns with the raw frequency data
# do I need to include the ungrammatical ones also?
countfreq <- c('fg1' = 7531,
               'fg2' = 21725 , 
               'fg3' = 3861  , 
               'fg4' = 4098  , 
               'fg5' = 886, 
               'fg6' = 8174  , 
               'fg7' = 107445 , 
               'fg8' = 9871,
               'fu1' = 7531,
               'fu2' = 21725 , 
               'fu3' = 3861  , 
               'fu4' = 4098  , 
               'fu5' = 886, 
               'fu6' = 8174  , 
               'fu7' = 107445 , 
               'fu8' = 9871,
               'ug1' = 1197 ,
               'ug2' = 803, 
               'ug3' = 427 , 
               'ug4' = 2423, 
               'ug5' = 123 , 
               'ug6' = 5912, 
               'ug7' = 4402, 
               'ug8' = 4252,
               'uu1' = 1197 ,
               'uu2' = 803, 
               'uu3' = 427 , 
               'uu4' = 2423, 
               'uu5' = 123 , 
               'uu6' = 5912, 
               'uu7' = 4402, 
               'uu8' = 4252)
ajt_df_temp$countfreq <- countfreq[ajt_df_temp$item]


# import dele data and join with df
ajt_df <- read_csv("./data/prof_df.csv") %>% 
  left_join(ajt_df_temp, ., by = 'participant')



# !!!! NO MORE OPENING THE FOLLOWING FILES:
# - ajt_comp_clean.csv
# - ajt_raw.csv
# - ept_comp_clean.csv
# - ept_raw.csv

# proficiency
# - create data frame (excel) with participant ID and prof label and dele (participant, prof, dele)
# - save as 'prof_df.csv' in data folder

# frequency
# - create data frame (excel) with verb (infinitive) and frequency (davis score)
# - 2 columns, verb and freq
# - save as 'freq_df.csv' in data folder

# additional participants
# - create new data frame for additional participants
# - same columns as 'ajt_raw.csv' and 'ept_raw.csv'
# - save as 'ajt_additional_data.csv' in data folder
# - save as 'ept_additional_data.csv' in data folder







# check it 

as.data.frame(ajt_df[, c('participant', 'group', 'proficiency')])


#~~~~~~~#
# Means #
#~~~~~~~#

# response as a function of group, freq, and gram
ajt_df %>% 
  group_by(group, proficiency, countfreq, gram) %>% 
  summarize(., mean = mean(response), 
            sd = sd(response))

# group proficiency frequency  gram     mean       sd
#    co          co         f     g 4.266667 1.214294
#    co          co         f     u 2.066667 1.142997
#    co          co         u     g 4.358333 1.121193
#    co          co         u     u 2.191667 1.245468
#    hs      hs_adv         f     g 3.837500 1.288007
#    hs      hs_adv         f     u 3.412500 1.225451
#    hs      hs_adv         u     g 3.812500 1.264849
#    hs      hs_adv         u     u 3.456250 1.217518
#    hs      hs_int         f     g 3.462500 1.449083
#    hs      hs_int         f     u 3.750000 1.307234
#    hs      hs_int         u     g 3.625000 1.390519
#    hs      hs_int         u     u 3.462500 1.422636

# Response as a function of group and gram
ajt_df %>% 
  group_by(group, proficiency, gram) %>% 
  summarize(., mean = mean(response), 
            sd = sd(response))

# group proficiency  gram     mean       sd
#    co          co     g 4.312500 1.167127
#    co          co     u 2.129167 1.194471
#    hs      hs_adv     g 3.825000 1.274540
#    hs      hs_adv     u 3.434375 1.219772
#    hs      hs_int     g 3.543750 1.417975
#    hs      hs_int     u 3.606250 1.369464

# Named vector of condition labels
group_names <- c(co = "CO", hs_adv = "HS Adv.", hs_int = "HS Int.")

# judgement score as a function of group, gram and freq
ajt_elog <- ajt_df %>% 
  group_by(., participant, proficiency, countfreq, groupCon, response, item, verbs, gramCon, gram) %>% 
  summarize(., n = 8, 
            wDOM = sum(response), 
            woDOM = n - wDOM, 
            eLog = log((wDOM + 0.5) / (n - wDOM + 0.5)), 
            wts = 1 / (wDOM + 0.5) + 1 / (n - wDOM + 0.5),
            countLog = log(countfreq))

ajt_elog2 <- ajt_elog %>%
  filter(., item != "fg2") %>%
  filter(., item != "fg7") %>%
  filter(., item != "fu2") %>%
  filter(., item != "fu7") %>%
  filter(., item != "ug3") %>%
  filter(., item != "ug5") %>%
  filter(., item != "uu3") %>%
  filter(., item != "uu5") 


ajt_p1 <- ajt_elog2 %>% 
  ggplot(., aes(x = countLog, y = eLog, color = proficiency)) + 
  geom_point()+
  geom_smooth(method =lm)+
  scale_color_brewer(palette = "Set1")

ajt_p1

# ggsave(filename = 'ajt_p1.png', plot = ajt_p1, path = "./figs", 
#        width = 7.5, height = 3.5, units = "in", dpi = 300)


# Get order from most to least frequent and store in vector
#order <- ajt_elog2%>% 
#  group_by(., verbs, countLog, proficiency) %>% 
#  summarize(., prop = mean(response)) %>% 
#  arrange(proficiency, countLog, desc(prop)) %>% 
#  as.data.frame %>% 
#  slice(1:16) %>% 
#  pull(verbs)

#order

# Relevel verbs using frequency order
ajt_df$verbs <- factor(ajt_elog2$verbs, levels = order)

condition_names <- c('co' = "CO", 'hs_adv' = "HS Adv.", 'hs_int' = "HS Int.", 
                     "g" = "Grammatical", "u" = "Ungrammatical")

# plot response as a function of verbs, gram and freq for each group
ajt_p2 <- ajt_elog2 %>% 
  # filter(., !(participant %in% c("p03", "p13"))) %>%
  ggplot(., aes(x = verbs, y = eLog, color = proficiency, dodge = proficiency)) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', position = position_dodge(1), size = 0.25) + 
  stat_summary(fun.y = mean, geom = 'point', position = position_dodge(1), color = 'white', size = 1.25) + 
  scale_color_brewer(palette = "Set1") + 
  labs(y = 'Mean judgement score', x = 'Verb', caption = "Mean +/- 95% CI") + 
  theme_grey(base_family = 'Times', base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ajt_p2

# ggsave(filename = 'ajt_p2.png', plot = ajt_p2, path = "./figs", 
# width = 7.5, height = 3.5, units = "in", dpi = 300)


#ajt_p3 <- ajt_elog2 %>% 
#ggplot(., aes(x = gram, y = eLog, color = proficiency, dodge = proficiency)) + 
#  scale_color_brewer(palette = "Set1", name = "", labels = c("CO", 'HS Adv.', 'HS Int.')) + 
#  scale_x_discrete(labels = c("Grammatical", "Ungrammatical")) + 
#  ylim(1, 5) + 
#  labs(y = 'Judgement score', x = 'Group', caption = "Mean +/- 95% CI") + 
#  theme_grey(base_family = 'Times', base_size = 12)

#ajt_p3

# ggsave(filename = 'ajt_p3.png', plot = ajt_p3, path = "./figs", 
# width = 5.5, height = 3.5, units = "in", dpi = 300)



lmer_null <- lmer(response ~ 1 + 
                    (1 + gramCon + countLog | participant) + 
                    (0 + gramCon + countLog | verbs), 
                  data = ajt_elog2, REML = FALSE)

lmer_prof <- lmer(response ~ proficiency + 
                    (1 + gramCon + countLog | participant) + 
                    (0 + gramCon + countLog | verbs), 
                  data = ajt_elog2, REML = FALSE)

anova(lmer_null, lmer_prof, test = 'Chisq') # no main effect of proficiency

lmer_gram <- lmer(response ~ proficiency + gramCon +
                    (1 + gramCon + countLog | participant) + 
                    (0 + gramCon + countLog | verbs), 
                  data = ajt_elog2, REML = FALSE)

anova(lmer_prof, lmer_gram, test = 'Chisq') # main effect of grammaticality

# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# object 13 3403.9 3468.7 -1689.0   3377.9                             
#..1    14 3394.4 3464.2 -1683.2   3366.4 11.546      1   0.000679 ***

lmer_freq <- lmer(response ~ proficiency + gramCon + countLog + 
                    (1 + gramCon + countLog | participant) + 
                    (0 + gramCon + countLog | verbs), 
                  data = ajt_elog2, REML = FALSE)

anova(lmer_gram, lmer_freq, test = 'Chisq') #  no main effect of frequency

#  Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)
#object 14 3394.4 3464.2 -1683.2   3366.4                         
#..1    15 3395.6 3470.3 -1682.8   3365.6 0.8219      1     0.3646

lmer_prof_gram <- lmer(response ~ proficiency + gramCon + countLog + 
                         proficiency:gramCon + 
                         (1 + gramCon + countLog | participant) + 
                         (0 + gramCon + countLog | verbs), 
                       data = ajt_elog2, REML = FALSE)

anova(lmer_freq, lmer_prof_gram, test = 'Chisq') # proficiency by gram interaction

lmer_prof_freq <- lmer(response ~ proficiency + gramCon + countLog + 
                         proficiency:gramCon + proficiency:countLog + 
                         (1 + gramCon + countLog | participant) + 
                         (0 + gramCon + countLog | verbs), 
                       data = ajt_elog2, REML = FALSE)

anova(lmer_prof_gram, lmer_prof_freq, test = 'Chisq') # no proficiency by freq interaction


lmer_full <- lmer(response ~ proficiency * gramCon * countLog + 
                    (1 + gramCon + countLog | participant) + 
                    (0 + gramCon + countLog | verbs), 
                  data = ajt_elog2, REML = FALSE)

anova(lmer_prof_freq, lmer_full, test = 'Chisq') # three way interaction

#Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)  
#object 19 3366.6 3461.3 -1664.3   3328.6                           
#..1    22 3362.8 3472.5 -1659.4   3318.8 9.7736      3    0.02059 *

summary(lmer_prof_gram)

#AIC      BIC   logLik deviance df.resid 
#33360.2   3445.0  -1663.1   3326.2     1063 

#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.1519 -0.5279  0.1362  0.6479  2.4952 

#Random effects:
#  Groups      Name        Variance Std.Dev. Corr       
#participant (Intercept) 1.135343 1.06552             
#gramCon     0.511782 0.71539  -0.27      
#countLog    0.013061 0.11429  -0.94  0.19
#verbs       gramCon     0.173885 0.41699             
#countLog    0.001804 0.04247  0.09       
#Residual                1.083406 1.04087             
#Number of obs: 1080, groups:  participant, 45; verbs, 12

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#(Intercept)                 2.1910     0.9757 12.7200   2.246  0.04317 *  
#  proficiencyhs_adv           0.4098     0.1477 44.5100   2.773  0.00807 ** 
#  proficiencyhs_int           0.3836     0.1766 44.5100   2.172  0.03520 *  
#  gramCon                     2.1993     0.2460 47.5100   8.939 9.54e-12 ***
#  countLog                    0.1295     0.1209 12.2200   1.071  0.30472    
#proficiencyhs_adv:gramCon  -1.8674     0.2836 44.7800  -6.584 4.32e-08 ***
#  proficiencyhs_int:gramCon  -2.3893     0.3390 44.7700  -7.048 8.87e-09 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Correlation of Fixed Effects:
#  (Intr) prfcncyhs_d prfcncyhs_n gramCn contLg prfcncyhs_d:C
#prfcncyhs_d   -0.087                                                    
#prfcncyhs_n   -0.072  0.478                                             
#gramCon       -0.033  0.144       0.121                                 
#countLog      -0.988  0.000       0.000       0.016                     
#prfcncyhs_d:C  0.019 -0.219      -0.105      -0.659  0.000              
#prfcncyhs_n:C  0.016 -0.105      -0.219      -0.551  0.000  0.478   

MuMIn::r.squaredGLMM(lmer_full)



###############
# big picture #
###############

# Main effects
# Proficiency    - no main effect: X2(2) = 1.32, p > 0.05
# Grammaticality - main effect: X2(1) = 14.189, p < 0.001
# Frequency      - main effect: X2(1) = 17.809, p < 0.001

# Two-way interactions
# Proficiency x Grammaticality - Yes!: X2(2) = 37.611, p < 0.001
# Proficiency x Frequency      - No: X2(2) = 0.1918, p > 0.05

# Three-way interaction
# Proficiency x Grammaticality x Frequency - NO!: X2(3) = 5.8834, p > 0.05

# So... we will examine futher the proficiency x grammaticality interaction
# We are interested in group differences, so we will split grammaticality

ajt_df_grammatical   <- filter(ajt_elog, gram == 'g')
ajt_df_ungrammatical <- filter(ajt_elog, gram == 'u')

# refit models

lmer_gram <- lmer(response ~ proficiency + 
                    (1|participant) + 
                    (1|verbs), REML = F,
                  data = ajt_df_grammatical)
summary(lmer_gram)

#                   Estimate Std. Error      df t value Pr(>|t|)    
#  (Intercept)         4.3481     0.1530 46.3200  28.418  < 2e-16 ***
#  proficiencyhs_adv  -0.4874     0.1789 44.8100  -2.724 0.009169 ** 
#  proficiencyhs_int  -0.7687     0.2139 44.8000  -3.594 0.000807 ***                    


ajt_df_grammatical$proficiency <- factor(ajt_df_grammatical$proficiency, levels = c('hs_adv', 'co', 'hs_int'))
lmer(response ~ proficiency + (1|participant) + (1|verbs), REML = F, 
     data = ajt_df_grammatical) %>% summary

#Fixed effects:
#Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)         3.8607     0.1375 41.5900  28.074  < 2e-16 ***
#  proficiencyco       0.4874     0.1789 44.8100   2.724  0.00917 ** 
#  proficiencyhs_int  -0.2813     0.2029 44.7800  -1.386  0.17251   

# For grammatical items... 
# - CO has highest judegement scores
#    - they are significantly higher than HS Adv and HS int
#    - CO vs. HS Adv: B = -0.48, SE = 0.17, t = -2.72, p < 0.01
#    - CO vs. HS Int: B = -0.77, SE = 0.21, t = -3.58, p < 0.001
# - HS Adv and HS Int are not different from each other 
#    - HS Adv vs. HS Int: (B = -0.28, SE = 0.20, t = -1.38, p > 0.05)




lmer_ugram <- lmer(response ~ proficiency + 
                     (1|participant) + 
                     (1|verbs), , REML = F,
                   data = ajt_df_ungrammatical)
summary(lmer_ugram)

#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)    
#  (Intercept)         2.0786     0.1886 51.0300  11.023 4.22e-15 ***
#  proficiencyhs_adv   1.3085     0.2229 44.8300   5.870 4.93e-07 ***
#  proficiencyhs_int   1.4804     0.2664 44.8100   5.557 1.43e-06 ***

ajt_df_ungrammatical$proficiency <- factor(ajt_df_ungrammatical$proficiency, levels = c('hs_adv', 'co', 'hs_int'))
lmer(response ~ proficiency + (1|participant) + (1|verbs), REML = F, 
     data = ajt_df_ungrammatical) %>% summary

#Fixed effects:
#                   Estimate Std. Error      df t value Pr(>|t|)    
#  (Intercept)         3.3871     0.1695 47.7300   19.99  < 2e-16 ***
#  proficiencyco      -1.3085     0.2229 44.8300   -5.87 4.93e-07 ***
#  proficiencyhs_int   0.1719     0.2527 44.7700    0.68      0.5   

# For ungrammatical items... 
# - CO has lowest judegement scores
#    - they are significantly lower than HS Adv and HS int
#    - CO vs. HS Adv: B = 1.30, SE = 0.22, t = 5.87, p < 0.001
#    - CO vs. HS Int: B = 1.48, SE = 0.26, t = 5.55, p < 0.001
# - HS Adv and HS Int are not different from each other 
#    - HS Adv vs. HS Int: (B = 0.17, SE = 0.25, t = 0.68, p > 0.05)


# What about HS (only) for grammatical vs. ungrammatical items?
ajt_df_only_hs  <- filter(ajt_elog, proficiency != 'co') %>% 
  mutate(., profCon = if_else(proficiency == 'hs_adv', true = 0.5, false = -0.5))

# 2 x 2 test
lmer(response ~ profCon * gramCon + (1+ gramCon | participant) + (1 + gramCon|verbs), REML = F, 
     data = ajt_df_only_hs) %>% summary
# sig. interaction of prof x gram, so we do individual tests

lmer(response ~ gramCon + (1 + gramCon | participant) + (1 + gramCon|verbs), 
     data = ajt_df_only_hs[ajt_df_only_hs$proficiency == 'hs_adv', ]) %>% summary
# For HS adv grammatical items get slightly higher scores
# B = 0.39, SE = 0.20, t = 1.94, p < 0.01

lmer(response ~ gramCon + (1 + gramCon | participant) + (1 + gramCon|verbs), 
     data = ajt_df_only_hs[ajt_df_only_hs$proficiency == 'hs_int', ]) %>% summary
# For HS int grammatical and ungrammatical items do not get different scores
# B = -0.04, SE = 0.24, t = -0.16, p > 0.05



# Summarize (take home message)
# Native speakers have accurate intuitions about grammaticality 
# (grammatical item have higher scores than ungrammatical items).
# Heritage speakers acceptability scores differ from controls for 
# grammatical and ungrammatical items. For Advanced HS acceptability 
# does vary as a function of grammaticality (grammatical item get 
# slighly higher scores). This is not true for intermediate HS 
# (grammatical and ungrammatical items get similar scores).










# Compare AJT and EPT
# - we will calculate an AJT score for gramatical items 
#   and frequent/unfrequent conditions for each participant in 
#   each group
# - This will collapse over item
# - each participant with have 4 scores

#ajt_df %>% 
#  group_by(., participant, proficiency, frequency, gram) %>% 
#  summarize(., avg_ajt = mean(response)) %>% 
#  filter(., gram == 'g') %>% 
#  write_csv(., "./data/ajt_comp_clean.csv")
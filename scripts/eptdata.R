# clean working directory
#rm(list = ls(all = TRUE))

# Set working directory
# setwd("~/Desktop/Final paper")

#load package
library(here)
library(tidyverse)
library(lme4)
library(lmerTest)

#load data
ept_df1 <- read_csv(here("data", "ept_raw.csv")) %>% 
  mutate(., groupCon = recode(group, co = 0.5, hs = -0.5), #porque 0.5?
         freqCon = recode(frequency, f = 0.5, u = -0.5))

ept_df2 <- read_csv(here("data", "ept_additional_data.csv")) %>% 
    mutate(., groupCon = recode(group, co = 0.5, hs = -0.5),
         freqCon = recode(frequency, f = 0.5, u = -0.5))


# incorporate the df with the additional data

ept_df_temp <- rbind(ept_df1, ept_df2)


verbs <- c('f1' = 'cuidar', 
           'f2' = 'encontrar', 
           'f3' = 'tocar', 
           'f4' = 'aceptar', 
           'f5' = 'medir', 
           'f6' = 'parar', 
           'f7' = 'llevar', 
           'f8' = 'cambiar',
           'u1' = 'vigilar',
           'u2' = 'hallar', 
           'u3' = 'acariciar', 
           'u4' = 'acoger', 
           'u5' = 'abrigar', 
           'u6' = 'detener', 
           'u7' = 'trasladar', 
           'u8' = 'reemplazar')

ept_df_temp$verbs <- verbs[ept_df_temp$item]


unique(ept_df_temp$participant)





# import dele data and join with df
ept_df <- read_csv(here("data", "prof_df.csv")) %>% 
  left_join(ept_df_temp, ., by = 'participant')

# the new frequency counts from the Davis corpus
countfreq <- c('f1' = 7531,
               'f2' = 21725 , 
               'f3' = 3861  , 
               'f4' = 4098  , 
               'f5' = 886, 
               'f6' = 8174  , 
               'f7' = 107445 , 
               'f8' = 9871,
               'u1' = 1197 ,
               'u2' = 803, 
               'u3' = 427 , 
               'u4' = 2423, 
               'u5' = 123 , 
               'u6' = 5912, 
               'u7' = 4402, 
               'u8' = 4252)
ept_df$countfreq <- countfreq[ept_df$item]


# added a new column in order to plot per items as a synonim pair.
#ept_df %>%
#  ggplot(., aes(x = response, y = countfreq)) +
#           geom_boxplot()
         
         
#unique(ept_df_temp$participant)
         
#glimpse(ept_df)
         
#         synonym <- c('f1' = 1 , 
#                      'f2' = 2 , 
#                      'f3' = 3 , 
#                      'f4' = 4 , 
#                      'f5' = 5 , 
#                      'f6' = 6 , 
#                      'f7' = 7 , 
#                      'f8' = 8 ,
#                      'u1' = 1 ,
#                      'u2' = 2 , 
#                      'u3' = 3 , 
#                      'u4' = 4 , 
#                      'u5' = 5 , 
#                      'u6' = 6 , 
#                      'u7' = 7 , 
#                      'u8' = 8)
#         ept_df$synonym <- synonym[ept_df$item]
         
         
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
         
         
#         ept_df %>%
#           group_by(., proficiency) %>%
#           summarize(.,mean_response = mean(response), sd_score = sd(response),
#                     mean_countfreq =mean(countfreq), sd_exp = sd(countfreq)) 

## A tibble: 3 x 5
#proficiency mean_response sd_score mean_countfreq sd_exp
#   <chr>               <dbl>    <dbl>          <dbl>  <dbl>
# 1 co                  0.996   0.0645         11446. 25369.
# 2 hs_adv              0.441   0.497          11446. 25356.
# 3 hs_int              0.144   0.352          11446. 25395.

#ept_df %>%
#  ggplot(., aes(x=log(countfreq), y=response, color = proficiency)) +
#  geom_jitter(height = 0.02, width = 0.4) + 
#  geom_smooth(method = lm, se = F) + 
#  facet_grid(. ~ as.factor(synonym))

ept_df %>%
  ggplot(., aes(x = factor(synonym), y = response, color = proficiency, dodge = proficiency)) +
   stat_summary(fun.data = mean_se, geom = 'pointrange', position = position_dodge(0.5))

ept_elog <- ept_df %>% 
  group_by(., participant, proficiency, countfreq, groupCon, response, item) %>% 
  summarize(., n = 8, 
            wDOM = sum(response), 
            woDOM = n - wDOM, 
            eLog = log((wDOM + 0.5) / (n - wDOM + 0.5)), 
            wts = 1 / (wDOM + 0.5) + 1 / (n - wDOM + 0.5),
            countLog = log(countfreq))


ept_elog %>% 
  ggplot(., aes(x = countLog, y = response, color = proficiency)) +
  geom_jitter(height = 0.04, width = 0.2, alpha = 0.5) +
  geom_smooth(method = lm, fullrange = T) +
  scale_color_brewer(palette = "Set1") + 
  theme_bw()



ept_elog2 <- ept_elog %>%
  filter(., !(item %in% c("f2", 'f7', 'u3' ,'u5'))) 



ept_fig1 <- ept_elog2 %>%
  ggplot(., aes(x = countLog, y = response, color = proficiency)) +
    geom_jitter(height = 0.04, width = 0.2, alpha = 0.5) +
    geom_smooth(method = lm, fullrange = T) +
    scale_color_brewer(palette = "Set1") 







#           filter(., countfreq!= "f2") %>%
#             filter(., countfreq!= "f7") %>%
#             filter(., countfreq!= "f8") %>%
#             
#         ept_elog %>%
#           filter(., countfreq!= "f2") %>%
#           filter(., countfreq!= "f7") %>%
#           ggplot(., aes(x=countLog, y=response, color = proficiency)) +
#           geom_hline(yintercept = 0.60205, lty = 2, size = 0.25, color = "grey60") + # from fitted model below
#           geom_vline(xintercept = 0, lty = 2, size = 0.25, color = "grey60") +       # from fitted model below
#           geom_point() + 
#           geom_smooth(method = lm, se = F, fullrange = T) + 
#           geom_abline(intercept = 0.60205, slope = 0.67977, lty = 3) 

         
         # the plot per items
#         ept_df %>%
#           ggplot(., aes(x=countfreq, y=response, color = proficiency)) +
#           geom_abline(intercept = 0.60205, slope = 0.67977, lty = 3) +
#           geom_hline(yintercept = 0.60205, lty = 2, size = 0.25, color = "grey60") + # from fitted model below
#           geom_vline(xintercept = 0, lty = 2, size = 0.25, color = "grey60") +       # from fitted model below
#           geom_point() + 
#           geom_smooth(method = lm, se = F, fullrange = T) + 
#           geom_abline(intercept = 0.60205, slope = 0.67977, lty = 3) +
#           facet_wrap( ~ synonym, nrow = 3)
                       
#Previous Nested Comparison Data from my QP
#ept_elog <- ept_df %>% 
#  group_by(., participant, proficiency, countfreq, groupCon, response, item) %>% 
#  summarize(., n = 8, 
#            wDOM = sum(response), 
#            woDOM = n - wDOM, 
#            eLog = log((wDOM + 0.5) / (n - wDOM + 0.5)), 
#            wts = 1 / (wDOM + 0.5) + 1 / (n - wDOM + 0.5),
#            countLog = log(countfreq))
  

#nested model comparison
## GOOD
mod_nul <- lm(eLog ~1, data = ept_elog2)
summary(mod_nul)

mod_cat <-lm(eLog ~ countLog, data = ept_elog2)
summary(mod_cat)

mod_con <-lm(eLog ~ countLog + proficiency, data = ept_elog2)
summary(mod_con)

mod_int <-lm(eLog ~ countLog * proficiency, data = ept_elog2)
summary(mod_int)

anova(mod_nul, mod_cat, mod_con, mod_int)

## EPT en elog sin los verbos mas y menos frecuentes y sin controles

ept_elog2_hs <- ept_elog2 %>% filter(., proficiency != 'co') %>% 
  mutate(., profCon = if_else(proficiency == 'hs_adv', true = 0.5, false = -0.5))

mod_nul_hs <- lm(eLog ~1, data = ept_elog2_hs)
summary(mod_nul)

mod_cat_hs <-lm(eLog ~ countLog, data = ept_elog2_hs)
summary(mod_cat)

mod_con_hs <-lm(eLog ~ countLog + proficiency, data = ept_elog2_hs)
summary(mod_con)

mod_int_hs <-lm(eLog ~ countLog * proficiency, data = ept_elog2_hs)
summary(mod_int)

anova(mod_nul_hs, mod_cat_hs, mod_con_hs, mod_int_hs)

#ept_elog2_hs %>%
#  ggplot(., aes(x = countLog, y = response, color = proficiency)) +
#  geom_point()+
#  geom_smooth(method =lm)+
#  scale_color_brewer(palette = "Set1")


#lm_full <- lmer(eLog ~ proficiency * countLog +
#                  (1 | participant),
#                data = ept_elog, weights = 1/wts, 
#                control=lmerControl(optimizer="bobyqa"))


#anova(lm_null, lm_prof, lm_full, test = 'Chisq')

#summary(lm_full)
#ranef(lm_full)

#ept_elog_hs <- ept_elog %>% filter(., proficiency != 'co') %>% 
# mutate(., profCon = if_else(proficiency == 'hs_adv', true = 0.5, false = -0.5))

#lm_hs <- lmer(eLog ~ countLog * profCon + 
#                (1 | participant), 
#              data = ept_elog_hs, weights = 1/wts, 
#              control = lmerControl(optimizer = 'bobyqa')) 

#summary(lm_hs)





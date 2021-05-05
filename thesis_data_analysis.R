
#throw the output images in a powerpoint for will, include a note for the # of outliers, find a way to colour boxplots if possible, use the work hours since

if(!require("broom")) install.packages("broom")
if(!require("fs")) install.packages("fs")

pacman::p_load(pacman, rio, tidyverse, broom, fs, ggpubr, rstatix, datarium)

library(ggpubr)
getwd()
setwd("C:/Users/elizk/OneDrive/Desktop/Thesis/Data analysis")
#(df <- import("recoded_clean_data.csv") %>% as_tibble())
(df <- import("Latest_recoded_clean_data.csv") %>% as_tibble())



#assign item in question to variable const
const <- df$Fun_Item1

#Mean and SD for item const in control condition

ctlmean <- mean(const[df$Condition=='Control'])
ctlsd <- sd(const[df$Condition=='Control'])

ctlmean
ctlsd

#Mean and SD for item const in experimental condition

expmean <- mean(const[df$Condition=='Experimental'])
expsd <- sd(const[df$Condition=='Experimental'])

expmean
expsd

#difference in means accross conditions for item const (exp - ctl)

meandiff <- expmean - ctlmean
sddiff <- expsd - ctlsd

meandiff
sddiff


#t-test with confidence interval for variable const
t.test(const ~ df$Condition)

#cohens d for effect size, need to specify item within function

df %>%
  cohens_d(Competitive_Item1 ~ Condition, var.equal = TRUE)



#identifying outliers

df %>%
    group_by(Condition) %>%
    identify_outliers(Mistreat_Item1)

identify_outliers(df,Mistreat_Item1)


#check equality of variances 

df %>% levene_test(ManipulationCheck_Item1 ~ Condition)

#since variance is equal between two groups (insig leven test), student's t-test

stat.test <- df %>%
  t_test(ManipulationCheck_Item1 ~ Condition, var.equal = TRUE) %>%
  add_significance()
stat.test


#report
#adds stats to box plot
 stat.test <- stat.test %>% add_xy_position(x = "Condition")
 
#prints plot with stats 
 bxp + 
   stat_pvalue_manual(stat.test, tip.length = 0) +
   labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#Shapiro-Wilk test for normality if not equality of variance
 shapiro.test(df$Perfect_Item2)
 
 #pearson's R correlation between 2 variables
 

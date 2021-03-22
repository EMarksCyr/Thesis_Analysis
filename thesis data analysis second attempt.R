
#throw the output images in a powerpoint for will, include a note for the # of outliers, find a way to colour boxplots if possible, use the work hours since

if(!require("broom")) install.packages("broom")
if(!require("fs")) install.packages("fs")
)
pacman::p_load(pacman, rio, tidyverse, broom, fs, ggpubr, rstatix, datarium)

library(ggpubr)
getwd()
setwd("C:/Users/elizk/OneDrive/Desktop/Thesis/Data analysis")
(df <- import("recoded_clean_data.csv") %>% as_tibble())
# 
# test_var <- "ExploitLegit_Item1"
# compare_by <- "Condition"

# testing for correlations between items
#cor.test(df$exploitlegit_1, df$exploitlegit_2, method = "pearson", alternative = "two.sided", exact = TRUE)


#boxplot to analyze spread. code from https://www.datanovia.com/en/lessons/t-test-in-r/#independent-samples-t-test and https://cran.r-project.org/web/packages/ggpubr/ggpubr.pdf 
bxp <- ggboxplot(
  df, x = "Condition", y = "ManipulationCheck_Item1",
  ylab = "Condition", xlab = "Manipulation Check Item 1", bxp.errorbar = TRUE, add = "jitter"
)
bxp

# It would be more efficient to just edit and re-run the code for each variable 
# bxp <- ggboxplot(
#  df, x = compare_by, y = test_var,
#  ylab = compare_by, xlab = test_var, bxp.errorbar = TRUE, add = "jitter"
# )
# bxp

#identifying outliers, don't think this is working
df %>%
    group_by(Condition) %>%
    identify_outliers(Mistreat_Item1)

identify_outliers(df,Mistreat_Item1)


#qq plot to check normality of groups apparently?
#ggqqplot(df, x = "RewardWork_Item2", facet.by = "Condition")

#check equality of variances 

df %>% levene_test(ManipulationCheck_Item1 ~ Condition)

#since variance is equal between two groups (insig leven test), student's t-test

stat.test <- df %>%
  t_test(ManipulationCheck_Item1 ~ Condition, var.equal = TRUE) %>%
  add_significance()
stat.test

#cohens d for effect size

df %>%
  cohens_d(Perfect_Item2 ~ Condition, var.equal = TRUE)

#report
#adds stats to box plot
 stat.test <- stat.test %>% add_xy_position(x = "Condition")
 
#prints plot with stats 
 bxp + 
   stat_pvalue_manual(stat.test, tip.length = 0) +
   labs(subtitle = get_test_label(stat.test, detailed = TRUE))

#Shapiro-Wilk test for normality
 shapiro.test(df$Perfect_Item2)
 
 #pearson's R correlation between 2 variables
 

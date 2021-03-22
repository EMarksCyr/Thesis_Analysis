if(!require("ggplot2")) install.packages("ggplot2")
pacman::p_load(pacman, rio, tidyverse, broom, fs, ggpubr, rstatix, datarium)
pacman::p_load(ggplot2, dplyr, hrbrthemes, viridis, forcats, tidyr)
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggcorrplot")
import_titillium_web()
hrbrthemes.loadfonts = TRUE
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(forcats)
library(tidyr)
library(ggcorrplot)

getwd()
setwd("C:/Users/elizk/OneDrive/Desktop/Thesis/Data analysis")
(df <- import("recoded_clean_data.csv") %>% as_tibble())


#sample size
sample_size = df %>% group_by(Condition)%>% dplyr::summarize(num=n())

#violin chart wrapping boxplot

vbp <-
  df%>%
  left_join(sample_size)%>%
  mutate(myaxis = paste0(Condition, "\n", "n=", num))%>%
  ggplot(aes(x=myaxis, y=Perfect_Item2, fill=Condition, colour = Condition)) +
  geom_violin(width=0.9, size=0.2, alpha=0.5) +
  scale_fill_manual(values=c("#f6c6c6", "#C5CBE2")) +
  geom_boxplot(width=0.4, alpha=0.2) +
  #ggpubr::theme_transparent() +
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),    
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "none",
    plot.title = element_text(size=11),
    text = element_text(size=15)
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
  )+
  #ggtitle("Mistreatment Item 1") +
  xlab("Condition") +
  ylab("Expectations of Perfectionism Item 2")
vbp

ggsave("Perfect_Item2_vio_box_plot.png", vbp, bg = "transparent")


#histogram
his<- 
  ggplot(df, aes(x=Perfect_Item2, color=Condition, fill=Condition)) +
  scale_fill_manual(values=c("#f6c6c6", "#C5CBE2")) +
  geom_histogram(alpha=0.5, position="identity", binwidth=1, size=1) +
  #ggpubr::theme_transparent() +
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),    
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA),
    #panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    plot.title = element_text(size=11),
    text = element_text(size=15))+
    xlab("Expectations of Perfectionism Item 2") +
    ylab("Frequency")
his


ggsave("Perfect_Item2_histo.png", his, bg = "transparent")

#box plot

df%>%
  left_join(sample_size)%>%
  mutate(myaxis = paste0(Condition, "\n", "n=", num))%>%
  ggplot( aes(x=myaxis, y=ManipulationCheck_Item1, fill=Condition)) +
  geom_boxplot() +
  #scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  scale_fill_manual(values=c("#C5CBE2","#f6c6c6")) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  xlab("Condition") +
  ylab("Manipulation Check")

#violin chart
df%>%
  left_join(sample_size)%>%
  mutate(myaxis = paste0(Condition, "\n", "n=", num))%>%
  ggplot(aes(x=Condition, y=ManipulationCheck_Item1, fill=Condition)) +
  geom_violin()

#making a new data fram with select columns

df_new <- df%>%
  select(ManipulationCheck_Item1, Fun_Item1, WellBeing_Item1, RewardWork_Item1, Competitive_Item1, ExploitLegit_Item1,
         ExploitLegit_Item2, Mistreat_Item1, Mistreat_Item2, Perfect_Item1, Perfect_Item2, ExtraWork_Item1, ExtraWork_Item2,
         WorkHours_Item1, WorkHours_Item2, RewardWork_Item2, Competitive_Item1, Competitive_Item2,
         CompanyFirst_Item1, Loyalty_Item1, Gratitude_Item1)
df_new
#Correlation matrix
data(df_new)
cormat <- round(cor(df_new), 2)
head(cormat)

#Compute a matrix of correlation p-values
p.mat <- cor_pmat(df_new)
head(p.mat)

#visualize the correlation matrix with ggcorrplot
ggcorrplot(cormat, type = "upper",
           lab = TRUE, p.mat = p.mat, insig = "blank",
           outline.color = "white",
           legend.position = "left",
           colors = c("#0047bd", "white", "#bd0012"))

#correlation 
cor.test(df$ExploitLegit_Item1, df$ExploitLegit_Item2) 

#t-test
stat.test <- df %>%
  t_test(ManipulationCheck_Item1 ~ Condition, var.equal = TRUE) %>%
  add_significance()
stat.test

#adds stats to box plot
stat.test <- stat.test %>% add_xy_position(x = "Condition")

#prints plot with stats, not working right now
#plot + 
#  stat_pvalue_manual(stat.test, tip.length = 0) +
#  labs(subtitle = get_test_label(stat.test, detailed = TRUE))
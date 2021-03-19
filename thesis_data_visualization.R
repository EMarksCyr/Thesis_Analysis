if(!require("ggplot2")) install.packages("ggplot2")
pacman::p_load(pacman, rio, tidyverse, broom, fs, ggpubr, rstatix, datarium)
pacman::p_load(ggplot2, dplyr, hrbrthemes, viridis, forcats, tidyr)

import_titillium_web()
hrbrthemes.loadfonts = TRUE
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(forcats)
library(tidyr)

getwd()
setwd("C:/Users/elizk/OneDrive/Desktop/Thesis/Data analysis")
(df <- import("recoded_clean_data.csv") %>% as_tibble())


#sample size
sample_size = df %>%
  group_by(Condition)%>%
  summarize(num=n())

#violin chart wrapping boxplot
df%>%
  left_join(sample_size)%>%
  mutate(myaxis = paste0(Condition, "\n", "n=", num))%>%
  ggplot(aes(x=myaxis, y=ManipulationCheck_Item1, fill=Condition)) +
  geom_violin(width=0.9, size=0.2, color= NA) +
  scale_fill_manual(values=c("#C5CBE2","#f6c6c6")) +
  geom_boxplot(width=0.4, alpha=0.2) +
  #ggpubr::theme_transparent() +
  theme_bw() + 
  theme(
    legend.position = "none",
    plot.title = element_text(size=11),
    text = element_text(size=15)
    #panel.grid.major = element_blank(),
    #panel.grid.minor = element_blank(),
  )+
  #ggtitle("Manipulation Check Item 1") +
  xlab("Condition") +
  ylab("Manipulation Check")


#histogram
df<- 
  ggplot(df, aes(x=ManipulationCheck_Item1, color=Condition, fill=Condition)) +
  scale_fill_manual(values=c("#f6c6c6", "#C5CBE2")) +
  geom_histogram(alpha=0.5, position="identity", binwidth=1, size=1) +
  #ggpubr::theme_transparent() +
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA),
    #panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x=element_blank(),
    legend.position = "top",
    plot.title = element_text(size=11),
    text = element_text(size=15))+
  ylab("Manipulation Check")
#xlab(" ")


ggsave("manip_histo_trans.png", df, bg = "transparent")

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
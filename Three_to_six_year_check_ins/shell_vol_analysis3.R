# ----------------- #
# test if there was a change in shell volume between the 2015 and 2018 check in
# round 3 - data has been corrected since last iteration
# this is probably still incorrect due to naming conventions - Sara is looking into it
# ----------------- #

# ----------------- #
# load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(agricolae) #HSD.test
# ----------------- #

# ----------------- #
# set dir
# ----------------- #
dir.in = "G:/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/4.0 2018-2015 cohort/2.0 Data"
dir.out = "G:/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/shell_volume_analysis/"
# ----------------- #

# ----------------- #
# load data
# ----------------- #
dat = read_excel("G:/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/4.0 2018-2015 cohort/2.0 Data/shell_vol_final.xlsx", sheet=1)
 dat=dat%>% rename(vol='total vol per sq m') 


# ----------------- #

# ----------------- #
# aov
# ----------------- #
dat2 = mutate(dat, reef_year = paste(ReefID, Year)) 

dat_aov_totVol = aov(TotVolume ~ reef_year, data = dat2)
summary(dat_aov_totVol)

dat_aov_surfVol = aov(surfShellVol ~ reef_year, data = dat2)
summary(dat_aov_surfVol)
# ----------------- #

# ----------------- #
# tukeys HSD
# ----------------- #
#TukeyHSD(dat_aov) 

#total
out = HSD.test(dat_aov_totVol, "reef_year", console=TRUE)

x = as.data.frame(out$groups)
y = as.data.frame(matrix(rownames(x))) %>% 
  mutate(reef = sapply(strsplit(as.character(V1), "\\s+"), head, 1), 
         year = sapply(strsplit(as.character(V1), "\\s+"), "[[", 2)) %>% 
  dplyr::select(-V1)
HSD_groups = cbind(x,y)

x = as.data.frame(out$means)
y = as.data.frame(matrix(rownames(x))) %>% 
  mutate(reef = sapply(strsplit(as.character(V1), "\\s+"), head, 1), 
         year = sapply(strsplit(as.character(V1), "\\s+"), "[[", 2)) %>% 
  dplyr::select(-V1)
HSD_means = cbind(x,y)

# surf
out2 = HSD.test(dat_aov_surfVol, "reef_year", console=TRUE)

x = as.data.frame(out2$groups)
y = as.data.frame(matrix(rownames(x))) %>% 
  mutate(reef = sapply(strsplit(as.character(V1), "\\s+"), head, 1), 
         year = sapply(strsplit(as.character(V1), "\\s+"), "[[", 2)) %>% 
  dplyr::select(-V1)
HSD_groups2 = cbind(x,y)

x = as.data.frame(out2$means)
y = as.data.frame(matrix(rownames(x))) %>% 
  mutate(reef = sapply(strsplit(as.character(V1), "\\s+"), head, 1), 
         year = sapply(strsplit(as.character(V1), "\\s+"), "[[", 2)) %>% 
  dplyr::select(-V1)
HSD_means2 = cbind(x,y)
# ----------------- #


# ----------------- #
# descriptive tables
# ----------------- #
#boxplot
p =  ggplot() + geom_boxplot(data = dat2, aes(x = ReefID, y = vol, fill = as.character(Year))) + theme_bw()+ 
+         geom_text(data = HSD_groups, aes(x = reef, y = vol + 25, label = groups, col = year), fontface = "bold", size = 5, position = "dodge")+
+       theme(text = element_text(size = 20))+
+       ggtitle("Total Shell Volume")
p
ggsave(paste(dir.out, "tot_shell_vol_boxplot.png",sep=""),p)

p = ggplot() + geom_boxplot(data = dat2, aes(x = reef, y = surfShellVol, fill = year)) + theme_bw() + 
  geom_text(data = HSD_groups2, aes(x = reef, y = surfShellVol + 25, label = groups, col = year), fontface = "bold", size = 5, position = "dodge")+
  theme(text = element_text(size = 20))+
  ggtitle("Surface Shell Volume")
p
ggsave(paste(dir.out, "surf_shell_vol_boxplot.png",sep=""),p)


# tot.
t1 = HSD_groups %>% group_by(reef) %>% summarize(n=n()) %>% filter(n>1)
t2 = HSD_groups %>% filter(reef %in% t1$reef) %>% arrange(reef, year) %>%
  group_by(reef) %>% mutate(t = groups == lag(groups)) %>%
  filter(any(t %in% TRUE))
sig.diff.years = HSD_groups %>% filter(reef %in% t1$reef, !reef %in% t2$reef) %>% arrange(reef,year) %>%
  group_by(reef) %>% mutate(diffs = TotVolume - lag(TotVolume)) %>% filter(!is.na(diffs))
# anom_tab = dplyr::select(HSD_groups, reef) %>% distinct(reef) %>% arrange(reef) %>%
#   mutate(diffs = NA,
#          diffs = replace(diffs, reef %in% sig.diff.years$reef, sig.diff.years$diffs),
#          diffs = replace(diffs, reef %in% t2$reef, NA),
#          Change = "Stable",
#          Change = replace(Change, diffs > 0, "Increase"),
#          Change = replace(Change, diffs < 0, "Decrease"))
anom_tab = dplyr::select(HSD_groups, reef) %>% distinct(reef) %>% arrange(reef) %>%
  mutate(diffs = 0.1, 
         diffs = replace(diffs, reef %in% sig.diff.years$reef, sig.diff.years$diffs),
         diffs = replace(diffs, reef %in% t2$reef, NA),
         Change = "Stable", 
         Change = replace(Change, diffs > 0.1, "Increase"), 
         Change = replace(Change, diffs < -0.1, "Decrease"),
         diffs = replace(diffs, is.na(diffs), 0.1))

p = ggplot() + geom_bar(data = anom_tab, aes(x = reef, y = diffs, fill = Change), stat="identity", position = "dodge") + 
  labs(x = "Reef", y = "Difference in Volume when Sig. Diff. btwn years") + 
  scale_fill_manual(values=c("blue","red","black")) + 
  scale_x_discrete(drop = FALSE) + theme_bw() +
  theme(text = element_text(size = 20)) + 
  ggtitle("Total Shell Volume")
p
ggsave(paste(dir.out, "tot_shell_vol_anom.png", sep=""), p)

# surf
t1 = HSD_groups2 %>% group_by(reef) %>% summarize(n=n()) %>% filter(n>1)
t2 = HSD_groups2 %>% filter(reef %in% t1$reef) %>% arrange(reef, year) %>%
  group_by(reef) %>% mutate(t = groups == lag(groups)) %>%
  filter(any(t %in% TRUE))
sig.diff.years = HSD_groups2 %>% filter(reef %in% t1$reef, !reef %in% t2$reef) %>% arrange(reef,year) %>%
  group_by(reef) %>% mutate(diffs = surfShellVol - lag(surfShellVol)) %>% filter(!is.na(diffs))
# anom_tab = dplyr::select(HSD_groups2, reef) %>% distinct(reef) %>% arrange(reef) %>%
#   mutate(diffs = NA,
#          diffs = replace(diffs, reef %in% sig.diff.years$reef, sig.diff.years$diffs),
#          diffs = replace(diffs, reef %in% t2$reef, NA),
#          Change = "Stable",
#          Change = replace(Change, diffs > 0, "Increase"),
#          Change = replace(Change, diffs < 0, "Decrease"))
anom_tab = dplyr::select(HSD_groups, reef) %>% distinct(reef) %>% arrange(reef) %>%
  mutate(diffs = 0.1, 
         diffs = replace(diffs, reef %in% sig.diff.years$reef, sig.diff.years$diffs),
         diffs = replace(diffs, reef %in% t2$reef, NA),
         Change = "Stable", 
         Change = replace(Change, diffs > 0.1, "Increase"), 
         Change = replace(Change, diffs < -0.1, "Decrease"),
         diffs = replace(diffs, is.na(diffs), 0.1))

p = ggplot() + geom_bar(data = anom_tab, aes(x = reef, y = diffs, fill = Change), stat="identity", position = "dodge") + 
  labs(x = "Reef", y = "Difference in Volume when Sig. Diff. btwn years") + 
  scale_fill_manual(values=c("blue","red","black")) + 
  scale_x_discrete(drop = FALSE) + theme_bw() +
  theme(text = element_text(size = 20)) + 
  ggtitle("Surface Shell Volume")
p
ggsave(paste(dir.out, "surf_shell_vol_anom.png", sep=""), p)

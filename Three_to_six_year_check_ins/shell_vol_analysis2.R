# ----------------- #
# test if there was a change in shell volume between the 2015 and 2018 check in
# round 2
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
dir.out = "G:/1.0 Restoration and Monitoring/1.0 3_6_yr_monitoring/shell_volume_analysis/"
# ----------------- #

# ----------------- #
# load data
# ----------------- #
dat_2015 <- read_excel("C:/Users/kcoleman/Downloads/HistoricMonitoringData_2012_2018_final.xlsx", sheet=3)
dat = filter(dat_2015, !is.na(TotVolume)) %>% 
  dplyr::select(SampleEvent, Rep, TotVolume, pctBlackShell) %>%
  mutate(year = sapply(strsplit(SampleEvent,"-"), "[[", 2), 
         trib = sapply(strsplit(SampleEvent, "-"), head, 1),
         survey = sapply(strsplit(SampleEvent,"-"), "[[", 3),
         reef = sapply(strsplit(SampleEvent, "-"), tail, 1),
         type = sapply(strsplit(SampleEvent, "-"), "[[", 4),
         surfShellVol = TotVolume - (TotVolume * (pctBlackShell/100)),
         type2 = paste(type, reef, sep = "-")) %>%
  filter(type2 %in% c("CS-001","CS-002","CS-003","CS-004","SS-104","PM-999","SO-TR3","SO-TR4"),
         trib %in% "HARR", year %in% c(2015, 2018))
#rm(dat_2015)
# ----------------- #

# ----------------- #
# aov
# ----------------- #
dat2 = mutate(dat, reef_year = paste(reef, year)) 

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

# tot.
t1 = HSD_groups %>% group_by(reef) %>% summarize(n=n()) %>% filter(n>1)
t2 = HSD_groups %>% filter(reef %in% t1$reef) %>% arrange(reef, year) %>%
  group_by(reef) %>% mutate(t = groups == lag(groups)) %>%
  filter(any(t %in% TRUE))
sig.diff.years = HSD_groups %>% filter(reef %in% t1$reef, !reef %in% t2$reef) %>% arrange(reef,year) %>%
  group_by(reef) %>% mutate(diffs = TotVolume - lag(TotVolume)) %>% filter(!is.na(diffs))
anom_tab = dplyr::select(HSD_groups, reef) %>% distinct(reef) %>% arrange(reef) %>%
  mutate(diffs = NA,
         diffs = replace(diffs, reef %in% sig.diff.years$reef, sig.diff.years$diffs),
         diffs = replace(diffs, reef %in% t2$reef, NA),
         Change = "Stable",
         Change = replace(Change, diffs > 0, "Increase"),
         Change = replace(Change, diffs < 0, "Decrease"))

# surf
t1 = HSD_groups2 %>% group_by(reef) %>% summarize(n=n()) %>% filter(n>1)
t2 = HSD_groups2 %>% filter(reef %in% t1$reef) %>% arrange(reef, year) %>%
  group_by(reef) %>% mutate(t = groups == lag(groups)) %>%
  filter(any(t %in% TRUE))
sig.diff.years = HSD_groups2 %>% filter(reef %in% t1$reef, !reef %in% t2$reef) %>% arrange(reef,year) %>%
  group_by(reef) %>% mutate(diffs = surfShellVol - lag(surfShellVol)) %>% filter(!is.na(diffs))
anom_tab = dplyr::select(HSD_groups2, reef) %>% distinct(reef) %>% arrange(reef) %>%
  mutate(diffs = NA,
         diffs = replace(diffs, reef %in% sig.diff.years$reef, sig.diff.years$diffs),
         diffs = replace(diffs, reef %in% t2$reef, NA),
         Change = "Stable",
         Change = replace(Change, diffs > 0, "Increase"),
         Change = replace(Change, diffs < 0, "Decrease"))

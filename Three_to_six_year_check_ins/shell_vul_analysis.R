# ----------------- #
# test if there was a change in shell volume between the 2015 and 2018 check in
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
         surfShellVol = TotVolume - (TotVolume * (pctBlackShell/100))) %>% 
  filter(trib %in% "HARR", year %in% c(2015, 2018), survey %in% c("3YR", "6YR"))
rm(dat_2015)
# ----------------- #

# dat_2018 <- read_excel("C:/Users/kcoleman/Downloads/Copy of BluePrint_2018.xlsx")
# names(dat_2018) = gsub(" ", "", names(dat_2018), fixed = TRUE)
# dat_2018 = dat_2018[34:42,]

#dat_2015_2 <- read_excel("C:/Users/kcoleman/Downloads/HistoricMonitoringData_2012_2018_final.xlsx", sheet=2)

#dat_2015_2 = dplyr::select(dat_2015_2, SampleEvent, SampleDate) %>% 
#  mutate(yr = year(SampleDate))
#  left_join(., dat_2015_2, by = "SampleEvent")


# tests = as.data.frame(matrix(ncol=3,nrow=9,data = NA))
# names(tests) = c("reefID","shellvol_2015","shellvol_2018")
# tests[,3] = dat_2018$'Aveshellvolumeacrossentirereef(litresperm2)'
# tests[,2] = as.numeric(dat_2018$'Aveshellvolumeacrossentirereef(litresperm2)') - as.numeric(dat_2018$TotalVolumeChange)
# tests[,1] = dat_2018$ReportReefIDs

# ----------------- #
# check to see that they are normal (but since reefID isnt really a continuous variable this is probably silly)
# ----------------- #
ggplot() + geom_bar(data = filter(dat, year %in% 2015), aes(x = reef, y = TotVolume), stat="identity")

ggplot() + geom_bar(data = filter(dat, year %in% 2018), aes(x = reef, y = TotVolume), stat="identity")

ggplot() + 
  geom_point(data = filter(dat, year %in% 2015), aes(x = reef, y = TotVolume)) + 
  geom_point(data = filter(dat, year %in% 2018), aes(x = reef, y = TotVolume), col = "magenta")
# ----------------- #

# ----------------- #
# aov
# ----------------- #
dat2 = mutate(dat, reef_year = paste(reef, year)) %>%
  filter(reef %in% c("002","008","046","056","072","106","TR1","TR2","TR5"))

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
# descriptive plots
# ----------------- #
#boxplot
p = ggplot() + geom_boxplot(data = dat2, aes(x = reef, y = TotVolume, fill = year)) + theme_bw() + 
  geom_text(data = HSD_groups, aes(x = reef, y = TotVolume + 25, label = groups, col = year), fontface = "bold", size = 5, position = "dodge")+
  theme(text = element_text(size = 20))+
  ggtitle("Total Shell Volume")
p
ggsave(paste(dir.out, "tot_shell_vol_boxplot.png",sep=""),p)

p = ggplot() + geom_boxplot(data = dat2, aes(x = reef, y = surfShellVol, fill = year)) + theme_bw() + 
  geom_text(data = HSD_groups2, aes(x = reef, y = surfShellVol + 25, label = groups, col = year), fontface = "bold", size = 5, position = "dodge")+
  theme(text = element_text(size = 20))+
  ggtitle("Surface Shell Volume")
p
ggsave(paste(dir.out, "surf_shell_vol_boxplot.png",sep=""),p)

# anomoly 
t1 = HSD_groups %>% group_by(reef) %>% summarize(n=n()) %>% filter(n>1)
t2 = HSD_groups %>% filter(reef %in% t1$reef) %>% arrange(reef, year) %>% 
  group_by(reef) %>% mutate(t = groups == lag(groups)) %>% 
  filter(any(t %in% TRUE))
sig.diff.years = HSD_groups %>% filter(reef %in% t1$reef, !reef %in% t2$reef) %>% arrange(reef,year) %>% 
  group_by(reef) %>% mutate(diffs = TotVolume - lag(TotVolume)) %>% filter(!is.na(diffs))
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
# anomoly 
t1 = HSD_groups2 %>% group_by(reef) %>% summarize(n=n()) %>% filter(n>1)
t2 = HSD_groups2 %>% filter(reef %in% t1$reef) %>% arrange(reef, year) %>% 
  group_by(reef) %>% mutate(t = groups == lag(groups)) %>% 
  filter(any(t %in% TRUE))
sig.diff.years = HSD_groups2 %>% filter(reef %in% t1$reef, !reef %in% t2$reef) %>% arrange(reef,year) %>% 
  group_by(reef) %>% mutate(diffs = surfShellVol - lag(surfShellVol)) %>% filter(!is.na(diffs))
anom_tab = dplyr::select(HSD_groups2, reef) %>% distinct(reef) %>% arrange(reef) %>%
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

# ----------------- #


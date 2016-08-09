setwd("/home/kira/ownCloud/BR Data/doping olympische spiele")
setwd("/Users/administrator/Desktop/doping olympische spiele")
library(dplyr)
library(ggplot2)
cases <- read.csv("IAAF-doping-cases-clean-deutsch.csv", stringsAsFactors = F, encoding="utf-8")
athl <- read.csv("LA-teilnehmer.csv", quote="", encoding="utf-8", sep=";")

#fälle pro jahr
cpy <- cases %>% group_by(Jahr, Geschlecht) %>% summarize(count=length(unique(Name))) %>%
  group_by(Jahr) %>% summarize(count=sum(count))# %>% arrange(-count)
write.csv(cpy, "fälleprojahr.csv", row.names = F)

#fälle pro land
cpc <- cases %>% group_by(COUNTRY, Land, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(COUNTRY, Land) %>% summarize(count=sum(count)) %>% arrange(-count)
tmp <- athl %>% group_by(Land, Jahr, Geschlecht) %>% summarize(count=length(unique(Name))) %>%
  group_by(Land) %>% summarize(sum = sum(count))
cpc <- left_join(cpc, tmp, by=c("COUNTRY" = "Land")) %>% mutate(norm = count/sum)
write.csv(cpc, "fälleproland.csv", row.names = F)

#fälle pro disziplin
cpd <- cases %>% group_by(Disziplin) %>% summarize(count=n()) %>% arrange(-count)
tmp <- athl %>% group_by(Disziplin) %>% summarize(sum=n())
cpd <- left_join(cpd, tmp, by="Disziplin") %>% mutate(norm = count/sum)
write.csv(cpd, "fälleprodisziplin.csv", row.names = F)

#fälle pro kategorie
cpk <- cases %>% group_by(Kategorie, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(Kategorie) %>% summarize(count=sum(count)) %>%arrange(-count)
tmp <- athl %>% group_by(Kategorie, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(Kategorie) %>% summarize(sum=sum(count)) 
cpk <- left_join(cpk, tmp, by="Kategorie") %>% mutate(norm = count/sum)
write.csv(cpk, "fälleprokategorie.csv", row.names = F)

#fälle nach geschlecht
cpg <- cases %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(Geschlecht) %>% summarize(count=sum(count)) %>%arrange(-count)
tmp <- athl %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(Geschlecht) %>% summarize(sum=sum(count)) 
cpg <- left_join(cpg, tmp, by="Geschlecht") %>% mutate(norm = count/sum)
#fälle nach geschlecht und jahr
cpgy <- cases %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>%arrange(-count)
tmp <- athl %>% group_by(Geschlecht, Jahr) %>% summarize(sum=length(unique(Name)))
cpgy <- left_join(cpgy, tmp, by=c("Geschlecht","Jahr")) %>% mutate(norm = count/sum)
write.csv(cpg, "fällenachgeschlecht.csv", row.names = F)
write.csv(cpgy, "fällenachgeschlechtundjahr.csv", row.names = F)

save.image(file="datasets.Rdata")

##################
##visualisierung##
##################

library(ggplot2)
pdf("results/alle-normiert.pdf", width=14)

#fälle pro jahr
ggplot(cpy, aes(x=Jahr, y=count)) + theme_light() + scale_x_continuous(breaks = rev(unique(cpgy$Jahr))) +
  ggtitle("Doping-Fälle über die Jahre") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Jahr, label=paste0(count)), vjust=-1)

#fälle pro land
#sonstiges kategorie einrichten
cpc <- cpc %>% ungroup %>% arrange(-count)
x <- cpc %>% mutate(Land = c(cpc$Land[1:5], rep("Sonstige",length(cpc$Land)-5)))
x <- mutate(x, Land = factor(x$Land, levels=c("Türkei", "Russland","Belarus","Ukraine","USA","Sonstige"))) %>%
  group_by(Land) %>% summarize(count=sum(count), sum=sum(sum), norm=count/sum)
#gesamtdurchschnitt
cmean = sum(cpc$count)/sum(cpc$sum) #0.01231637
ggplot(x, aes(x=Land, y=norm, fill=Land)) + theme_minimal() +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Ländern") +
  geom_hline(yintercept = cmean) + theme(legend.position="none") +
  geom_text(aes(x=1, y=cmean, label=paste("Gesamtdurchschnitt =", round(cmean,4))), hjust=0,vjust=-0.5)+
  geom_text(position= position_dodge(width=0.9), aes(x=Land, label=paste0(count,"/",sum)), vjust=-0.5)

#fälle nach geschlecht
cpg <- cpg %>% ungroup %>% arrange(-norm)
ggplot(cpg, aes(x=Geschlecht, y=norm, fill=Geschlecht)) + theme_minimal() +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Geschlecht") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Geschlecht, label=paste0(count,"/",sum)), vjust=-0.5)

#fälle nach geschlecht und jahr
cpgy <- cpgy %>% ungroup %>% arrange(-norm)
ggplot(cpgy, aes(x=Jahr, y=norm, fill=Geschlecht)) + scale_x_continuous(breaks = rev(unique(cpgy$Jahr))) +theme_minimal() +
  geom_bar(stat="identity",position="dodge") + ggtitle("Doping-Fälle nach Geschlecht und Jahr") +
  theme(legend.position="bottom", panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=3.5), aes(x=Jahr, label=paste0(count,"/",sum)), vjust=-0.5)

#fälle pro disziplin
cpd <- cpd %>% ungroup %>% arrange(-norm)
cpd$Disziplin <- factor(cpd$Disziplin, levels=unique(cpd$Disziplin))
ggplot(cpd, aes(x=Disziplin, y=norm, fill=Disziplin)) +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Disziplin") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Disziplin, label=paste0(count,"/",sum)), vjust=-0.5)

#fälle pro kategorie
cpk <- cpk %>% ungroup %>% arrange(-norm)
cpk$Kategorie <- factor(cpk$Kategorie, levels=unique(cpk$Kategorie))
ggplot(cpk, aes(x=Kategorie, y=norm, fill=Kategorie)) +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Kategorie") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Kategorie, label=paste0(count,"/",sum)), vjust=-0.5)

dev.off()

#zahlenabgleich
iaaf <- read.csv("iaaf-totals.csv",sep=";",quote="",stringsAsFactors = F)
#gesamtzahl nach jahren
ynrs <- athl %>% group_by(Jahr) %>% summarize(count = length(unique(Name))) %>% left_join(iaaf[c(1,4)], by=c("Jahr"="Year")) %>%
  mutate(diff = count-Athletes, perc=abs(1-(count/Athletes)))
#länderzahl nach jahren
cnrs <- athl %>% group_by(Jahr) %>% summarize(count = length(unique(Land))) %>% left_join(iaaf[c(1,3)], by=c("Jahr"="Year")) %>%
  mutate(diff = count-Countries, perc=abs(1-(count/Countries)))
#männerzahl nach jahren
mnrs <- athl %>% group_by(Jahr) %>% filter(Geschlecht == "Men") %>%
  summarize(count = length(unique(Name))) %>% left_join(iaaf[c(1,5)], by=c("Jahr"="Year")) %>%
  mutate(diff = count-Men, perc=abs(1-(count/Men)))
#frauenzahl nach jahren
wnrs <- athl %>% group_by(Jahr) %>% filter(Geschlecht == "Women") %>%
  summarize(count = length(unique(Name))) %>% left_join(iaaf[c(1,6)], by=c("Jahr"="Year")) %>%
  mutate(diff = count-Women, perc=abs(1-(count/Women)))
#eventzahl nach jahren
dnrs <- athl %>% group_by(Jahr, Geschlecht) %>% summarize(count = length(unique(Disziplin))) %>%
  group_by(Jahr) %>% summarize(count = sum(count)) %>% left_join(iaaf[c(1,7)], by=c("Jahr"="Year")) %>%
  mutate(diff = count-Events, perc=abs(1-(count/Events)))

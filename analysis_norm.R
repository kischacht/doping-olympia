setwd("/home/kira/ownCloud/BR Data/doping olympische spiele")
setwd("/Users/administrator/Desktop/doping olympische spiele")
library(dplyr)
library(ggplot2)
cases <- read.csv("IAAF-doping-cases-clean-deutsch.csv", stringsAsFactors = F, encoding="utf-8")
#athl <- read.csv("participants-by-event.csv", quote="", encoding="utf-8", sep=";")
#names(athl) = c("Jahr","Ort","Name","Land","Geschlecht","Sport","Disziplin","Medal")
library(tidyr)
athl <- read.csv("iaaf-teilnehmer.csv",sep=";",stringsAsFactors = F)
athl <- gather(athl, Geschlecht, count, 2:length(athl))
athl$Jahr <- as.numeric(substr(athl$Geschlecht,2,5))
athl$Geschlecht <- gsub("^X.*\\.","",athl$Geschlecht)
names(athl)[1] = "Land"
write.csv(athl, "iaaf-teilnehmer.csv", row.names = F)
athl <- read.csv("iaaf-teilnehmer.csv",stringsAsFactors = F)
#fälle pro jahr
cpy <- cases %>% group_by(Jahr) %>% summarize(count=length(unique(Name)))# %>%
  group_by(Jahr) %>% summarize(count=sum(count))# %>% arrange(-count)
tmp <- athl %>% group_by(Jahr) %>% summarize(sum = sum(count))
cpy <- left_join(cpy, tmp, by=c("Jahr")) %>% mutate(norm = count/sum)
write.csv(cpy, "fälleprojahr.csv", row.names = F)

#fälle pro land
cpc <- cases %>% group_by(COUNTRY, Land, Jahr) %>% filter(Jahr >= 1996) %>%
  summarize(count=length(unique(Name))) %>%
  group_by(COUNTRY, Land) %>% summarize(count=sum(count)) %>% arrange(-count)
tmp <- athl %>% filter(Jahr >= 1996) %>% group_by(Land) %>% summarize(sum = sum(count))
cpc <- left_join(cpc, tmp, by=c("COUNTRY" = "Land")) %>% mutate(norm = count/sum)
write.csv(cpc, "fälleproland.csv", row.names = F)


#fälle nach geschlecht
cpg <- cases %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(Geschlecht) %>% summarize(count=sum(count)) %>%arrange(-count)
tmp <- athl %>% group_by(Geschlecht) %>% summarize(sum=sum(count))
  #spread(Geschlecht, count) %>% mutate(wshare = Women/(Women+Men))
cpg <- left_join(cpg, tmp, by="Geschlecht") %>% mutate(norm = count/sum)
write.csv(cpg, "fällenachgeschlecht.csv", row.names = F)

#fälle nach geschlecht und jahr
cpgy <- cases %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>%arrange(-count)
tmp <- athl %>% group_by(Geschlecht, Jahr) %>% summarize(sum=sum(count))
cpgy <- left_join(cpgy, tmp, by=c("Geschlecht","Jahr")) %>% mutate(norm = count/sum)
write.csv(cpgy, "fällenachgeschlechtundjahr.csv", row.names = F)

#tests pro jahr
#tests bei olympischen spielen
tests <- read.csv("testnrs.csv")[c(1,3)]
tests$Number.of.tests <- as.numeric(gsub(",","",tests$Number.of.tests))

save.image(file="datasets.Rdata")

##################
##visualisierung##
##################

library(ggplot2)
pdf("results/alle-normiert.pdf", width=14)

#fälle pro jahr
ggplot(cpy, aes(x=Jahr, y=count)) + theme_light() + scale_x_continuous(breaks = rev(unique(cpy$Jahr))) +
  ggtitle("Doping Violations in Athletics at Olympic Games") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Jahr, label=paste0(count)), vjust=-1)
'
#fälle pro jahr normiert
ggplot(cpy, aes(x=Jahr, y=norm)) + theme_light() + scale_x_continuous(breaks = rev(unique(cpgy$Jahr))) +
  ggtitle("Anteil überführter Athleten über die Jahre") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Jahr, label=paste0(round(norm*100, 2),"%")), vjust=-1) + scale_y_continuous(labels=scales::percent)


#tests pro jahr
ggplot(tests, aes(x=Year, y=Number.of.tests)) + theme_light() + scale_x_continuous(breaks = rev(unique(tests$Year))) +
  ggtitle("Gesamtzahl an Doping-Tests bei den olympischen Spielen") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Year, label=paste0(Number.of.tests)), vjust=-1)
'
#fälle pro land
#sonstiges kategorie einrichten
cpc <- cpc %>% ungroup %>% arrange(-count)
x <- cpc %>% mutate(Land = c(cpc$Land[1:5], rep("Sonstige",length(cpc$Land)-5)))
x <- mutate(x, Land = factor(x$Land, levels=c("Türkei","Belarus","Russland","Ukraine","USA","Sonstige"))) %>%
  group_by(Land) %>% summarize(count=sum(count), sum=sum(sum), norm=count/sum)
#x$Land <- factor(c("Turkey","Belarus","Russia","Ukraine","USA","Others"), levels=c("Turkey","Belarus","Ukraine","Russia","USA","Others"))
#sonstiges summe und norm
tmp <- athl %>% filter(Jahr >= 1996) %>% group_by(Land) %>% summarize(sum = sum(count))
tmp2 = filter(tmp, Land != "TUR" & Land != "RUS" & Land != "BLR" & Land != "UKR" & Land != "USA")
x$sum[6] <- sum(tmp2$sum); x$norm[6] <- x$count[6] / x$sum[6]
#gesamtdurchschnitt
cmean = sum(cpc$count)/sum(tmp$sum) #0.00514601
ggplot(x, aes(x=Land, y=norm, fill=Land)) + theme_minimal()  + scale_y_continuous(labels=scales::percent) +
  geom_bar(stat="identity") + ggtitle("Anteil überführter Leichtathleten seit 1996") +
  geom_hline(yintercept = cmean) + theme(legend.position="none") +
  geom_text(aes(x=1, y=cmean, label=paste("Total mean =", paste0(round(cmean*100,2),"%"))), hjust=0,vjust=-0.5)+
  geom_text(position= position_dodge(width=0.9), aes(x=Land, label=paste0(count,"/",sum)), vjust=-0.5)

#fälle nach geschlecht
cpg <- cpg %>% ungroup
ggplot(cpg, aes(x=Geschlecht, y=count, fill=Geschlecht)) + theme_minimal() + #scale_y_continuous(labels=scales::percent) +
  geom_bar(stat="identity") + ggtitle("Anzahl überführter Leichtathleten nach Geschlecht seit 1996") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Geschlecht, label=paste0(count,"/",sum)), vjust=-0.5)
'
#fälle nach geschlecht und jahr
cpgy <- cpgy %>% ungroup %>% arrange(-norm)
ggplot(cpgy, aes(x=Jahr, y=norm, fill=Geschlecht)) + scale_x_continuous(breaks = rev(unique(cpgy$Jahr))) +theme_minimal() +
  geom_bar(stat="identity",position="dodge") + ggtitle("Doping-Fälle nach Geschlecht und Jahr") +
  theme(legend.position="bottom", panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=3.5), aes(x=Jahr, label=paste0(count,"/",sum)), vjust=-0.5)
'
dev.off()
write.csv(x, "results/fälleproland-reduziert.csv",row.names = F)
length(unique(cases$Name))
sort(unique(cases$Name))
sum(cpy$count)

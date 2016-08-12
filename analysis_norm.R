setwd("/home/kira/ownCloud/BR Data/doping olympische spiele")
setwd("/Users/administrator/Desktop/doping olympische spiele")
library(dplyr)
library(ggplot2)
cases <- read.csv("IAAF-doping-cases-clean-deutsch.csv", stringsAsFactors = F, encoding="utf-8")
athl <- read.csv("participants-by-event.csv", quote="", encoding="utf-8", sep=";")
names(athl) = c("Jahr","Ort","Name","Land","Geschlecht","Sport","Disziplin","Medal")

#fälle pro jahr
cpy <- cases %>% group_by(Jahr, Geschlecht) %>% summarize(count=length(unique(Name))) %>%
  group_by(Jahr) %>% summarize(count=sum(count))# %>% arrange(-count)
tmp <- athl %>% group_by(Jahr, Geschlecht) %>% summarize(count=length(unique(Name))) %>%
  group_by(Jahr) %>% summarize(sum = sum(count))
cpy <- left_join(cpy, tmp, by=c("Jahr")) %>% mutate(norm = count/sum)

write.csv(cpy, "fälleprojahr.csv", row.names = F)

#fälle pro land
cpc <- cases %>% group_by(COUNTRY, Land, Jahr) %>% filter(Jahr >= 1996) %>%
  summarize(count=length(unique(Name))) %>%
  group_by(COUNTRY, Land) %>% summarize(count=sum(count)) %>% arrange(-count)
tmp <- athl %>% group_by(Land, Jahr, Geschlecht) %>% filter(Jahr >= 1996) %>%
  summarize(count=length(unique(Name))) %>%
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
cpg <- cases %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>% filter(Jahr < 2012) %>%
  group_by(Geschlecht) %>% summarize(count=sum(count)) %>%arrange(-count)
tmp <- athl %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>% filter(Jahr < 2012) %>%
  #spread(Geschlecht, count) %>% mutate(wshare = Women/(Women+Men))
  group_by(Geschlecht) %>% summarize(sum=sum(count)) 
cpg <- left_join(cpg, tmp, by="Geschlecht") %>% mutate(norm = count/sum)

#fälle nach geschlecht und jahr
cpgy <- cases %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name))) %>%arrange(-count)
tmp <- athl %>% group_by(Geschlecht, Jahr) %>% summarize(sum=length(unique(Name)))
cpgy <- left_join(cpgy, tmp, by=c("Geschlecht","Jahr")) %>% mutate(norm = count/sum)
write.csv(cpg, "fällenachgeschlecht.csv", row.names = F)
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
ggplot(cpy, aes(x=Jahr, y=count)) + theme_light() + scale_x_continuous(breaks = rev(unique(cpgy$Jahr))) +
  ggtitle("Doping-Fälle in der Leichtathletik bei den olympischen Spielen") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Jahr, label=paste0(count)), vjust=-1)
'

#fälle pro jahr normiert
ggplot(cpy, aes(x=Jahr, y=norm)) + theme_light() + scale_x_continuous(breaks = rev(unique(cpgy$Jahr))) +
  ggtitle("Anteil überführter Athleten über die Jahre") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Jahr, label=paste0(round(norm*100, 2),"%")), vjust=-1) + scale_y_continuous(labels=scales::percent)
'

#tests pro jahr
#doping tests 
ggplot(tests, aes(x=Year, y=Number.of.tests)) + theme_light() + scale_x_continuous(breaks = rev(unique(tests$Year))) +
  ggtitle("Gesamtzahl an Doping-Tests bei den olympischen Spielen") + geom_line(colour="orange") + geom_point() +
  geom_text(aes(x=Year, label=paste0(Number.of.tests)), vjust=-1)

#fälle pro land
#sonstiges kategorie einrichten
cpc <- cpc %>% ungroup %>% arrange(-count)
x <- cpc %>% mutate(Land = c(cpc$Land[1:5], rep("Sonstige",length(cpc$Land)-5)))
x <- mutate(x, Land = factor(x$Land, levels=c("Türkei","Belarus","Ukraine","Russland","USA","Sonstige"))) %>%
  group_by(Land) %>% summarize(count=sum(count), sum=sum(sum), norm=count/sum)
#sonstiges summe und norm
tmp <- athl %>% group_by(Land, Jahr, Geschlecht) %>% filter(Jahr >= 1996) %>%
  summarize(count=length(unique(Name))) %>%
  group_by(Land) %>% summarize(sum = sum(count))
tmp2 = filter(tmp, Land != "TUR" & Land != "RUS" & Land != "BLR" & Land != "UKR" & Land != "USA")
x$sum[6] <- sum(tmp2$sum); x$norm[6] <- x$count[6] / x$sum[6]
#gesamtdurchschnitt
cmean = sum(cpc$count)/sum(tmp$sum) #0.00514601
ggplot(x, aes(x=Land, y=norm, fill=Land)) + theme_minimal()  + scale_y_continuous(labels=scales::percent) +
  geom_bar(stat="identity") + ggtitle("Anteil überführter Leichtathleten nach Land seit einschl. 1996") +
  geom_hline(yintercept = cmean) + theme(legend.position="none") +
  geom_text(aes(x=1, y=cmean, label=paste("Gesamtdurchschnitt =", paste0(round(cmean*100,2),"%"))), hjust=0,vjust=-0.5)+
  geom_text(position= position_dodge(width=0.9), aes(x=Land, label=paste0(count,"/",sum)), vjust=-0.5)

#fälle nach geschlecht
cpg <- cpg %>% ungroup %>% arrange(-norm)
ggplot(cpg, aes(x=Geschlecht, y=norm, fill=Geschlecht)) + theme_minimal() + scale_y_continuous(labels=scales::percent) +
  geom_bar(stat="identity") + ggtitle("Anteil überführter Leichtathleten nach Geschlecht seit 2004") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Geschlecht, label=paste0(count,"/",sum)), vjust=-0.5)
'
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
'
dev.off()


#32
athlwiki <- read.csv("LA-teilnehmer.csv", quote="", encoding="utf-8", sep=";")
wiki <- athlwiki %>% group_by(Land, Jahr, Geschlecht) %>% summarize(count=length(unique(Name))) %>%
  group_by(Land) %>% summarize(sum = sum(count))
cpc <- left_join(cpc, wiki, by=c("COUNTRY"="Land")) %>% arrange(-count)

ioc <- athl %>% group_by(Land, Jahr, Geschlecht) %>% summarize(count=length(unique(Name))) %>%
  group_by(Land) %>% summarize(sum = sum(count)) %>% arrange
sum(cpy$count)
x <- athl %>% group_by(Land, Jahr) %>% summarize(count = length(unique(Name)))
mean(x$count)
41/83

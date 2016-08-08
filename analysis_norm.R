setwd("/home/kira/ownCloud/BR Data/doping olympische spiele")
setwd("/Users/administrator/Desktop/doping olympische spiele")
library(dplyr)
library(ggplot2)
cases <- read.csv("IAAF-doping-cases-clean-deutsch.csv", stringsAsFactors = F, encoding="utf-8")
athl <- read.csv("LA-teilnehmer.csv", quote="", encoding="utf-8", sep=";")

#fälle pro land
cpc <- cases %>% group_by(COUNTRY, Land, Jahr) %>% summarize(count=length(unique(Name))) %>%
  group_by(COUNTRY, Land) %>% summarize(count=sum(count)) %>% arrange(-count)
tmp <- athl %>% group_by(Land, Jahr) %>% summarize(count=length(unique(Name))) %>%
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
write.csv(cpk, "fällenachgeschlecht.csv", row.names = F)
tmp <- athl %>% group_by(Geschlecht, Jahr) %>% summarize(count=length(unique(Name)))
save.image(file="datasets.Rdata")

##################
##visualisierung##
##################

library(ggplot2)
pdf("results/alle-normiert.pdf", width=14)

#fälle pro land
#sonstiges kategorie einrichten
cpc <- cpc %>% ungroup %>% arrange(-norm)
x <- cpc %>% mutate(Land = c(cpc$Land[1:5], rep("Sonstige",length(cpc$Land)-5)))
x <- mutate(x, Land = factor(x$Land, levels=unique(x$Land))) %>%
  group_by(Land) %>% summarize(count=sum(count), norm=mean(norm))
ggplot(x, aes(x=Land, y=norm, fill=Land)) + theme_minimal() +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Ländern") + theme(legend.position="none") +
  geom_text(position= position_dodge(width=0.9), aes(x=Land, label=paste(count,"Fälle")), vjust=-0.5)
#fälle pro disziplin
cpd <- cpd %>% ungroup %>% arrange(-norm)
cpd$Disziplin <- factor(cpd$Disziplin, levels=unique(cpd$Disziplin))
ggplot(cpd, aes(x=Disziplin, y=norm, fill=Disziplin)) +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Disziplin") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Disziplin, label=paste(count,"Fälle")), vjust=-0.5)
#fälle pro kategorie
cpk <- cpk %>% ungroup %>% arrange(-norm)
cpk$Kategorie <- factor(cpk$Kategorie, levels=unique(cpk$Kategorie))
ggplot(cpk, aes(x=Kategorie, y=norm, fill=Kategorie)) +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Kategorie") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Kategorie, label=paste(count,"Fälle")), vjust=-0.5)
#fälle nach geschlecht
cpg <- cpg %>% ungroup %>% arrange(-norm)
ggplot(cpg, aes(x=Geschlecht, y=norm, fill=Geschlecht)) +
  geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Geschlecht") +
  theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
  geom_text(position= position_dodge(width=0.9), aes(x=Geschlecht, label=paste(count,"Fälle")), vjust=-0.5)


dev.off()

tmp <- athl %>% group_by(Jahr, Geschlecht) %>% summarize(count= n())


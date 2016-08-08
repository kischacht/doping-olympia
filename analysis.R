setwd("/home/kira/ownCloud/BR Data/doping olympische spiele")
library(dplyr)
library(ggplot2)
cases <- read.csv("IAAF-doping-cases-clean-deutsch.csv", stringsAsFactors = F)
cases <- cases[,-c(5,7,10)]
names(cases)

#fälle pro jahr
cpy <- cases %>% group_by(Jahr) %>% summarize(count=length(unique(Name)))# %>% arrange(-count)
write.csv(cpy, "fälleprojahr.csv", row.names = F)
pdf("results/fälleprojahr.pdf", width=14)
ggplot(cpy, aes(x=Jahr, y=count)) + theme_light() + #scale_y_continuous(expand=c(0,0)) +
    ggtitle("Doping-Fälle über die Jahre") + geom_line(colour="orange")
dev.off()
?read.csv
#fälle pro land
cpc <- cases %>% group_by(Land) %>% summarize(count=length(unique(Name)))
cpc <- cpc %>% arrange(-count)
write.csv(cpc, "fälleproland.csv", row.names = F)
#sonstiges kategorie einrichten
cpc <- mutate(cpc,Land = c(cpc$Land[1:5], rep("Sonstige",length(cpc$Land)-5)))
cpc <- mutate(cpc, Land = factor(cpc$Land, levels=unique(cpc$Land))) %>%
    group_by(Land) %>% summarize(count=sum(count))
pdf("results/fälleproland.pdf", width=14)
ggplot(cpc, aes(x=Land, y=count, fill=Land)) + theme_minimal() +
    geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Ländern") + theme(legend.position="none") +
    geom_text(position= position_dodge(width=0.9), aes(x=Land, label=count), vjust=-0.5)
dev.off()

#fälle pro disziplin
cpd <- cases %>% group_by(Disziplin) %>% summarize(count=n()) %>% arrange(-count)
cpd$Disziplin <- factor(cpd$Disziplin, levels=unique(cpd$Disziplin))
write.csv(cpd, "fälleprodisziplin.csv", row.names = F)
pdf("results/fälleprodisziplin.pdf", width=14)
ggplot(cpd, aes(x=Disziplin, y=count, fill=Disziplin)) +
    geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Disziplin") +
    theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
    geom_text(position= position_dodge(width=0.9), aes(x=Disziplin, label=count), vjust=-0.5)
dev.off()

#fälle pro kategorie
cpk <- cases %>% group_by(Kategorie) %>% summarize(count=length(unique(Name))) %>% arrange(-count)
cpk$Kategorie <- factor(cpk$Kategorie, levels=unique(cpk$Kategorie))
write.csv(cpk, "fälleprokategorie.csv", row.names = F)
pdf("results/fälleprokategorie.pdf", width=14)
ggplot(cpk, aes(x=Kategorie, y=count, fill=Kategorie)) +
    geom_bar(stat="identity") + ggtitle("Doping-Fälle nach Kategorie") +
    theme(legend.position="none", axis.text.x = element_text(angle=45,hjust=1), panel.background = element_rect(fill="white")) +
    geom_text(position= position_dodge(width=0.9), aes(x=Kategorie, label=count), vjust=-0.5)
dev.off()

save.image(file="datasets.Rdata")


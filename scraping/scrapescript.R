setwd("/Users/administrator/Desktop/doping olympische spiele/scraping")
#install.packages("rvest")
library(rvest)
library(dplyr)

athlinks <- read_html("https://www.olympic.org/olympic-games") %>% html_nodes("li.summergames") %>%
    html_nodes("a") %>% html_attr("href")
athlinks <- paste0("https://www.olympic.org", athlinks,"/athletics")[-(1:2)]
#nacheinander athlinks lesen. für jedes jahr ein listenelement mit namen des jahres speichern
years <- list()
for(i in 1:4){
    #liste der links zu den disziplinen
    disclinks <- read_html(athlinks[i]) %>% html_nodes("a") %>%
        html_attr("href") %>% subset(grepl(pattern='^/.*/athletics/', x=.)) %>% unique
    #für jedes jahr eine liste in der liste machen, die alle tabellen der disziplinen enthält
    discs <- list()
    for(j in 1:length(disclinks)){
        print(disclinks[j])
        discs[[j]] <- read_html(paste0("https://www.olympic.org", disclinks[j])) %>%
            html_nodes("table") %>% html_table(fill=T)
        names(discs)[j] <- disclinks[j]
    }
    years[[i]] <- discs
    names(years)[i] <- athlinks[i]
}
#restliche links von wiki scrapen
wikilinks <- paste0("https://en.wikipedia.org/wiki/Athletics_at_the_",c(1996, 1992, 1988, 1984, 1976),"_Summer_Olympics")
yearswiki <- list()
for(i in 1:length(wikilinks)){
  disclinks <- read_html(wikilinks[i]) %>% html_nodes("a") %>%
    html_attr("href") %>% subset(grepl(pattern='/wiki/Athletics_at_the_.*_Summer_Olympics_', x=.)) %>% unique
  discs <- list()
  for(j in 1:length(disclinks)){
    print(disclinks[j])
    discs[[j]] <- read_html(paste0("https://en.wikipedia.org", disclinks[j]))
    discs[[j]] <- discs[[j]] %>% html_nodes("table")
    try(discs[[j]] <- discs[[j]] %>% html_table(fill=T))
    names(discs)[j] <- disclinks[j]
  }
  yearswiki[[i]] <- discs
  names(yearswiki)[i] <- wikilinks[i]
}
#"/wiki/Athletics_at_the_1992_Summer_Olympics_%E2%80%93_Men%27s_shot_put" Error in out[j + k, ] : Indizierung außerhalb der Grenzen
#Fehler manuell beheben:
yearswiki[[2]]$`/wiki/Athletics_at_the_1992_Summer_Olympics_%E2%80%93_Men%27s_shot_put` <- html_table(yearswiki[[2]]$`/wiki/Athletics_at_the_1992_Summer_Olympics_%E2%80%93_Men%27s_shot_put`[5:7], fill=T)
rm(i,j, discs, disclinks)
save.image("data.Rdata")


##saubermachen###
rm(list = ls())

###IOC seite: 2000-2012
load("data.Rdata")
#remove NA columns
#years = years[c(1,2,3,4)]
str(x[[1]], max.level = 1)
View(years[[1]][[1]])
years <- lapply(years, function(year) lapply(year, function(discdata) lapply(discdata, function(i) i <- i[,1:4])))
years <- lapply(years, function(year) lapply(year, function(d) lapply(d, function(discdata) setNames(discdata, c("Rank","Athlete","Result","Notes")))))
years <- lapply(years, function(year) lapply(year, function(discdata) do.call(rbind, discdata)))
years <- lapply(years, function(year) do.call(rbind, year))
years <- lapply(years, function(year) mutate(year, Game = row.names(year)))
years <- do.call(rbind, years)
years <- years[-grep("^Table|^Notes$", years$Notes),]
row.names(years) <- 1:nrow(years)
years <- years[,-(3:4)]
years$Rank <- ifelse(years$Rank == "G" | years$Rank == "S"| years$Rank == "B", years$Rank, "")
years$City <- substr(years$Game, 2, regexpr("-", years$Game)-1)
years$year <- substr(years$Game, regexpr("-", years$Game)+1, regexpr("/athl", years$Game)-1)
years$Discipline <- substr(years$Game, regexpr("cs/", years$Game)+3, regexpr("\\.", years$Game)-1)
years <- years[,-3]


##wiki: vor 2000##

#relevante datensätze heraussuchen
cols <- lapply(yearswiki, function(year){
  lapply(year, function(disc){
    sapply(disc, function(i)( any( grepl("Rank|RANK|Athlete|Atltete", names(i)) ) ) )
    })
  })
for(i in 1:length(yearswiki)){
  year <- yearswiki[[i]]
  print(names(yearswiki)[i])
  for(j in 1:length(year)){ #length(year) = anzahl disziplinen
    discipline <- year[[j]]
    year[[j]] <- discipline[cols[[i]][[j]]]
  }
  yearswiki[[i]] <- year
}
#namen vereinheitlichen
yearswiki <- lapply(yearswiki, function(year) lapply(year, function(disc) lapply(disc, function(i) setNames(i, 1:ncol(i)))))
#alles auf 4 spalten herunterbrechen
yearswiki <- lapply(yearswiki, function(year) lapply(year, function(disc) lapply(disc, function(i) i <- i[1:min(ncol(i),4)] )))
#bis auf ausnahmen auf 3 herunter
for(i in 1:length(yearswiki)){
  disciplines <- yearswiki[[i]]
  print(names(yearswiki)[i])
  for(j in 1:length(disciplines)){ #length(year) = anzahl disziplinen
    if(names(disciplines)[j] != "/wiki/Athletics_at_the_1988_Summer_Olympics_%E2%80%93_Men%27s_110_metres_hurdles" &
       names(disciplines)[j] != "/wiki/Athletics_at_the_1992_Summer_Olympics_%E2%80%93_Women%27s_100_metres" &
       names(disciplines)[j] != "/wiki/Athletics_at_the_1988_Summer_Olympics_%E2%80%93_Men%27s_200_metres" &
       names(disciplines)[j] != "/wiki/Athletics_at_the_1988_Summer_Olympics_%E2%80%93_Men%27s_1500_metres" &
       names(disciplines)[j] != "/wiki/Athletics_at_the_1988_Summer_Olympics_%E2%80%93_Women%27s_100_metres_hurdles" &
       names(disciplines)[j] != "/wiki/Athletics_at_the_1988_Summer_Olympics_%E2%80%93_Men%27s_110_metres_hurdles"){
      disciplines[[j]] <- lapply(disciplines[[j]], function(i) i <- i[1:3] )
    }
    else disciplines[[j]] <- disciplines[[j]][length(disciplines[[j]])]
  }
  yearswiki[[i]] <- disciplines
}
#disziplinen zusammenbinden
yearswiki <- lapply(yearswiki, function(year) lapply(year, function(disc) do.call(rbind, disc)))
#vierte spalte mit dritter mergen
for(i in 1:length(yearswiki)){
  disciplines <- yearswiki[[i]]
  print(names(yearswiki)[i])
  for(j in 1:length(disciplines)){ # anzahl disziplinen
    if(length(disciplines[[j]]) == 4){
      disciplines[[j]]
      disciplines[[j]][,3] = paste0(disciplines[[j]][,3], " (", disciplines[[j]][,4], ")")
      disciplines[[j]] <- disciplines[[j]][-4]
    }
  }
  yearswiki[[i]] <- disciplines
}
#doppelte entfernen

#jahre zusammenbinden
yearswiki <- lapply(yearswiki, function(year) do.call(rbind, year))
yearswiki <- do.call(rbind, yearswiki)
yearswiki$Game <- row.names(yearswiki)


save.image("data.Rdata")
write.csv(years, "IOC-teilnehmer.csv", row.names = F)
write.csv(yearswiki, "wiki-teilnehmer.csv", row.names = F)

# libs
install.packages("htmltab", "dplyr", "ggplot2")
library(htmltab)
library(dplyr)
library(ggplot2)
library(scales)

# load dataset https://en.wikipedia.org/wiki/Deloitte_Football_Money_League
for (i in c(2,4,7,9,11,13,15)) {
  temp <- paste("tbl",i,sep = "")
  table <- htmltab("http://en.wikipedia.org/wiki/Deloitte_Football_Money_League", i)
  table <- table[,c(1:4)]
  table$yearDFML <- gsub(".*([0-9]{4}).*","\\1", colnames(table)[1])
  assign(temp, table)
}

# merge tables
# df <- merge(x = tbl1, y = tbl3, by = "Club", all = TRUE)
df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(tbl2, tbl4, tbl7, tbl9,tbl11,tbl13,tbl15))

# rename columns
colnames(df)[5:6] <- c("Rank in2019", "Rank in2018")
colnames(df)[2] <- c("Revenue")

# data cleanup
df$`Revenue` <- as.numeric(as.character(df$`Revenue`))   
df$`Rank in2013` <- as.numeric(as.character(df$`Rank in2013`))  
df$`Rank in2014` <- as.numeric(as.character(df$`Rank in2014`))  
df$`Rank in2015` <- as.numeric(as.character(df$`Rank in2015`))  
df$`Rank in2016` <- as.numeric(as.character(df$`Rank in2016`))  
df$`Rank in2017` <- as.numeric(as.character(df$`Rank in2017`))  
df$`Rank in2018` <- as.numeric(as.character(df$`Rank in2018`))  
df$`Rank in2019` <- as.numeric(as.character(df$`Rank in2019`))
df$rank <- rowSums(df[,5:11], na.rm = T)

# query
# df[df$Club == "Arsenal",]

# viz

# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

allVisualize <- function() {
  ggplot(df, 
         aes(x = yearDFML, y = rank, group = Club)) + 
    geom_line()  +
    geom_point()  +
    facet_wrap(~Club) +
    labs(title = paste("Deloitte Football Money League - Ranking overview", sep = ""), 
         fill = "Ranking\n") +
    scale_y_continuous(breaks= pretty_breaks(), trans = "reverse")
}

yearTop8Visualize <- function(year) {
  clubList <- filter(df, year == year & rank < 7)[,1]
  ggplot(df[df$Club %in% clubList,], 
         aes(x = yearDFML, y = rank, color = Club, group = Club)) + 
    geom_line() +
    scale_colour_manual(values=cbPalette) +
    scale_y_continuous(breaks= pretty_breaks(), trans = "reverse")
}

twoTeamsVisualize <- function(team1, team2) {
  ggplot(df[df$Club %in% c(team1, team2),], 
         aes(x = yearDFML, y = rank, color = Club, group = Club)) + 
    geom_line() +
    scale_y_continuous(breaks= pretty_breaks(), trans = "reverse") +
    scale_colour_manual(values=cbPalette) 
}

teamsVisualize <- function(team) {
  ggplot(df[df$Club %in% c(team),], 
         aes(x = yearDFML, y = Revenue, fill = as.factor(rank))) + 
    geom_col() +
    scale_y_continuous(breaks= pretty_breaks()) +
    labs(title = paste("Deloitte Football Money League - Revenue: ",team, sep = ""), 
         fill = "Ranking\n") +
    scale_colour_manual(values=cbPalette) 
}

allVisualize()
yearTop8Visualize("2019")
twoTeamsVisualize("Arsenal", "Tottenham Hotspur")
teamsVisualize("Arsenal")
teamsVisualize("Tottenham Hotspur")







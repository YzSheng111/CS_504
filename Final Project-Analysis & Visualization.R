#Load dataset
Main_Data <- read.csv("C:/Users/sadat/Desktop/CS-504/Data/Main.Data.csv", stringsAsFactors=FALSE, h=T)

#Summary of Data
summary(Main_Data)

View(Main_Data)

#Load packages

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(ggplot2)
library(data.table)
library(ggmap)
library(ggrepel)
library(igraph)
library(networkD3)
library(corrplot)
require(plotly) #Interactive plots
library(RColorBrewer) 
library(viridis) #Color scales
require(tm) #Textmining for wordclouds
library(wordcloud) #Generation of wordclouds
library(leaflet) #Interactive maps
require(maps) #Maps generating

leaflet_map <- function(data, map_obj){
  
#Merge map and data
  map_obj$dataf <- left_join(data.frame(names = world$names, stringsAsFactors = F), 
                             data, 
                             by = c("names" = names(data)[2]))
  
  #Creating viridis numerical color palette
  pal <- if(is.numeric(map_obj$dataf[[3]])){
    colorNumeric("YlOrRd", 
                 domain = map_obj$dataf[[3]], na.color = "white")
  }else{
    colorFactor(brewer.pal(12, "Paired"), 
                domain = map_obj$dataf[[3]], na.color = "white")
  }
  
  #Generating label texts
  strings <- if(ncol(map_obj$dataf) == 3){
    
    #For a df with 3 columns (Region, Country, Variable)
    sprintf(
      paste("<strong>%s</strong><br/><strong>%s</strong><br/>", 
            names(map_obj$dataf)[3], 
            ifelse(is.numeric(map_obj$dataf[[3]]), ": %g ", ": %s ")),
      map_obj$dataf[[1]], map_obj$dataf[[2]], map_obj$dataf[[3]])
    
  }else if(ncol(map_obj$dataf) == 4){
    
    #For a df with 4 columns (Region, Country, Color_Variable, Label_Variable)
    sprintf(
      paste("<strong>%s</strong><br/><strong>%s</strong><br/>", 
            names(map_obj$dataf)[3], 
            ifelse(is.numeric(map_obj$dataf[[3]]), ": %g ", ": %s "), 
            paste("<br/>", 
                  names(map_obj$dataf)[4], 
                  ifelse(is.numeric(map_obj$dataf[[4]]), ": %g ", ": %s "))),
      map_obj$dataf[[1]], map_obj$dataf[[2]], map_obj$dataf[[3]], map_obj$dataf[[4]])
    
  }else{
    
    #For a df with 4 columns (Region, Country, Color_Variable, Label_Variable, Label_Variable)
    sprintf(
      paste("<strong>%s</strong><br/><strong>%s</strong><br/>", 
            names(map_obj$dataf)[3], 
            ifelse(is.numeric(map_obj$dataf[[3]]), ": %g ", ": %s "), 
            paste("<br/>", 
                  names(map_obj$dataf)[4], 
                  ifelse(is.numeric(map_obj$dataf[[4]]), ": %g ", ": %s ")),
            paste("<br/>", 
                  names(map_obj$dataf)[5], 
                  ifelse(is.numeric(map_obj$dataf[[5]]), ": %g ", ": %s "))),
      map_obj$dataf[[1]], map_obj$dataf[[2]], map_obj$dataf[[3]], map_obj$dataf[[4]], map_obj$dataf[[5]])
    
  }
  
  labels <- strings %>% lapply(htmltools::HTML)
  
  #Creating a leaflet basic map
  m <- leaflet(map_obj) %>% addTiles()
  
  #Adding polygon with the variable
  m <- m %>% addPolygons(
    fillColor = ~pal(dataf[[3]]),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE, stroke = 1),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"))
  
  #Adding legends to the plot
  m <- m %>%
    addLegend("bottomright", pal = pal, values = ~dataf[[3]],
              title = names(map_obj$dataf)[3],
              opacity = 1)
  
  return(m)
}
library(countrycode) #Matching country names

#Getting world map polygons
  World <- function (map, region = ".", exact = FALSE, ...) 
  {
    require("maps")
    fortify(map("world", plot = FALSE, fill = TRUE, 
                ...))
  } 

# Analyze Terrorist attacks over the years

Attacks.year <- Main_Data %>%
  count(year) %>%
  mutate(Year = year, Frequency = n) %>%
  ggplot(aes(Year, Frequency))+
  geom_line(color = "firebrick2")+
  geom_point(color = "firebrick2")+
  labs(title = "Frequency of terrorist atacks over the years") +
  theme(legend.position = "none")

ggplotly(Attacks.year)

# Terrorism in the US

leaflet(Main_Data) %>%
  addTiles() %>%
  addMarkers(lng=Main_Data$longitude,lat=Main_Data$latitude, clusterOptions = markerClusterOptions(),
             popup= paste("<strong>Date: </strong>", Main_Data$day,"/",Main_Data$month,"/", Main_Data$year,
                          "<br><br><strong>Place: </strong>", Main_Data$city,"-",Main_Data$State,
                          "<br><strong>Killed: </strong>", Main_Data$nkill,
                          "<br><strong>Wounded: </strong>", Main_Data$nwound
             ))

leaflet(Main_Data) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png') %>%
  addCircles(color="#ffa500")

# Terrorist attacks by City

City <- Main_Data%>%
  count(year, State, city) %>%
  mutate(Year = year, City = city, State = reorder(State, n, sum), Frequency = n) %>%
  ggplot(aes(Year, Frequency))+
  geom_line(aes(color = City))+
  facet_wrap("State")+
  labs(title = "Frequency of terrorist attacks by City in each State") +
  theme(legend.position = "none", text = element_text(size = 8))

ggplotly(City)

#Obtaining frequencies by State and merging with the map

US <- Main_Data%>% 
  count(State, city, attacktype1_txt) %>% 
  mutate(State= State, City = city, Type = attacktype1_txt) %>%
  group_by(State, City) %>%
  mutate(Events = n, Proportion = n/sum(n)) %>%
  filter(Proportion == max(Proportion)) %>%
  arrange(State, Type) %>%
  select(State, City, Type, Events, Proportion)

#Removing ties
dup_cols <- US[c("State" ,"City")]
US <- US[!duplicated(dup_cols),]


#Ploting the map
leaflet_map(US, World)


#Perpetrators'terrorist involvment
Main_Data$Terror <-ifelse(Main_Data$Dominant.Ideology==1, "Extreme Right Wing",
                ifelse(Main_Data$Dominant.Ideology==2, "Extreme Left Wing",
                       ifelse(Main_Data$Dominant.Ideology==3,"Religious", 
                              ifelse(Main_Data$Dominant.Ideology==4, "Ethno-nationalist/Separatist",
ifelse(Main_Data$Dominant.Ideology==5,"Single Issue", "Unknown")))))

Main_Data$Regions <- ifelse(
  Main_Data$State %in% c("Connecticut", "Maine","Massachusetts", "New Hampshire", "Rhode Island", "Vermont"),
  "New England",
  ifelse(
    Main_Data$State %in% c("Delaware", "Maryland", "New Jersey", "New York","Pennsylvania","District of Columbia"
    ),
    "Mid-Atlantic",
    ifelse(
      Main_Data$State %in% c("Alabama", "Arkansas","Florida","Georgia","Kentucky","Louisiana", "Mississippi","North Carolina",
                       "South Carolina", "Tennessee", "Virginia", "West Virginia"
      ),
      "South",
      ifelse(
        Main_Data$State %in% c("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota", 
                         "Missouri","Nebraska","North Dakota","Ohio" ,"South Dakota", "Wisconsin"
        ),
        "Midwest",
        ifelse(
          Main_Data$State %in% c("Arizona","New Mexico","Oklahoma","Texas "),
          "Southwest",
          "West")))))


# Link group terrorists to regions with highest attacks

terror.org<-Main_Data%>%filter(gname!="Unknown")%>%group_by(Terror,Regions)%>%summarise(attacks=sum(success))

terror.top<-terror.org%>%ungroup()%>%group_by(Regions)%>%mutate(rnk=rank(desc(attacks)))%>%filter(rnk==1)

terror.state<-Main_Data%>%inner_join(terror.top,by=c("Terror","Regions"))%>%select(one_of("Terror","Regions","State","success"))%>%group_by(Terror,State,Regions)%>%summarise(attacks=sum(success))
View(terror.state)
links<-terror.state%>%select(one_of("from","to","attacks","region"))%>%rename(from=Terror,to=State,weight=attacks)

nodes <-
  rbind(
    data.frame(
      name = unique(terror.state$Terror),
      type = 1,
      size = 100
    ),
    data.frame(
      name = unique(terror.state$State)[terror.state$Regions ==
                                                  "New England"],
      type = 2,
      size = terror.state$attacks[terror.state$Regions ==
                                        "New England"]
    ),
    data.frame(
      name = unique(terror.state$State)[terror.state$Regions ==
                                                  "Mid-Atlantic"],
      type = 3,
      size = terror.state$attacks[terror.state$Regions ==
                                        "Mid-Atlantic"]
      
    ),
    data.frame(
      name = unique(terror.state$State)[terror.state$Regions ==
                                                  "South"],
      type = 4,
      size = terror.state$attacks[terror.state$Regions ==
                                        "South"]
    ),
    
    data.frame(
      name = unique(terror.state$State)[terror.state$Regions ==
                                                  "Midwest"],
      type = 5,
      size = terror.state$attacks[terror.state$Regions ==
                                        "Midwest"] ),
    data.frame(
      name = unique(terror.state$State)[terror.state$Regions ==
                                                  "Southwest"],
      type = 6,
      size = terror.state$attacks[terror.state$Regions ==
                                        "Southwest"]),
      data.frame(
        name = unique(terror.state$State)[terror.state$Regions ==
                                            "West"],
        type = 7,
        size = terror.state$attacks[terror.state$Regions ==
                                          "West"])
  )
library(Hmisc)

net<-graph.data.frame(links,nodes,directed = T)
net <- simplify(net,remove.multiple = F)
colrs <- c("darkgoldenrod", "tomato","darkolivegreen3","darkorange","burlywood4","firebrick")
V(net)$color<-colrs[V(net)$type]

E(net)$width <- 1+E(net)$weight/12
{r fig.height=10, fig.width=12}



set.seed(1492) 

l <- layout.fruchterman.reingold(net, niter=5000, area=vcount(net)^10*10)

plot(net,  layout=l,
     edge.arrow.size=.6, 
     vertex.label.cex=0.75, 
     vertex.label.family="Helvetica",
     vertex.label.font=2,
     vertex.shape="circle", 
     vertex.size=30, 
     vertex.label.color="black", 
     edge.curved=.1)

legend(x=-1.5, y=-1.1, c("Extreme Right Wing","Extreme Left Wing","Religious", "Ethno-nationalist/Separacist","Single Issue","Unknown"), pch=21,
       col="#777777", pt.bg=colrs, pt.cex=2, cex=.8, bty="n", ncol=1)

links.target <-
  Main_Data %>% group_by(Terror, targtype1_txt) %>% summarise(weight =
              sum(success)) %>% arrange(Terror) %>% rename(from = Terror, to = targtype1_txt)

nodes.target <-
  rbind(data.frame(
    name = unique(Main_Data$Terror),
    type = 1
  ),
  data.frame(
    name =
      unique(Main_Data$targtype1_txt),
    type = 2
  ))

links.target$from <- match(links.target$from, nodes.target$name) - 1
links.target$to <- match(links.target$to, nodes.target$name) - 1

require(d3Network)
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "from",
  Target = "to",
  Value = "weight",
  NodeID = "name",
  nodeWidth = 50,
  fontSize = 14
)

#Perpetrators' motives

Motives <- Main_Data$motive[Main_Data$motive != ""] #Filters all filled motives

Motives <- iconv(Motives,"WINDOWS-1252","UTF-8")

words <- Motives %>% as.character() %>%
  removePunctuation() %>% #Removes punctuation
  tolower() %>% #Converts all characters to lower case
  removeWords(stopwords()) %>% #Remove english stop words
  str_split(pattern = " ") %>% #Splits all lines into lists of words
  unlist() #converts all lists into a single concatenated vector

wordcloud(words,max.words = 120, colors = c("gold","chocolate","darkorange", 
                                            "firebrick3","red"), random.order = FALSE)

#Targets Type

wordcloud(Main_Data$targtype1_txt,
          max.words = 100,colors = c("blue","purple" 
                                  ,"firebrick3","red"),random.order = FALSE)


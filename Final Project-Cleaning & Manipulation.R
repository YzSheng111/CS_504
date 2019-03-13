# Load PPT data

PPT <- read.csv("C:/Users/sadat/Desktop/CS-504/Data/Perpetrators US.csv", stringsAsFactors=FALSE, h=T)

# View Data
View(PPT)

# LOad packages
library(dplyr)


#=====================================================Data Cleaning========================================================================="

# Select and assign useful variables of PPT to a new data table
PPT1 <- select(PPT, c("ORGNAME","GATK_FIRST_YEAR", "GATK_LAST_YEAR", "LOC_USATK_STATE_1", "LOC_USATK_STATE_2","LOC_USATK_STATE_3", 
                      "LOC_USATK_STATE_4", "LOC_USATK_STATE_5", "LOC_USAHQ_STATE_1", "LOC_USAHQ_STATE_2",
                      "LOC_USAHQ_STATE_3", "LOC_USAHQ_STATE_4", "LOC_USAHQ_STATE_5", "DOM_I","STRUC_1","STRUC_2",
                      "STRUC_3","STRUC_4","STRUC_OTH", "REC_1", "REC_2", "REC_3", "REC_4",
                      "REC_5", "REC_OTH", "MREC_1", "MREC_2"))

# Remove NAs from specific columns
is.na((PPT1$LOC_USATK_STATE_1))
PPT1$LOC_USATK_STATE_1[is.na(PPT1$LOC_USATK_STATE_1)] <- 0

is.na((PPT1$LOC_USATK_STATE_2))
PPT1$LOC_USATK_STATE_2[is.na(PPT1$LOC_USATK_STATE_2)] <- 0

is.na((PPT1$LOC_USATK_STATE_3))
PPT1$LOC_USATK_STATE_3[is.na(PPT1$LOC_USATK_STATE_3)] <- 0

is.na((PPT1$LOC_USATK_STATE_4))
PPT1$LOC_USATK_STATE_4[is.na(PPT1$LOC_USATK_STATE_4)] <- 0

is.na((PPT1$LOC_USATK_STATE_5))
PPT1$LOC_USATK_STATE_5[is.na(PPT1$LOC_USATK_STATE_5)] <- 0

is.na((PPT1$LOC_USAHQ_STATE_1))
PPT1$LOC_USAHQ_STATE_1[is.na(PPT1$LOC_USAHQ_STATE_1)] <- 0

is.na((PPT1$LOC_USAHQ_STATE_2))
PPT1$LOC_USAHQ_STATE_2[is.na(PPT1$LOC_USAHQ_STATE_2)] <- 0

is.na((PPT1$LOC_USAHQ_STATE_3))
PPT1$LOC_USAHQ_STATE_3[is.na(PPT1$LOC_USAHQ_STATE_3)] <- 0

is.na((PPT1$LOC_USAHQ_STATE_4))
PPT1$LOC_USAHQ_STATE_4[is.na(PPT1$LOC_USAHQ_STATE_4)] <- 0

is.na((PPT1$LOC_USAHQ_STATE_5))
PPT1$LOC_USAHQ_STATE_5[is.na(PPT1$LOC_USAHQ_STATE_5)] <- 0

View(PPT1)

#Rename columns

colnames(PPT1)[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27)] <- c("gname","year.first.atk", "year.last.atk", "Attack location 1", "Attack location 2", "Attack location 3",
                                              "Attack location 4","Attack location 5", "orgheadquarter1", "orgheadquarter2",
                                              "orgheadquarter3", "orgheadquarter4","orgheadquarter5","Dominant Ideology", "hierarchical", "Network",
                                              "Umbrella","Movement", "Other","The net", "The funnel", "The infection", "The seed crystal",
                                              "Self recruitment", "Others","Direct", "Indirect")

#Assign Recruitment modes to organizations

PPT1$Thenet<- ifelse(PPT1$'The net'==1, "The Net", ifelse(PPT1$'The net'== -99, "Unknown",""))
PPT1$Thefunnel<- ifelse(PPT1$'The funnel'==1, "The funnel", ifelse(PPT1$'The funnel'== -99, "Unknown",""))                                              
PPT1$Theinfection<- ifelse(PPT1$'The infection'==1, "The infection", ifelse(PPT1$'The infection'== -99, "Unknown",""))
PPT1$Thesecrystal<- ifelse(PPT1$'The seed crystal'==1, "The seed crystal", ifelse(PPT1$'The seed crystal'== -99, "Unknown",""))
PPT1$Selfrecrut<- ifelse(PPT1$'Self recruitment'==1, "Self recruitment", ifelse(PPT1$'Self recruitment'== -99, "Unknown",""))



install.packages(reshape2)
library(reshape2)

#Reshape dataset frokm wide to long format

PPT1_long <- reshape(PPT1,
                     varying = list(c("Attack location 1", "Attack location 2", "Attack location 3",
                                      "Attack location 4","Attack location 5"), c( "orgheadquarter1", "orgheadquarter2",
                                        "orgheadquarter3", "orgheadquarter4","orgheadquarter5"), c("hierarchical", "Network",
                                        "Umbrella","Movement", "Other"), c("Thenet", "Thefunnel", "Theinfection", "Thesecrystal",
                                        "Selfrecrut")),                   
                     direction = "long", 
                     v.names = c("Attacks Location", "OrganizationHeadquarters", "Organizational Structure",
                                 "Recruitment Strategies"),
                     timevar = "Outcome"
                     )
View(PPT1_long)                     

#Remove columns
PPT1_long <- subset(PPT1_long, select=-Others)
PPT1_long <- subset(PPT1_long, select=-id)
PPT1_long <- subset(PPT1_long, select=-Outcome)
PPT1_long <- subset(PPT1_long, select=-year.first.atk)
PPT1_long <- subset(PPT1_long, select=-year.last.atk)
PPT1_long <- PPT1_long[,-(3:7)]  
#Assign Names to Location

PPT1_long$OrgHeadquaters<-  ifelse(PPT1_long$OrganizationHeadquarters==1,"Alabama",ifelse(PPT1_long$OrganizationHeadquarters == c(2,3,4,5), c("Alaska","Arizona",
                            "Arkansas","California"),                                                     
                            ifelse(PPT1_long$OrganizationHeadquarters==6,"Colorado", 
                            ifelse(PPT1_long$OrganizationHeadquarters==7,"Connecticut",
                            ifelse(PPT1_long$OrganizationHeadquarters==8,"Delaware", 
                            ifelse(PPT1_long$OrganizationHeadquarters==9,"District of Columbia", 
                            ifelse(PPT1_long$OrganizationHeadquarters==10,"Florida",
                            ifelse(PPT1_long$OrganizationHeadquarters==11,"Georgia", 
                            ifelse(PPT1_long$OrganizationHeadquarters==12,"Guam", 
                            ifelse(PPT1_long$OrganizationHeadquarters==13, "Hawaii", 
                            ifelse(PPT1_long$OrganizationHeadquarters==14, "Idaho",
                            if_else(PPT1_long$OrganizationHeadquarters==15,"Illinois", 
                            ifelse(PPT1_long$OrganizationHeadquarters==16,"Indiana", 
                            ifelse(PPT1_long$OrganizationHeadquarters==17,"Iowa", 
                            ifelse(PPT1_long$OrganizationHeadquarters==18,"Kansas",
                            ifelse(PPT1_long$OrganizationHeadquarters==19,"Kentucky", 
                            ifelse(PPT1_long$OrganizationHeadquarters==20,"Louisiana",
                            ifelse(PPT1_long$OrganizationHeadquarters==21,"Maine", 
                            ifelse(PPT1_long$OrganizationHeadquarters==22,"Maryland",
                            ifelse(PPT1_long$OrganizationHeadquarters==23,"Massachusetts", 
                            ifelse(PPT1_long$OrganizationHeadquarters==24,"Michigan", 
                            ifelse(PPT1_long$OrganizationHeadquarters==25,"Minnesota", 
                            ifelse(PPT1_long$OrganizationHeadquarters==26,"Mississippi",
                            ifelse(PPT1_long$OrganizationHeadquarters==27,"Missouri",
                            ifelse(PPT1_long$OrganizationHeadquarters==28,"Montana", 
                            ifelse(PPT1_long$OrganizationHeadquarters==29,"Nebraska",
                            ifelse(PPT1_long$OrganizationHeadquarters==30,"Nevada",
                            ifelse(PPT1_long$OrganizationHeadquarters==31,"New Hampshire",
                            ifelse(PPT1_long$OrganizationHeadquarters==32,"New Jersey", 
                            ifelse(PPT1_long$OrganizationHeadquarters==33,"New Mexico",
                            ifelse(PPT1_long$OrganizationHeadquarters==34,"New York",
                            ifelse(PPT1_long$OrganizationHeadquarters==35,"North Carolina", 
                            ifelse(PPT1_long$OrganizationHeadquarters==36,"North Dakota", 
                            ifelse(PPT1_long$OrganizationHeadquarters==37,"Ohio",
                            ifelse(PPT1_long$OrganizationHeadquarters==38,"Oklahoma",ifelse(PPT1_long$OrganizationHeadquarters==39,"Oregon",
                            ifelse(PPT1_long$OrganizationHeadquarters==41,"Pennsylvania",
                            ifelse(PPT1_long$OrganizationHeadquarters==42,"Puerto Rico", 
                            ifelse(PPT1_long$OrganizationHeadquarters==43,"Rhode Island",
                            ifelse(PPT1_long$OrganizationHeadquarters==44,"South Carolina", 
                            ifelse(PPT1_long$OrganizationHeadquarters==45,"South Dakota", 
                            ifelse(PPT1_long$OrganizationHeadquarters==46,"Tennessee", 
                            ifelse(PPT1_long$OrganizationHeadquarters==47,"Texas",
                            ifelse(PPT1_long$OrganizationHeadquarters==48,"Utah", 
                            ifelse(PPT1_long$OrganizationHeadquarters==49,"Vermont", 
                            ifelse(PPT1_long$OrganizationHeadquarters==50,"Virginia",
                            ifelse(PPT1_long$OrganizationHeadquarters==51,"Washington",
                            ifelse(PPT1_long$OrganizationHeadquarters==52,"West Virginia", 
                            ifelse(PPT1_long$OrganizationHeadquarters==53,"Wisconsin",ifelse(PPT1_long$OrganizationHeadquarters==54,"Wyoming","Unknown"))))))))))))))))))))))))))))))))))))))))))))))))))

PPT.F<- PPT1_long  %>% 
  group_by(gname)

#Reorder Columns                         

PPT.F<- PPT.F[, c(1,5:8,2:4,9)]
View(PPT.F)

#Load GTD dataset

GTD <- read.csv("C:/Users/sadat/Desktop/CS-504/Data/GTD_US.csv", stringsAsFactors=FALSE, h=T)

#Delete columns

GTD <- subset(GTD, select=-country)
GTD <- subset(GTD, select=-country_txt)
GTD <- subset(GTD, select=-region)
GTD <- subset(GTD, select=-gsubname)
GTD <- subset(GTD, select=-gname2)
GTD <- subset(GTD, select=-gsubname2)
GTD <- subset(GTD, select=-gname3)
GTD <- subset(GTD, select=-gsubname3)
GTD <- subset(GTD, select=-X)
GTD <- subset(GTD, select=-eventid)
GTD <- subset(GTD, select=-natlty1)
GTD <- subset(GTD, select=-natlty2)
GTD <- subset(GTD, select=-natlty3)
GTD <- subset(GTD, select=-scite1)
GTD <- subset(GTD, select=-scite2)
GTD <- subset(GTD, select=-scite3)
GTD <- subset(GTD, select=-dbsource)
GTD <- subset(GTD, select=-INT_LOG)
GTD <- subset(GTD, select=-INT_IDEO)
GTD <- subset(GTD, select=-related)
GTD <- subset(GTD, select=-INT_MISC)
GTD <- subset(GTD, select=-INT_ANY)

#Code provstate with numeric variables to match states codes in PPT.F

GTD$AttacksLocation<-  ifelse(GTD$provstate=="Alabama",1,ifelse(GTD$provstate==c("Alaska","Arizona",
                           "Arkansas","California"),c(2,3,4,5),                                                     
                       ifelse(GTD$provstate=="Colorado",6, 
                              ifelse(GTD$provstate=="Connecticut",7,
            ifelse(GTD$provstate=="Delaware",8, 
                   ifelse(GTD$provstate=="District of Columbia",9, 
                          ifelse(GTD$provstate=="Florida",10,
          ifelse(GTD$provstate=="Georgia",11, 
                 ifelse(GTD$provstate=="Guam",12, 
                        ifelse(GTD$provstate=="Hawaii",13, 
                               ifelse(GTD$provstate=="Idaho",14,
          if_else(GTD$provstate=="Illinois",15, 
                  ifelse(GTD$provstate=="Indiana",16, 
                         ifelse(GTD$provstate=="Iowa",17, 
                                ifelse(GTD$provstate=="Kansas",18,
          ifelse(GTD$provstate=="Kentucky",19, 
                 ifelse(GTD$provstate=="Louisiana",20,
                        ifelse(GTD$provstate=="Maine",21, 
                               ifelse(GTD$provstate=="Maryland",22,
          ifelse(GTD$provstate=="Massachusetts",23, 
                 ifelse(GTD$provstate=="Michigan",24, 
                        ifelse(GTD$provstate=="Minnesota",25, 
                               ifelse(GTD$provstate=="Mississippi",26,
        ifelse(GTD$provstate=="Missouri",27,
               ifelse(GTD$provstate=="Montana",28, 
                      ifelse(GTD$provstate=="Nebraska",29,
                             ifelse(GTD$provstate=="Nevada",30,
        ifelse(GTD$provstate=="New Hampshire",31, 
               ifelse(GTD$provstate=="New Jersey",32, 
                      ifelse(GTD$provstate=="New Mexico",33,
                             ifelse(GTD$provstate=="New York",34,
      ifelse(GTD$provstate=="North Carolina",35, 
             ifelse(GTD$provstate=="North Dakota",36, 
                    ifelse(GTD$provstate=="Ohio",37,
  ifelse(GTD$provstate=="Oklahoma",38,ifelse(GTD$provstate=="Oregon",39,
       ifelse(GTD$provstate=="Pennsylvania",41,
              ifelse(GTD$provstate=="Puerto Rico",42, 
                     ifelse(GTD$provstate=="Rhode Island",43,
ifelse(GTD$provstate=="South Carolina",44, 
       ifelse(GTD$provstate=="South Dakota",45, 
              ifelse(GTD$provstate=="Tennessee",46, 
                     ifelse(GTD$provstate=="Texas",47,
ifelse(GTD$provstate=="Utah",48, 
       ifelse(GTD$provstate=="Vermont",49, 
              ifelse(GTD$provstate=="Virginia",50,
                     ifelse(GTD$provstate=="Washington",51,
                            ifelse(GTD$provstate=="West Virginia",52, 
       ifelse(GTD$provstate=="Wisconsin",53,ifelse(GTD$provstate=="Wyoming",54,-99))))))))))))))))))))))))))))))))))))))))))))))))))
View(GTD)

#rename columns

colnames(GTD)[c(1,2,3,7,8,115)]<-c("year","month","day","Region","State","Attacks Location")

#Reorder Columns and remove NAs                          

GTD<- GTD[, c(1,2,3,6,7,115,8:114)]

is.na(GTD)
GTD[is.na(GTD)] <- ""

View(GTD)

#Merge GTD and PPT.F

Main.Data <- GTD %>%
  left_join(PPT.F, by = (c("Attacks Location","gname")))

is.na(Main.Data)
Main.Data[is.na(Main.Data)] <- ""
View(Main.Data) 

#Exporting Data
# Write CSV in R
write.csv(Main.Data, file = "C:/Users/sadat/Desktop/CS-504/Data/Main.Data.csv", na="")


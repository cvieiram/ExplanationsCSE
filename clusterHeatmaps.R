library(sparcl)
library(Rmisc)
library(ggplot2)

# Explainers Codes:
# 0 - No explanation
# 1 - Reasoners
# 2 - Conceptual 
# 3 - Procedural 
# 4 - Principle-based Schematic 
# 5 - Schematic
# 6 - Conditional
# 7 - Limited

#colorsCPMSE <- c("white", "goldenrod4", "aquamarine4", "red4","gray42", "green3",  "salmon3","dodgerblue4", "mediumpurple3", "lemonchiffon4",  "firebrick4")

colorsCPMSE <- c("white", "goldenrod4", "aquamarine4", "red4","gray42", "dodgerblue4", "mediumpurple3",  "chocolate1","green3", "lemonchiffon4",  "firebrick4")

# Activity #2
heatM <- read.table("allSectionsAct2.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:5
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK','PK','SK','TK'),5)

# Identify and visualize the number of clusters
# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters <- computeClusters(knowTypeDF[,1:20], 4)
stdClusters[stdClusters==1,]$stdClusters <- 40
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 20
stdClusters[stdClusters==4,]$stdClusters <- 10
#groupLabels <-c('','Schematic - Principle-based', 'Procedural',  'Conceptual','Reasoners','','','')
groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','','','','')


# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std
# Update the ordering to respect the colors

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categories, 
                              'Section 1 - Creating the Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categories, 
                              'Section 2 - Setting up Problem Parameters', 11,colorsLevels,groupLabels,FALSE,colorsCPMSE  )
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categories, 
                              'Section 3 - Setting up Supporting Variables', 11,colorsLevels,groupLabels,FALSE,colorsCPMSE  )
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categories, 
                              'Section 4 - Validating Result', 11,colorsLevels,groupLabels,FALSE,colorsCPMSE  )
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categories, 
                              'Section 5 - End of the Function', 11,colorsLevels,groupLabels,'legend',colorsCPMSE  )

multiplot(section1,section2,section3, section4, section5, cols=2)
multiplot(section1,section2,section3, section4, cols=2)

png(file="../images/Act2ClusterHeat.png",width=1600,height=800)
multiplot(section1,section2,section3, section4, section5, cols=3)
dev.off()




# Activity #5
heatM <- read.table("allSectionsAct5.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:5
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK','PK','SK','TK'),5)

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,1:20], 4)

# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 20
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 40
stdClusters[stdClusters==4,]$stdClusters <- 50
#stdClusters[stdClusters==5,]$stdClusters <- 70
#groupLabels <-c('','Conditional', 'Procedural',  'Schematic',  'Conceptual','Limited','','')
groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','','','','')


# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categories, 
                              'Section 1 - Creating the Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categories, 
                              'Section 2 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categories, 
                              'Section 3 - Iterating', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categories, 
                              'Section 4 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categories, 
                              'Section 5 - End of the Function', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)

multiplot(section1,section2,section3, section4, cols=2)

png(file="../images/Act5ClusterHeat.png",width=1600,height=800)
multiplot(section1,section2,section3, section4, section5, cols=3)
dev.off()


# Activity 11 - This one has seven sections!
heatM <- read.table("allSectionsAct11.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:7
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK','CK','PK','SK','TK'),7)

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,1:30], 3)



# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 40
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 50


groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','','','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categories, 
                              'Section 1 - Creating the Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categories, 
                              'Section 2 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categories, 
                              'Section 3 - Setting up Supporting Variables', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categories, 
                              'Section 4 - Iterating', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categories, 
                              'Section 5 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categories,
                              'Section 6 - Validation', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section7 <- createHeatMapData(heatM[,c(1,128:148)], stdClusters, students, categories, 
                              'Section 7 - End of the Function', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)
multiplot(section1,section2,section3, section4, section5,section6,section7, cols=3)
multiplot(section1,section2,section3, section4, section5,section6,cols=2)

png(file=paste("../images/Act11ClusterHeat.png", sep=""),width=1600,height=850)
multiplot(section1,section2,section3, section4, section5,section6, section7, cols=3)
dev.off()













#Module 1 -  This one has six sections
heatM <- read.table("allSectionsMod1.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:6
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK','CK','PK','SK','TK'),6)

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,6:30], 4)
# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
# 60 - Goal-oriented
# 70 - Conceptual+Problem
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 20
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 60
stdClusters[stdClusters==4,]$stdClusters <- 70

groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','Goal-oriented','Conceptual+Problem','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categories, 
                              'Section 1 - Importing Libraries', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categories, 
                              'Section 2 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categories, 
                              'Section 3 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categories, 
                              'Section 4 - Setting up Supporting Variables', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categories, 
                              'Section 5 - Setting up Supporting Variables', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categories, 
                              'Section 6 - Validating Result',  11,colorsLevels,groupLabels,FALSE,colorsCPMSE)

multiplot(section1,section2,section3, section4, section5,section6,cols=3)

png(file="../images/Mod1ClusterHeat.png",width=1600,height=950)
multiplot(section2,section3, section4, section5, section6, section1, cols=3)
dev.off()






#Module 2 -  This one has eight sections!
heatM <- read.table("allSectionsMod2.csv",header=TRUE,sep=",")

# Create a vector with the number of sections
sections<- 1:8
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowType, heatM=heatM )))

# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK','CK','PK','SK','TK'),8)

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,1:35], 4)
#stdClusters <- computeClusters(knowTypeDF, 4)
# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
# 60 - Goal-oriented
# 70 - Conceptual+Problem
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 50
stdClusters[stdClusters==2,]$stdClusters <- 70
stdClusters[stdClusters==3,]$stdClusters <- 30
stdClusters[stdClusters==4,]$stdClusters <- 60
#stdClusters[stdClusters==5,]$stdClusters <- 20
groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','Goal-oriented','Conceptual+Problem','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categories, 
                              'Section 1 - Importing Libraries', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categories, 
                              'Section 2 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categories, 
                              'Section 3 - Creating Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categories, 
                              'Section 4 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categories, 
                              'Section 5 - Validation', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categories, 
                              'Section 6 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section7 <- createHeatMapData(heatM[,c(1,128:148)], stdClusters, students, categories, 
                              'Section 7 - Setting up GUI', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section8 <- createHeatMapData(heatM[,c(1,149:169)], stdClusters, students, categories, 
                              'Section 8 - Launching Program', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)
section9 <- createHeatMapData(heatM[,c(1,149:169)], stdClusters, students, categories, 
                              'Section 8 - Launching Program', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)

multiplot(section1,section2,section3,section4,  section5,section6,  section7,section8, cols=2)


png(file=paste("../images/Mod2ClusterHeat.png", sep=""),width=1600,height=1500)
multiplot(section1,section2,section3,section4,  section5,section6,  section7,section9, section8, cols=3)
dev.off()





# Module 3 - This one has eleven sections!
heatM <- read.table("allSectionsMod3.csv",header=TRUE,sep=",")

# Create a vector with the number of sections
sections<- 1:11
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowType, heatM=heatM )))

# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK','CK','PK','SK','TK'),11)

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,c(16:40)], 5)
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 70
stdClusters[stdClusters==2,]$stdClusters <- 60
stdClusters[stdClusters==3,]$stdClusters <- 20
stdClusters[stdClusters==4,]$stdClusters <- 50
stdClusters[stdClusters==5,]$stdClusters <- 50
#stdClusters[stdClusters==6,]$stdClusters <- 40
groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','Goal-oriented','Conceptual+Problem','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categories, 
                              'Section 1 - Importing Libraries', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categories, 
                              'Section 2 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categories, 
                              'Section 3 - Creating Configuring Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categories, 
                              'Section 4 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categories, 
                              'Section 5 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categories, 
                              'Section 6 - Creating Configuring Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section7 <- createHeatMapData(heatM[,c(1,128:148)], stdClusters, students, categories, 
                              'Section 7 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section8 <- createHeatMapData(heatM[,c(1,149:169)], stdClusters, students, categories, 
                              'Section 8 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section9 <- createHeatMapData(heatM[,c(1,170:190)], stdClusters, students, categories, 
                              'Section 9 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section10 <- createHeatMapData(heatM[,c(1,191:211)], stdClusters, students, categories, 
                               'Section 10 - Setting up GUI', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section11 <- createHeatMapData(heatM[,c(1,212:232)], stdClusters, students, categories, 
                               'Section 11 - Launching Program', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section12 <- createHeatMapData(heatM[,c(1,212:232)], stdClusters, students, categories, 
                               'Section 11 - Launching Program', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE )
multiplot(section1,section2,section3,section7,section8,section9, cols=2)
multiplot(section4,  section5,section6, section10,  section11,NULL, cols=2)

png(file=paste("../images/Mod3ClusterHeat.png", sep=""),width=2200,height=1500)
multiplot(section1,section2,section3,section4,  section5,section6,
          section7, section8, section9, section10, section11,section12, cols=4)
dev.off()

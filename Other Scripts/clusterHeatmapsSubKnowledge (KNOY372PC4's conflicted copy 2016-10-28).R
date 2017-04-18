
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

colorsCPMSE <- c("white", "goldenrod4", "aquamarine4", "red4","gray42", "dodgerblue4", "slateblue",  "saddlebrown","green3", "lemonchiffon4",  "firebrick4")

# Activity #2
heatM <- read.table("allSectionsAct2.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:5
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowSubType, heatM=heatM )))

# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK1','CK2','PK','SK','SK2','TK','TK2'),5)

# Organize the columns based on the new order (COD and DAT together, MON and OWN together.)
ordColumnNames <- unlist(lapply(sections,orderColNames, vars=categoriesNew))
heatM<- heatM[,c('STD',ordColumnNames)]

# Identify and visualize the number of clusters
# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))

stdClusters <- computeClusters(knowTypeDF[,1:32], 4)
stdClusters[stdClusters==1,]$stdClusters <- 40
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 70
stdClusters[stdClusters==4,]$stdClusters <- 10
#groupLabels <-c('','Schematic - Principle-based', 'Procedural',  'Conceptual','Reasoners','','','')
groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std
# Update the ordering to respect the colors

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categoriesNew, 
                              '(a) Section 1 - Creating the Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categoriesNew, 
                              '(b) Section 2 - Setting up Problem Parameters', 11,colorsLevels,groupLabels,FALSE,colorsCPMSE  )
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categoriesNew, 
                              '(c) Section 3 - Setting up Supporting Variables', 11,colorsLevels,groupLabels,FALSE,colorsCPMSE  )
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categoriesNew, 
                              '(d) Section 4 - Validating Result', 11,colorsLevels,groupLabels,FALSE,colorsCPMSE  )
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categoriesNew, 
                              '(e) Section 5 - End of the Function', 11,colorsLevels,groupLabels,'legend',colorsCPMSE  )

#multiplot(section1,section2,section3, section4, section5, cols=2)
#multiplot(section1,section2,section3, section4, cols=2)

png(file="../images/temp/Act2ClusterHeatS.png",width=1600,height=800)
multiplot(section1,section2,section3, section4, section5, cols=3)
dev.off()




# Activity #5
heatM <- read.table("allSectionsAct5.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:5
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowSubType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK1','CK2','PK','SK','SK2','TK','TK2'),5)

# Organize the columns based on the new order (COD and DAT together, MON and OWN together.)
ordColumnNames <- unlist(lapply(sections,orderColNames, vars=categoriesNew))
heatM<- heatM[,c('STD',ordColumnNames)]

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,1:32], 4)

# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 30
stdClusters[stdClusters==2,]$stdClusters <- 40
stdClusters[stdClusters==3,]$stdClusters <- 50
stdClusters[stdClusters==4,]$stdClusters <- 10
# The explanations of student S10 look nothing similar to S26 and S27,
# The clustering method puts them together because they didn't have many
# codes in each section, but the actual codes look different.
stdClusters[stdClusters$std=='S10',]$stdClusters <- 30
#groupLabels <-c('','Conditional', 'Procedural',  'Schematic',  'Conceptual','Limited','','')
groupLabels <-c('','Reasoners','Conceptual','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')


# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categoriesNew, 
                              '(a) Section 1 - Creating the Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categoriesNew, 
                              '(b) Section 2 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categoriesNew, 
                              '(c) Section 3 - Iterating', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categoriesNew, 
                              '(d) Section 4 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categoriesNew, 
                              '(e) Section 5 - End of the Function', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)

#multiplot(section1,section2,section3, section4, cols=2)

png(file="../images/temp/Act5ClusterHeat.png",width=1600,height=800)
multiplot(section1,section2,section3, section4, section5, cols=3)
dev.off()




# Activity 11 - This one has seven sections!
heatM <- read.table("allSectionsAct11.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:7
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowSubType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK1','CK2','PK','SK','SK2','TK','TK2'),7)

# Organize the columns based on the new order (COD and DAT together, MON and OWN together.)
ordColumnNames <- unlist(lapply(sections,orderColNames, vars=categoriesNew))
heatM<- heatM[,c('STD',ordColumnNames)]

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,1:48], 4)



# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 40
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 70
stdClusters[stdClusters==4,]$stdClusters <- 50

groupLabels <-c('','Reasoners','Declarative','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')


# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categoriesNew, 
                              '(a) Section 1 - Creating the Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categoriesNew, 
                              '(b) Section 2 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categoriesNew, 
                              '(c) Section 3 - Setting up Supporting Variables', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categoriesNew, 
                              '(d) Section 4 - Iterating', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categoriesNew, 
                              '(e) Section 5 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categoriesNew,
                              '(f) Section 6 - Validation', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section7 <- createHeatMapData(heatM[,c(1,128:148)], stdClusters, students, categoriesNew, 
                              '(g) Section 7 - End of the Function', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)
#multiplot(section1,section2,section3, section4, section5,section6,section7, cols=3)
#multiplot(section1,section2,section3, section4, section5,section6,cols=2)

png(file=paste("../images/temp/Act11ClusterHeat.png", sep=""),width=1600,height=850)
multiplot(section1,section2,section3, section4, section5,section6, section7, cols=3)
dev.off()






#Module 1 -  This one has six sections
heatM <- read.table("allSectionsMod1.csv",header=TRUE,sep=",")
# Create a vector with the number of sections
sections<- 1:6
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowSubType, heatM=heatM )))
# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK1','CK2','PK','SK','SK2','TK','TK2'),6)
# Organize the columns based on the new order (COD and DAT together, MON and OWN together.)
ordColumnNames <- unlist(lapply(sections,orderColNames, vars=categoriesNew))
heatM<- heatM[,c('STD',ordColumnNames)]

# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,9:48], 4)
# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
# 60 - Summarizer
# 70 - Problem-oriented
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 20
stdClusters[stdClusters==2,]$stdClusters <- 30
stdClusters[stdClusters==3,]$stdClusters <- 80
stdClusters[stdClusters==4,]$stdClusters <- 90

groupLabels <-c('','Reasoners','Declarative','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')

colorsTemp<-c("#FFFFFF", "#8C510A", "#C38937", "#E4CB8E", "#F5ECD5", "#DEEFED", "#9AD7CE", "#47A49B", "#076C64", "#003C30")
colorsTemp <- colorsCPMSE
# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categoriesNew, 
                              '(a) Section 1 - Importing Libraries', 11,colorsLevels, groupLabels, 'legend',colorsTemp)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categoriesNew, 
                              '(b) Section 2 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsTemp)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categoriesNew, 
                              '(c) Section 3 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsTemp)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categoriesNew, 
                              '(d) Section 4 - Setting up Supporting Variables', 11,colorsLevels, groupLabels, FALSE,colorsTemp)
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categoriesNew, 
                              '(e) Section 5 - Setting up Supporting Variables', 11,colorsLevels, groupLabels, FALSE,colorsTemp)
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categoriesNew, 
                              '(f) Section 6 - Validating Result',  11,colorsLevels,groupLabels,FALSE,colorsTemp)

#multiplot(section1,section2,section3, section4, section5,section6,cols=3)

png(file="../images/temp/Mod1ClusterHeat.png",width=1600,height=950)
multiplot(section2,section3, section4, section5, section6, section1, cols=3)
dev.off()


png(file="../images/temp/SampleHeat.png",width=550,height=500)
section4
dev.off()




#Module 2 -  This one has eight sections!
heatM <- read.table("allSectionsMod2.csv",header=TRUE,sep=",")

# Create a vector with the number of sections
sections<- 1:8
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowSubType, heatM=heatM )))

# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK1','CK2','PK','SK','SK2','TK','TK2'),8)
ordColumnNames <- unlist(lapply(sections,orderColNames, vars=categoriesNew))
heatM<- heatM[,c('STD',ordColumnNames)]
# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,1:56], 4)
#stdClusters <- computeClusters(knowTypeDF, 4)
# 0 - No explanation
# 10 - Reasoners
# 20 - Conceptual 
# 30 - Procedural 
# 40 - Schematic
# 50 - Limited
# 60 - Summarizer
# 70 - Problem-oriented
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 20
stdClusters[stdClusters==2,]$stdClusters <- 50
stdClusters[stdClusters==3,]$stdClusters <- 30
stdClusters[stdClusters==4,]$stdClusters <- 60
#stdClusters[stdClusters==5,]$stdClusters <- 20
#stdClusters[stdClusters==5,]$stdClusters <- 20
groupLabels <-c('','Reasoners','Declarative','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categoriesNew, 
                              '(a) Section 1 - Importing Libraries', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categoriesNew, 
                              '(b) Section 2 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categoriesNew, 
                              '(c) Section 3 - Creating Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categoriesNew, 
                              '(d) Section 4 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categoriesNew, 
                              '(e) Section 5 - Validation', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categoriesNew, 
                              '(f) Section 6 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section7 <- createHeatMapData(heatM[,c(1,128:148)], stdClusters, students, categoriesNew, 
                              '(g) Section 7 - Setting up GUI', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section8 <- createHeatMapData(heatM[,c(1,149:169)], stdClusters, students, categoriesNew, 
                              '(h) Section 8 - Launching Program', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE)
section9 <- createHeatMapData(heatM[,c(1,149:169)], stdClusters, students, categoriesNew, 
                              '(i) Section 8 - Launching Program', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)

#multiplot(section1,section2,section3,section4,  section5,section6,  section7,section8, cols=2)


png(file=paste("../images/temp/Mod2ClusterHeat.png", sep=""),width=1600,height=1500)
multiplot(section1,section2,section3,section4,  section5,section6,  section7,section9, section8, cols=3)
dev.off()



# Module 3 - This one has eleven sections!
heatM <- read.table("allSectionsMod3.csv",header=TRUE,sep=",")

# Create a vector with the number of sections
sections<- 1:11
# Aggregate the types of knowledge in all sections
knowTypeDF <- as.data.frame(do.call("cbind",lapply(sections,aggregateKnowSubType, heatM=heatM )))

# Include the students column
rownames(knowTypeDF)<-heatM$STD
# Assign the column names
names(knowTypeDF)<- rep(c('LK', 'CK1','CK2','PK','SK','SK2','TK','TK2'),11)
ordColumnNames <- unlist(lapply(sections,orderColNames, vars=categoriesNew))
heatM<- heatM[,c('STD',ordColumnNames)]
# Identify and visualize the number of clusters
stdClusters <- computeClusters(knowTypeDF[,c(9:64)], 5)
colorsLevels <- as.character(c(0,seq( from = 10 , to = 90, by = 10 )))
stdClusters[stdClusters==1,]$stdClusters <- 70
stdClusters[stdClusters==2,]$stdClusters <- 60
stdClusters[stdClusters==3,]$stdClusters <- 20
stdClusters[stdClusters==4,]$stdClusters <- 50
stdClusters[stdClusters==5,]$stdClusters <- 70

# Second clustering process.
# Since this example had many sections, it was necessary to do this process a second time with a sub group.
sub <- knowTypeDF[stdClusters[stdClusters$stdClusters==60,]$std,]
stdClustersSub <- computeClusters(sub[,c(9:64)],5)
stdClustersSub[stdClustersSub==1,]$stdClusters <- 60
stdClustersSub[stdClustersSub==2,]$stdClusters <- 50
stdClustersSub[stdClustersSub==3,]$stdClusters <- 60
stdClustersSub[stdClustersSub==4,]$stdClusters <- 20
stdClustersSub[stdClustersSub==5,]$stdClusters <- 60
a<-merge(stdClustersSub, stdClusters, by = "std",all = TRUE)
a[is.na(a$stdClusters.x),]$stdClusters.x <- a[is.na(a$stdClusters.x),]$stdClusters.y
stdClusters[a$std,]$std<-a$std
stdClusters[a$std,]$stdClusters<-a$stdClusters.x


#stdClusters[stdClusters==6,]$stdClusters <- 40
groupLabels <-c('','Reasoners','Declarative','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')

# Students ordered by the clusters
students <- stdClusters[order(stdClusters$stdClusters),]$std

# Creating the heat maps for each of the sections
section1 <- createHeatMapData(heatM[,c(1,2:22)], stdClusters, students, categoriesNew, 
                              '(a) Section 1 - Importing Libraries', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section2 <- createHeatMapData(heatM[,c(1,23:43)], stdClusters, students, categoriesNew, 
                              '(b) Section 2 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section3 <- createHeatMapData(heatM[,c(1,44:64)], stdClusters, students, categoriesNew, 
                              '(c) Section 3 - Creating Configuring Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE)
section4 <- createHeatMapData(heatM[,c(1,65:85)], stdClusters, students, categoriesNew, 
                              '(d) Section 4 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section5 <- createHeatMapData(heatM[,c(1,86:106)], stdClusters, students, categoriesNew, 
                              '(e) Section 5 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section6 <- createHeatMapData(heatM[,c(1,107:127)], stdClusters, students, categoriesNew, 
                              '(f) Section 6 - Creating Configuring Function', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section7 <- createHeatMapData(heatM[,c(1,128:148)], stdClusters, students, categoriesNew, 
                              '(g) Section 7 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section8 <- createHeatMapData(heatM[,c(1,149:169)], stdClusters, students, categoriesNew, 
                              '(h) Section 8 - Setting up Problem Parameters', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section9 <- createHeatMapData(heatM[,c(1,170:190)], stdClusters, students, categoriesNew, 
                              '(i) Section 9 - Creating GUI Object', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section10 <- createHeatMapData(heatM[,c(1,191:211)], stdClusters, students, categoriesNew, 
                               '(j) Section 10 - Setting up GUI', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section11 <- createHeatMapData(heatM[,c(1,212:232)], stdClusters, students, categoriesNew, 
                               '(k) Section 11 - Launching Program', 11,colorsLevels, groupLabels, FALSE,colorsCPMSE )
section12 <- createHeatMapData(heatM[,c(1,212:232)], stdClusters, students, categoriesNew, 
                               '(l) Section 11 - Launching Program', 11,colorsLevels, groupLabels, 'legend',colorsCPMSE )

png(file=paste("../images/temp/Mod3ClusterHeat.png", sep=""),width=2200,height=1500)
multiplot(section1,section2,section3,section4,  section5,section6,
          section7, section8, section9, section10, section11,section12, cols=4)
dev.off()


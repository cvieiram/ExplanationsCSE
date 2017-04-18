# Package for melting function - Histogram
library(reshape)
library(plyr)
library(ggplot2)
library(sparcl)
library(Rmisc)
library(ggplot2)
library(RColorBrewer)

# This function will aggregate the number of instances per category per student as
# the types of knowledge the students used in a section
aggregateKnowType <- function(heatM, sectionNumber){
  # Categories for each type of knowledge
  limitedK <- c('SIM','INC','LIM','PHR')
  conceptualK <- c('COA','VAR','DAT','PAR','COD')
  proceduralK <- c('HOW','EXE')
  schematicK <- c('PRO','GOA','BGK','WHY','RAG','INS')
  strategicK <- c('CON','MON','CHK','OWN')
  # The column name changes after the first section
  if(sectionNumber>1)
  {
    limitedK <- paste(limitedK,".",(sectionNumber-1), sep="")
    conceptualK <- paste(conceptualK,".",(sectionNumber-1), sep="")
    proceduralK <- paste(proceduralK,".",(sectionNumber-1), sep="")
    schematicK <- paste(schematicK,".",(sectionNumber-1), sep="")
    strategicK <- paste(strategicK,".",(sectionNumber-1), sep="")
  }
  #aggregate the number of instances
  typesofKnowledge <-cbind(rowSums(heatM[,limitedK]),
                           rowSums(heatM[,conceptualK]),
                           rowSums(heatM[,proceduralK]),
                           rowSums(heatM[,schematicK]),
                           rowSums(heatM[,strategicK]))
}

aggregateKnowSubType <- function(heatM, sectionNumber){
  # Categories for each type of knowledge
  limitedK <- c('SIM','INC','LIM','PHR')
  concK <- c('COA','VAR','PAR')
  concK2 <- c('DAT','COD')
  proceduralK <- c('HOW', 'EXE')
  schmK <- c('PRO','GOA','BGK')
  schmK2 <- c('WHY','RAG','INS')
  strategicK <- c('CON','CHK')
  strategicK2 <- c('MON','OWN')
  # The column name changes after the first section
  if(sectionNumber>1)
  {
    limitedK <- paste(limitedK,".",(sectionNumber-1), sep="")
    concK <- paste(concK,".",(sectionNumber-1), sep="")
    concK2 <- paste(concK2,".",(sectionNumber-1), sep="")
    proceduralK <- paste(proceduralK,".",(sectionNumber-1), sep="")
    schmK <- paste(schmK,".",(sectionNumber-1), sep="")
    schmK2 <- paste(schmK2,".",(sectionNumber-1), sep="")
    strategicK <- paste(strategicK,".",(sectionNumber-1), sep="")
    strategicK2 <- paste(strategicK2,".",(sectionNumber-1), sep="")
  }
  #aggregate the number of instances
  typesofKnowledge <-cbind(rowSums(heatM[,limitedK]),
                           rowSums(heatM[,concK]),
                           rowSums(heatM[,concK2]),
                           rowSums(heatM[,proceduralK]),
                           rowSums(heatM[,schmK]),
                           rowSums(heatM[,schmK2]),
                           rowSums(heatM[,strategicK]),
                           rowSums(heatM[,strategicK2]))
}

orderColNames <- function(sectionNumber, vars){
  orderedNames<- vars
  # The column name changes after the first section
  if(sectionNumber>1)
  {
    orderedNames <- paste(orderedNames,".",(sectionNumber-1), sep="")
  }
  orderedNames
}


# This function computes the distance among students and creates the
# numClusters clusters based on this distance
# Note that we are using a binary distance to group students based on the type of knowledge
# they used (or did not use) in all sections
computeClusters <- function(knowTypeDF, numClusters)
{
  set.seed(1234)
  # Calculate the distance among students
  d <- dist(knowTypeDF, method = "binary")
  # Create the hierarchical clusters using the distance matrix
  hc <- hclust(d)
  # load code of A2R function
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  # colored dendrogram
  op = par(bg = "#EFEFEF")
  stdClusters = cutree(hc, numClusters)
  stdClusters<- as.data.frame(stdClusters)
  stdClusters$std<- rownames(stdClusters)
  # Hierarchical Tree for the Clusters
  A2Rplot(hc, k = numClusters, boxes = FALSE, col.up = "gray50", 
          col.down = c("#FF6B6B", "#4ECDC4", "#556270","#AB6270", "#886270"),
          main = "Hierarchical Cluster Analysis: Activity #2")
  stdClusters
}

createHeatMap <- function(filename, stdClusters, students, plotTitle, nColors)
{
  my_palette <- colorRampPalette(c("white", "blue", "red"))(n = nColors)
  # Read the data for heat map activity section 1
  heatM <- read.table(filename,header=TRUE,sep=",")
  # Multiply the initial value by the number of the cluster to have different colors depending on the cluster
  if(!is.null(stdClusters))
  {
    heatM[,2:22]<- heatM[heatM$STD==stdClusters$std,-1]*stdClusters$stdClusters
  } 
  # Need to order the students based on the cluster they belong to
  heatM$STD <- factor(heatM$STD, levels = students)
  
  # We need to "melt" the data into this three-colum format in order to plot it
  meltedHM<- melt(heatM,id.vars=c("STD"))
  
  # Create the heatmap plot
  # Variable is the type of explanation
  # value is the number that is used to distinguish among clusters
  section <- ggplot(meltedHM, aes(x = variable, y = STD, fill = factor(value))) +
    scale_fill_manual(values = my_palette, name = "", guide = FALSE) +
    ggtitle(plotTitle) +
    labs(y= 'Student', x='Explanation Type')+geom_tile(color = "grey")
  
  section
}

# This function creates the heat map differentiating the clusters of students by color
createHeatMapData <- function(heatM, stdClusters, students, categories, plotTitle, nColors, explainerLevels,
                              groupLabels=NULL, showLegend='', colorList=c("white", "darkseagreen", "dodgerblue4", "goldenrod4", "firebrick4"))
{
  
  my_palette <- colorRampPalette(colorList)(n = nColors)
  # Read the data for heat map activity section 1
  names(heatM) <-  c("STD",categories)
  # Multiply the initial value by the number of the cluster to have different colors depending on the cluster
  if(!is.null(stdClusters))
  {
    heatM[,2:22]<- heatM[heatM$STD==stdClusters$std,-1]*stdClusters$stdClusters
  } 
  # Need to order the students based on the cluster they belong to
  heatM$STD <- factor(heatM$STD, levels = students)
  
  # We need to "melt" the data into this three-colum format in order to plot it
  meltedHM<- melt(heatM,id.vars=c("STD"))
  colorsLevels <- c(0,levels(as.factor(stdClusters$stdClusters)))
  #colorsLevels <- explainerLevels
  # Create the heatmap plot
  # Variable is the type of explanation
  # value is the number that is used to distinguish among clusters

  print(groupLabels)
  print(explainerLevels)
  #print(my_palette)
  section <- ggplot(meltedHM, aes(x = variable, y = STD, fill = factor(value, levels=explainerLevels))) +
    scale_fill_manual(values = my_palette, labels=groupLabels, 
                      guide=showLegend, 
                      breaks=explainerLevels, name = "", drop=FALSE) +
    ggtitle(plotTitle) + 
    labs(y= 'Student', x='Explanation Type')+geom_tile(color = "grey")
  
  if(showLegend=='legend')
  {
    section<-section+theme(legend.position="left", legend.text=element_text(size=20)) +
      guides(fill = guide_legend(reverse = TRUE))
  }
  
  section
}

# This function reads the students explanation files (e.g., studentsAct2, studentsMod1)
# and returns a data frame with the number of times each student used each category 
# in their explanations
organizeStudentsExplanations <- function(fileName, categories)
{
  data <- read.table(fileName,header=TRUE,sep=",")
  
  expList <- sapply(data, table)
  
  expDF <- data.frame()
  for(i in seq(along=expList)) for(j in categories)
    expDF[i,j] <- expList[[i]][j]
  
  expDF[is.na(expDF)]<- 0
  expDF <- expDF[, categories]
  expDF<- cbind(colnames(data), expDF)
  expDF
}

plotKnowledgeDist <- function(distributionKnow, course='CPMSE')
{
  mData <- melt(distributionKnow,id.vars=c("Activity"))
  types <- c('Limited Knowledge', 'Declarative Knowledge', 'Procedural Knowledge', 'Schematic Knowledge', 'Strategic Knowledge')
  
  # calculate midpoints of bars (simplified using comment by @DWin)
  mData <- ddply(mData, 'Activity', transform, pos = cumsum(value) - (0.5 * value))
  mData$value <- round(mData$value,digits = 2)
  if(course=='CPMSE')
  {
    mData$Activity <- factor(mData$Activity, levels = c('Two', 'Five', 'Eleven'))
  }
  else
  {
    mData$Activity <- factor(mData$Activity, levels = c('One', 'Two', 'Three'))
  }
  
  returnPlot <- ggplot(mData, aes(x=Activity,y=value,fill=factor(variable))) + 
        geom_bar(stat="identity")+
        scale_fill_brewer(palette="BrBG", labels=types) +
        geom_text(aes(y=pos, label=sprintf("%1.2f%%", value)), color="black", size=6)+
        labs(y= 'Percentage of ocurrences', x='Activity', fill='Type of Knowledge')+
        theme(legend.text=element_text(size=16), axis.text = element_text(size=16), axis.title = element_text(size=16))
        
  returnPlot
}

plotCategoriesDist <- function(data, title)
{
  colnames(data) <- c('KnowType', 'category', 'value')
  data$value <-  as.numeric(data$value)
  
  # calculate midpoints of bars (simplified using comment by @DWin)
  data <- ddply(data, 'category', transform, pos = cumsum(value) - (0.5 * value))
  returnPlot <- ggplot(data, aes(x=category,y=value, fill=category)) + 
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_brewer(palette="BrBG") +
    geom_text(aes(y=pos, label=value), color="black", size=4)+
    ggtitle(title)+coord_cartesian(ylim = c(-1, 650)) +
    labs(y= 'Number of Instances', x='Category', fill='Category')+
    theme(panel.background = element_rect(fill='gray'))
    
  returnPlot
}


  coord_polar(theta = "y")
  labs(y= 'Percentage of Students', x='Comment Category', fill="Code Section")

plotMeans <- function(dataSE, colName, plotingVars, plotingVarsNames, 
                      groupingLabels, groupingLevels, colorP='BrBG',
                      xLabel='', yLabel='', fillLabel='', title='', 
                      maxY=10, minY=0, showLegend='legend')
{
  mdataSE <- melt(dataSE, id.vars = c("Student",colName), measure.vars=plotingVars)
  #colorP='BrBG',
  brbg <- brewer.pal(11, colorP)
  nColors <- length(groupingLevels)+1
  my_palette <- colorRampPalette(brbg[-1])(n = nColors)
  # Only consider those columns/students who have a value in it
  mdataSE<-mdataSE[complete.cases(mdataSE),]
  # Compute means and standard error (se)
  means <- ddply(mdataSE, c(colName, "variable"), summarise, 
                 mean=mean(value, na.rm = TRUE), se=sd(value, na.rm = TRUE)/sqrt(length(value)))
  
  means[,colName] <- factor(means[,colName], levels=groupingLevels)
  colnames(means)[1]<- "PerformanceGroup"
  
  p <- ggplot(means, aes(x=variable, y=mean, fill= PerformanceGroup)) + 
    geom_bar(position=position_dodge(), stat="identity")+
    labs(x= xLabel, y=yLabel, fill=fillLabel)+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
    scale_x_discrete(labels=plotingVarsNames)+
    scale_y_continuous(breaks=c(-3:10))+
    scale_fill_manual(values = my_palette, labels=groupingLabels, 
                      guide=showLegend, 
                      breaks=groupingLevels, name = "", drop=FALSE)+
    coord_cartesian(ylim = c(minY, maxY)) +
    ggtitle(title)+
    theme(panel.background = element_rect(fill='gray'), axis.text = element_text(size=14), axis.title = element_text(size=16))
  if(showLegend=='legend')
  {
    p<-p+theme(legend.text=element_text(size=20))
  }
  p
}

createHeatMapPerformance <- function(heatM, stdGroups, students, categories, plotTitle, nColors)
{
  my_palette <- colorRampPalette(c("white", "blue", "red"))(n = nColors)
  # Read the data for heat map activity section 1
  names(heatM) <-  c("STD",categories)
  # Multiply the initial value by the number of the cluster to have different colors depending on the cluster
  if(!is.null(stdGroups))
  {
    heatM[,2:22]<- heatM[heatM$STD==stdGroups$std,-1]*(stdGroups$performance)
  } 
  # Need to order the students based on the cluster they belong to
  heatM$STD <- factor(heatM$STD, levels = students)
  
  # We need to "melt" the data into this three-colum format in order to plot it
  meltedHM<- melt(heatM,id.vars=c("STD"))
  colorsLevels <- c(0,levels(as.factor(stdClusters$stdClusters)))
  # Create the heatmap plot
  # Variable is the type of explanation
  # value is the number that is used to distinguish among clusters
  section <- ggplot(meltedHM, aes(x = variable, y = STD, fill = factor(value, levels=colorsLevels))) +
    scale_fill_manual(values = my_palette, breaks=colorsLevels, name = "", guide=FALSE, drop=FALSE) +
    ggtitle(plotTitle) +
    labs(y= 'Student', x='Explanation Type')+geom_tile(color = "grey")
  
  section
}


plotExplainerDist <- function(distExplainers, colorList='BrBG', course='CPMSE')
{
  mData <- melt(distExplainers,id.vars=c("Activity"))
  nColors <- 10
  if(length(colorList)<2)
  {
    types <- c('Reasoners','Declarative','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented')
    expLevels <- c('Reasoners','Declarative','Procedural','Schematic','Limited', 'GoalOriented','Problem.oriented')
    
    brbg <- brewer.pal(11, colorList)
    
    my_palette <- colorRampPalette(brbg[-1])(n = 7)
  }
  else
  {
    types <- c('','Reasoners','Declarative','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented','','')
    expLevels <- c('No', 'Reasoners','Declarative','Procedural','Schematic','Limited', 'GoalOriented','Problem.oriented','out1','out2')
    
    my_palette <- colorRampPalette(colorList)(n = nColors)
  }
  
  # calculate midpoints of bars (simplified using comment by @DWin)
  mData <- ddply(mData, 'Activity', transform, pos = cumsum(value) - (0.5 * value))
  mData$value <- round(mData$value,digits = 2)
  
  if(course=='CPMSE')
  {
    mData$Activity <- factor(mData$Activity, levels = c('Activity 2', 'Activity 5', 'Activity 11'))
  }
  else
  {
    mData$Activity <- factor(mData$Activity, levels = c('Module 1', 'Module 2', 'Module 3'))
  }
  mData$variable <- factor(mData$variable, levels=expLevels)
  
  returnPlot <- ggplot(mData, aes(x=Activity,y=value,fill=variable)) + 
    geom_bar(stat="identity")+
    scale_fill_manual(values = my_palette, labels=types, 
                      guide='legend', 
                      breaks=expLevels, name = "Type of Explainer", drop=FALSE)+
    geom_text(aes(y=pos, label=sprintf("%1.2f%%", value)), color="black", size=5.2)+
    labs(y= 'Percentage of Students', x='Activity', fill='Type of Knowledge')+
    theme(legend.text=element_text(size=16), axis.text = element_text(size=16), axis.title = element_text(size=16), panel.background = element_rect(fill='gray'))
    
  
  returnPlot
}


plotCategoriesPerGroup <- function(mergedData, groupingVble, variables, fillTitle='',plotTitle='', colorP='BrBG')
{
  meltedResult <- melt(mergedData[,variables], id.vars=c("Student", groupingVble))
  meltedResult<- meltedResult[complete.cases(meltedResult),]
  names <- c('Low', 'Mid', 'High')
  means <- ddply(meltedResult, c(groupingVble, "variable"), summarise, 
                 mean=mean(value, na.rm = TRUE), se=sd(value, na.rm = TRUE)/sqrt(length(value)))
  colnames(means) <- c('Group','variable','mean','se')
  ggplot(means, aes(x=variable, y=mean, fill= as.factor(Group))) + 
    geom_bar(position=position_dodge(), stat="identity")+
    scale_fill_brewer(labels=names, palette = colorP, guide = guide_legend(title = fillTitle))+
    labs(x= 'Explanation Type', y='Average Number per Group', fill=names)+
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
    scale_y_continuous(breaks=c(-3:10))+
    ggtitle(plotTitle) +
    theme(panel.background = element_rect(fill='gray'))
}

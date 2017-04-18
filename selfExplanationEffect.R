library(RColorBrewer)

# Ploting
library(ggplot2)
# ddply
library(plyr)

library(dplyr)
# Package for melting function - Histogram
library(reshape)
# Multiplot
library(Rmisc)

dataSE <- read.table("studentsDataCPMSE.csv",header=TRUE,sep=",")
dataSE <- read.table("studentsDataCPMSEHL.csv",header=TRUE,sep=",")

dataSE$CourseGrade <- dataSE$CourseGrade/100
dataSE$Midterm1 <- dataSE$Midterm1/10
dataSE$Midterm2 <- dataSE$Midterm2/10

dataSE$wordsAct2 <- 10*dataSE$wordsAct2/max(dataSE$wordsAct2, na.rm = TRUE)
dataSE$NumExplanationsAct2 <- 10*dataSE$NumExplanationsAct2/max(dataSE$NumExplanationsAct2, na.rm = TRUE)
dataSE$NumCategoriesAct2 <- 10*dataSE$NumCategoriesAct2/max(dataSE$NumCategoriesAct2, na.rm = TRUE)

dataSE$wordsAct5 <- 10*dataSE$wordsAct5/max(dataSE$wordsAct5, na.rm = TRUE)
dataSE$NumExplanationsAct5 <- 10*dataSE$NumExplanationsAct5/max(dataSE$NumExplanationsAct5, na.rm = TRUE)
dataSE$NumCategoriesAct5 <- 10*dataSE$NumCategoriesAct5/max(dataSE$NumCategoriesAct5, na.rm = TRUE)

dataSE$wordsAct11 <- 10*dataSE$wordsAct11/max(dataSE$wordsAct11, na.rm = TRUE)
dataSE$NumExplanationsAct11 <- 10*dataSE$NumExplanationsAct11/max(dataSE$NumExplanationsAct11, na.rm = TRUE)
dataSE$NumCategoriesAct11 <- 10*dataSE$NumCategoriesAct11/max(dataSE$NumCategoriesAct11, na.rm = TRUE)
dataSE$RubricAct11 <- 10*(dataSE$RubricAct11/15)

dataSE$PerceivedAbility <- 10*(dataSE$PerceivedAbility/5)

##########################################################################################
#########################################CPMSE############################################
##################################PERFORMANCE BY CLUSTER###################################

##########################################################################################
#####################################Activity #2#########################################
##########################################################################################
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Act 2'
groupingLabels <- c('Reasoners','Procedural','Schematic', 'Problem-O')
plotingVarsNames<- c('Perceived Ability',  'Midterm One', 'Midterm Two', 'Quiz Score')
plotingVars <- c('PerceivedAbility',  'Midterm1', 'Midterm2','RubricAct2')
plotingVarsNames<- c('Perceived Ability',  'Midterm One', 'Midterm Two')
plotingVars <- c('PerceivedAbility',  'Midterm1', 'Midterm2')
groupingLevels <- c(10,30,40,70)
colName <- 'ClusterAct2'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)
png(file="../images/temp/Act2Cluster.png",width=1000,height=800)
p1
dev.off()


tiff(file="../images/paper/Act2Cluster.tiff",width=1000,height=800)
p1
dev.off()


# Inferential STATS
fit<- aov(dataSE$PerceivedAbility~as.factor(dataSE$ClusterAct2), na.action = na.omit)
summary(fit)
plot(fit)
fit<- aov(dataSE$PerceivedAbility~as.factor(dataSE$ClusterAct5))
summary(fit)
TukeyHSD(fit)
str(dataSE$ClusterAct5)

fit<- aov(dataSE$Midterm1~as.factor(dataSE$ClusterAct5))
summary(fit)
TukeyHSD(fit)

##########################################################################################
#####################################Activity #5##########################################
##########################################################################################
#  Course grade, perceived ability, Score Activity and Rubric Activity by Cluster
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Act 5'
groupingLabels <- c('Reasoners','Procedural','Schematic','Limited')
#plotingVarsNames<- c('Perceived Ability', 'Course Grade', 'Midterm One', 'Midterm Two', 'Quiz Score')
#plotingVars <- c('PerceivedAbility', 'CourseGrade', 'Midterm1', 'Midterm2','RubricAct5')
plotingVarsNames<- c('Perceived Ability', 'Midterm One', 'Midterm Two', 'Quiz Score')
plotingVars <- c('PerceivedAbility',  'Midterm1', 'Midterm2','ScoreAct5')
plotingVarsNames<- c('Perceived Ability', 'Midterm One', 'Midterm Two')
plotingVars <- c('PerceivedAbility',  'Midterm1', 'Midterm2')

groupingLevels <- c(10,30,40,50)
colName <- 'ClusterAct5'
dataSET <- dataSE[dataSE$ClusterAct5 %in% c(10,30,40), ]
p1 <- plotMeans(dataSET, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)
png(file="../images/temp/Act5Cluster.png",width=1000,height=800)
p1
dev.off()

tiff(file="../images/paper/Act5Cluster.tiff",width=1000,height=800)
p1
dev.off()


##########################################################################################
#####################################Activity #11##########################################
##########################################################################################
#  Course grade, perceived ability, Score Activity and Rubric Activity by Cluster
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Act 11'
groupingLabels <- c('Procedural','Schematic', 'Limited','Problem Oriented')
plotingVarsNames<- c('Perceived Ability', 'Midterm One', 'Midterm Two', 'Quiz Score')
plotingVars <- c('PerceivedAbility', 'Midterm1', 'Midterm2','RubricAct11')
groupingLevels <- c(30,40, 50,70)
colName <- 'ClusterAct11'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)
png(file="../images/temp/Act11Cluster.png",width=1000,height=800)
p1
dev.off()



##########################################################################################
#############################Explanations by Prior Experience#############################
##########################################################################################

##########################################################################################
#####################################Activity #2##########################################
##########################################################################################
xLabel<- 'Measures Act 2'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Quiz Score')
plotingVars <- c('NumCategoriesAct2', 'NumExplanationsAct2', 'wordsAct2','RubricAct2')
groupingLevels <- c(0, 1,2)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)

png(file="../images/temp/Act2Prior.png",width=1000,height=800)
p1
dev.off()


##########################################################################################
#####################################Activity #5##########################################
##########################################################################################
xLabel<- 'Measures Act 5'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Quiz Score')
plotingVars <- c('NumCategoriesAct5', 'NumExplanationsAct5', 'wordsAct5','RubricAct5')
groupingLevels <- c(0, 1,2)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)

png(file="../images/temp/Act5Prior.png",width=1000,height=800)
p1
dev.off()


##########################################################################################
#####################################Activity #11##########################################
##########################################################################################
xLabel<- 'Measures Act 11'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Quiz Score')
plotingVars <- c('NumCategoriesAct11', 'NumExplanationsAct11', 'wordsAct11','RubricAct11')
groupingLevels <- c(0, 1,2)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)

png(file="../images/temp/Act5Prior.png",width=1000,height=800)
p1
dev.off()

##########################################################################################
###############################Explanations by Performance################################
##########################################################################################

##########################################################################################
#####################################Activity #2##########################################
##########################################################################################
xLabel<- 'Measures Activity 2'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Performance Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words')
plotingVars <- c('NumCategoriesAct2', 'NumExplanationsAct2', 'wordsAct2')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(a) Perceived Ability', 10, 0, FALSE)
colName <- 'GroupMid1'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(b) Midterm 1', 10, 0, FALSE)
colName <- 'GroupMid2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(c) Midterm 2', 10, 0, 'legend')
colName <- 'GroupAct2'
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(d) Quiz Score', 10, 0, 'legend')
png(file="../images/temp/Act2Performance.png",width=1400,height=400)
multiplot(p1,p2,p3, cols=3)
dev.off()


tiff(file="../images/paper/Act2Performance.tiff",width=1400,height=400)
multiplot(p1,p2,p3, cols=3)
dev.off()

# Inferential STATS
fit<- aov(dataSE$wordsAct2~as.factor(dataSE$GroupAbility), na.action = na.omit)
summary(fit)
plot(fit)
fit<- aov(dataSE$PerceivedAbility~as.factor(dataSE$ClusterAct5))
summary(fit)
TukeyHSD(fit)
str(dataSE$ClusterAct5)

fit<- aov(dataSE$Midterm1~as.factor(dataSE$ClusterAct5))
summary(fit)
TukeyHSD(fit)

fit<- aov(dataSE$wordsAct5~as.factor(dataSE$GroupAbility), na.action = na.omit)
summary(fit)

fit<- aov(dataSE$wordsAct2~as.factor(dataSE$GroupMid1), na.action = na.omit)
summary(fit)

fit<- aov(dataSE$wordsAct5~as.factor(dataSE$GroupMid1), na.action = na.omit)
summary(fit)


##########################################################################################
#####################################Activity #5##########################################
##########################################################################################
xLabel<- 'Measures Activity 5'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Performance Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words', 'Quiz Score')
plotingVars <- c('NumCategoriesAct5', 'NumExplanationsAct5', 'wordsAct5','RubricAct5')
plotingVarsNames<- c('Categories', 'Explanations', 'Words' )
plotingVars <- c('NumCategoriesAct5', 'NumExplanationsAct5', 'wordsAct5')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(d) Perceived Ability', 10, 0, FALSE)
colName <- 'GroupMid1'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(e) Midterm 1', 10, 0, FALSE)
colName <- 'GroupMid2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(f) Midterm 2', 10, 0, 'legend')
colName <- 'GroupAct5'
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, 'Quiz Score', 10, 0, 'legend')
png(file="../images/temp/Act5Performance2.png",width=1500,height=400)
#multiplot(p1,p2,p3, p4, cols=2)
multiplot(p1,p2,p3, cols=3)
dev.off()


tiff(file="../images/paper/Act5Performance.tiff",width=1500,height=400)
multiplot(p1,p2,p3, cols=3)
dev.off()



##########################################################################################
#####################################Activity #11##########################################
##########################################################################################
xLabel<- 'Measures Activity 11'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Performance Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words')
plotingVars <- c('NumCategoriesAct11', 'NumExplanationsAct11', 'wordsAct11')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(a) Perceived Ability', 10, 0, FALSE)
colName <- 'GroupMid1'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(b) Midterm 1', 10, 0, FALSE)
colName <- 'GroupMid2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, '(c) Midterm 2', 10, 0, 'legend')
colName <- 'GroupAct11'
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,'Test Activity 11')

png(file="../images/temp/Act11Performance.png",width=1500,height=400)
multiplot(p1,p2,p3, cols=3)
dev.off()


dataSE[dataSE$GroupAbility %in% c(1),]


# Other Pallete: 'YlGnBu'

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

dataSE$CourseGrade <- dataSE$CourseGrade/100
dataSE$Midterm1 <- dataSE$Midterm1/10
dataSE$Midterm2 <- dataSE$Midterm2/10
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
fillLabel <- 'Explainers Act 11'
groupingLabels <- c('Problem-oriented', 'Reasoners',  'Procedural',  'Conceptual','Limited')
plotingVarsNames<- c('Perceived Ability', 'Course Grade', 'Midterm One', 'Midterm Two', 'Quiz Score')
plotingVars <- c('PerceivedAbility', 'CourseGrade', 'Midterm1', 'Midterm2','RubricAct5')
groupingLevels <- c(1,2,3,4,5)
colName <- 'ClusterAct5'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)
png(file="../images/Act5Cluster.png",width=1000,height=800)
p1
dev.off()


##########################################################################################
#####################################Activity #5##########################################
##########################################################################################
#  Course grade, perceived ability, Score Activity and Rubric Activity by Cluster
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Act 5'
groupingLabels <- c('Problem-oriented', 'Reasoners',  'Procedural',  'Conceptual','Limited')
plotingVarsNames<- c('Perceived Ability', 'Course Grade', 'Midterm One', 'Midterm Two', 'Quiz Score')
plotingVars <- c('PerceivedAbility', 'CourseGrade', 'Midterm1', 'Midterm2','RubricAct5')
groupingLevels <- c(1,2,3,4,5)
colName <- 'ClusterAct5'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)
png(file="../images/Act5Cluster.png",width=1000,height=800)
p1
dev.off()





##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Prior Experience
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

png(file="../images/Act5Prior.png",width=1000,height=800)
p1
dev.off()


##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Performance Groups
xLabel<- 'Measures Activity 5'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Performance Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words', 'Quiz Score')
plotingVars <- c('NumCategoriesAct5', 'NumExplanationsAct5', 'wordsAct5','RubricAct5')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, 'Perceived Ability')
colName <- 'GroupMid1'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, 'Midterm 1')
colName <- 'GroupMid2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, 'Midterm 2')
colName <- 'GroupAct5'
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel, 'Quiz Score')
png(file="../images/Act5Performance.png",width=1000,height=800)
multiplot(p1,p2,p3, p4, cols=2)
dev.off()














##########################################################################################
#####################################Activity #11#########################################
##########################################################################################
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Act 11'
groupingLabels <- c('Procedural explainers', 'Conceptual explainers', 
                    'Principle-based explainers', 'Integrating Explainers')
plotingVarsNames<- c('Perceived Ability', 'Course Grade', 'Midterm One', 'Midterm Two', 'Rubric Score')
plotingVars <- c('PerceivedAbility', 'CourseGrade', 'Midterm1', 'Midterm2','RubricAct11')
groupingLevels <- c(1,2,3,4)
colName <- 'ClusterAct11'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel)

png(file="../images/Act11Cluster.png",width=1000,height=800)
p1
dev.off()



##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Prior Experience
xLabel<- 'Measures Act 11'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score')
plotingVars <- c('NumCategoriesAct11', 'NumExplanationsAct11', 'wordsAct11','RubricAct11')
groupingLevels <- c(0, 1,2)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel)

png(file="../images/Act11Prior.png",width=1000,height=800)
p1
dev.off()

##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Performance Groups
xLabel<- 'Measures Activity 11'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Performance Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words', 'Rubric Score')
plotingVars <- c('NumCategoriesAct11', 'NumExplanationsAct11', 'wordsAct11','RubricAct11')
groupingLevels <- c(1,2,3)
colName <- 'GroupGrade'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Course Grade')
colName <- 'GroupMid1'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Midterm 1')
colName <- 'GroupMid2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Midterm 2')
colName <- 'GroupAct11'
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel,'Test Activity 11')

png(file="../images/Act11Performance.png",width=1000,height=800)
multiplot(p1,p4, p2,p3, cols=2)
dev.off()



dataSE <- read.table("studentsDataThermo.csv",header=TRUE,sep=",")

dataSE$wordsMod1 <- 10*dataSE$wordsMod1/max(dataSE$wordsMod1, na.rm = TRUE)
dataSE$wordsMod2<- 10*dataSE$wordsMod2/max(dataSE$wordsMod2, na.rm = TRUE)
dataSE$wordsMod3 <- 10*dataSE$wordsMod3/max(dataSE$wordsMod3, na.rm = TRUE)

dataSE$NumExplanationsMod1 <- 10*dataSE$NumExplanationsMod1/max(dataSE$NumExplanationsMod1, na.rm = TRUE)
dataSE$NumExplanationsMod2 <- 10*dataSE$NumExplanationsMod2/max(dataSE$NumExplanationsMod2, na.rm = TRUE)
dataSE$NumExplanationsMod3 <- 10*dataSE$NumExplanationsMod3/max(dataSE$NumExplanationsMod3, na.rm = TRUE)

dataSE$NumCategoriesMod1 <- 10*dataSE$NumCategoriesMod1/max(dataSE$NumCategoriesMod1, na.rm = TRUE)
dataSE$NumCategoriesMod2 <- 10*dataSE$NumCategoriesMod2/max(dataSE$NumCategoriesMod2, na.rm = TRUE)
dataSE$NumCategoriesMod3 <- 10*dataSE$NumCategoriesMod3/max(dataSE$NumCategoriesMod3, na.rm = TRUE)

##########################################################################################
#####################################Module # 2###########################################
##########################################################################################


#  Course grade, perceived ability, Score Activity and Rubric Activity by Cluster
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Mod 2'

groupingLabels <- c('Problem-oriented', 'Limited', 'Limited+Schematic' , 'Procedural', 'Reasoners')
plotingVarsNames<- c('Perceived Ability', 'Pretest', 'Posttest', 'Gain')
plotingVars <- c('PerceivedAbility', 'Pretest2', 'Posttest2', 'Gain2')
groupingLevels <- c(1,2,3,4, 5)
colName <- 'ClusterMod2'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel,'Performance by Cluster',5,-0.5)
png(file="../images/Mod2Cluster.png",width=1000,height=800)
p1
dev.off()


##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Prior Experience
xLabel<- 'Measures Mod 2'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two', 'Three')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Post-Pre Gain')
plotingVars <- c('NumCategoriesMod2', 'NumExplanationsMod2', 'wordsMod2','Gain2')
groupingLevels <- c(0, 1,2, 3)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,
                'Number of Explanations by Prior Experience',10,-1)

png(file="..images/Mod2Prior.png",width=1000,height=800)
p1
dev.off()


##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Performance Groups
xLabel<- 'Measures Module 2'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words', 'Posttest')
plotingVars <- c('NumCategoriesMod2', 'NumExplanationsMod2', 'wordsMod2','Posttest2')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Perceived Ability')
colName <- 'GroupGain2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Post-Pre Gain')
colName <- 'GroupPre2'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Pretest Score')
colName <- 'PriorCourses'
dataSE[dataSE[,'PriorCourses']>2&complete.cases(dataSE[,'PriorCourses']),]$PriorCourses <-2
groupingLevels <- c(0,1,2)
groupingLabels <- c('Zero', 'One', 'Two +')
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'YlGnBu', xLabel, yLabel, fillLabel, 'Prior Courses')
png(file="../images/Mod2Performance.png",width=1000,height=800)
multiplot(p1,p2,p3, p4, cols=2)
dev.off()


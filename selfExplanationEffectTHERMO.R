dataSE <- read.table("studentsDataThermo.csv",header=TRUE,sep=",")
dataSE <- read.table("studentsDataThermoSub.csv",header=TRUE,sep=",")

dataSE$wordsMod1 <- 10*dataSE$wordsMod1/max(dataSE$wordsMod1, na.rm = TRUE)
dataSE$wordsMod2<- 10*dataSE$wordsMod2/max(dataSE$wordsMod2, na.rm = TRUE)
dataSE$wordsMod3 <- 10*dataSE$wordsMod3/max(dataSE$wordsMod3, na.rm = TRUE)

dataSE$NumExplanationsMod1 <- 10*dataSE$NumExplanationsMod1/max(dataSE$NumExplanationsMod1, na.rm = TRUE)
dataSE$NumExplanationsMod2 <- 10*dataSE$NumExplanationsMod2/max(dataSE$NumExplanationsMod2, na.rm = TRUE)
dataSE$NumExplanationsMod3 <- 10*dataSE$NumExplanationsMod3/max(dataSE$NumExplanationsMod3, na.rm = TRUE)

dataSE$NumCategoriesMod1 <- 10*dataSE$NumCategoriesMod1/max(dataSE$NumCategoriesMod1, na.rm = TRUE)
dataSE$NumCategoriesMod2 <- 10*dataSE$NumCategoriesMod2/max(dataSE$NumCategoriesMod2, na.rm = TRUE)
dataSE$NumCategoriesMod3 <- 10*dataSE$NumCategoriesMod3/max(dataSE$NumCategoriesMod3, na.rm = TRUE)

dataSE[dataSE[,'PriorCourses']>2&complete.cases(dataSE[,'PriorCourses']),]$PriorCourses <-2

#  Course grade, perceived ability, Score Activity and Rubric Activity by Cluster
##########################################################################################
#####################################Module # 1###########################################
##########################################################################################
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Mod 1'

groupingLabels <- c('Conceptual', 'Procedural', 'Summarizer', 'Problem-oriented')
groupingLabels <-c('Reasoners','Conceptual','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented')
#plotingVarsNames<- c('Perceived Ability', 'Pretest', 'Posttest', 'Gain')
#plotingVars <- c('PerceivedAbility', 'Pretest1', 'Posttest1', 'Gain1')
plotingVarsNames<- c('Perceived Ability', 'Pretest', 'Posttest')
plotingVars <- c('PerceivedAbility', 'Pretest1', 'Posttest1')

groupingLevels <- c(20,30,60,70)
groupingLevels <- c(10, 20,30, 40,50, 60,70)
colName <- 'ClusterMod1'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel,'Performance by Cluster',5)
png(file="../images/temp/Mod1Cluster.png",width=1000,height=800)
p1
dev.off()

##########################################################################################
#####################################Module # 2###########################################
##########################################################################################
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Mod 2'

groupingLabels <- c('Conceptual','Procedural',  'Limited', 'Summarizer')
groupingLabels <-c('Reasoners','Conceptual','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented')
# plotingVarsNames<- c('Perceived Ability', 'Pretest', 'Posttest', 'Gain')
# plotingVars <- c('PerceivedAbility', 'Pretest2', 'Posttest2', 'Gain2')
plotingVarsNames<- c('Perceived Ability', 'Pretest',  'Posttest','Gain2')
plotingVars <- c('PerceivedAbility', 'Pretest2',  'Posttest2','Gain2')
groupingLevels <- c(20,30, 50,60)
groupingLevels <- c(10, 20,30, 40,50, 60,70)
colName <- 'ClusterMod2'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,'Performance by Cluster',5,-0.5, FALSE)
p2A <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,'Performance by Cluster',5,-0.5)
png(file="../images/temp/Mod2Cluster2.png",width=1500,height=800)
multiplot(p2,p2A, cols=2)
dev.off()

fit<- aov(dataSE$Pretest2~as.factor(dataSE$ClusterMod2), na.action = na.omit)
summary(fit)

fit<- aov(dataSE$Gain2~as.factor(dataSE$ClusterMod2), na.action = na.omit)
summary(fit)

plot(fit)
fit<- aov(dataSE$PerceivedAbility~as.factor(dataSE$ClusterMod2))
summary(fit)


##########################################################################################
#####################################Module # 3###########################################
##########################################################################################
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Mod 3'

groupingLabels <- c('Conceptual',  'Limited', 'Summarizer', 'Problem-oriented')
groupingLabels <-c('Reasoners','Conceptual','Procedural','Schematic', 'Limited','Summarizer','Problem-oriented')
#plotingVarsNames<- c('Perceived Ability', 'Pretest', 'Posttest', 'Gain')
#plotingVars <- c('PerceivedAbility', 'Pretest3', 'Posttest3', 'Gain3')
plotingVarsNames<- c('Perceived Ability', 'Pretest', 'Posttest', 'Gain3')
plotingVars <- c('PerceivedAbility', 'Pretest3', 'Posttest3', 'Gain3')

groupingLevels <- c(20,50, 60,70)
groupingLevels <- c(10, 20,30, 40,50, 60,70)
colName <- 'ClusterMod3'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,'Performance by Cluster',5,-0.5, FALSE)
png(file="../images/temp/Mod3Cluster.png",width=1000,height=800)
p1
dev.off()

png(file="../images/temp/Mod2AND3Cluster.png",width=1500,height=500)
#multiplot(p1,p2,p3, p4, cols=2)
multiplot(p2,p1,p2A, cols=3)
dev.off()


fit<- aov(dataSE$Pretest3~as.factor(dataSE$ClusterMod3), na.action = na.omit)
summary(fit)

fit<- aov(dataSE$Gain3~as.factor(dataSE$ClusterMod3), na.action = na.omit)
summary(fit)

plot(fit)
fit<- aov(dataSE$PerceivedAbility~as.factor(dataSE$ClusterMod3))
summary(fit)


##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Prior Experience
##########################################################################################
#####################################Module # 1###########################################
##########################################################################################
xLabel<- 'Measures Mod 1'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two', 'Three')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Post-Pre Gain')
plotingVars <- c('NumCategoriesMod1', 'NumExplanationsMod1', 'wordsMod1','Gain1')
groupingLevels <- c(0, 1,2, 3)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,
                'Number of Explanations by Prior Experience',10,-1)

png(file="../images/temp/Mod1Prior.png",width=1000,height=800)
p1
dev.off()

##########################################################################################
#####################################Module # 2###########################################
##########################################################################################
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

png(file="../images/temp/Mod2Prior.png",width=1000,height=800)
p1
dev.off()

##########################################################################################
#####################################Module # 3###########################################
##########################################################################################
xLabel<- 'Measures Mod 3'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Prior Courses'
groupingLabels <- c('Zero', 'One', 'Two', 'Three')
plotingVarsNames<- c('Number of Categories', 'Number of Explanations', 'Number of Words', 'Post-Pre Gain')
plotingVars <- c('NumCategoriesMod3', 'NumExplanationsMod3', 'wordsMod3','Gain3')
groupingLevels <- c(0, 1,2, 3)
colName <- 'PriorCourses'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel,
                'Number of Explanations by Prior Experience',10,-1)

png(file="../images/temp/Mod3Prior.png",width=1000,height=800)
p1
dev.off()



##########################################################################################
#  Number of Categories', 'Number of Explanations', 'Number of Words', 'Rubric Score Activity by Performance Groups
##########################################################################################
#####################################Module # 1###########################################
##########################################################################################
xLabel<- 'Measures Module 1'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words', 'Posttest')
plotingVars <- c('NumCategoriesMod1', 'NumExplanationsMod1', 'wordsMod1','Posttest1')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Perceived Ability', 10, 0, FALSE)
colName <- 'GroupPre1'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Pretest Score', 10, 0, FALSE)
colName <- 'GroupGain1'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Post-Pre Gain', 10, 0, FALSE)

colName <- 'GroupGain1'
p3A <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Post-Pre Gain', 10, 0, 'legend')
colName <- 'PriorCourses'
groupingLevels <- c(0,1,2)
groupingLabels <- c('Zero', 'One', 'Two +')
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Prior Courses', 10, 0, 'legend')
png(file="../images/temp/Mod1Performance.png",width=1900,height=500)
#multiplot(p1,p2,p3, p4, cols=2)
multiplot(p1,p2,p3,p3A, cols=4)
dev.off()


##########################################################################################
#####################################Module # 2###########################################
##########################################################################################
xLabel<- 'Measures Module 2'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words')
plotingVars <- c('NumCategoriesMod2', 'NumExplanationsMod2', 'wordsMod2')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(a) Perceived Ability', 10, 0, FALSE)
colName <- 'GroupPre2'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(b)Pretest Score', 10, 0, FALSE)
colName <- 'GroupGain2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(c) Post-Pre Gain', 10, 0, FALSE)
p3A <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(b) Post-Pre Gain', 10, 0, 'legend')

colName <- 'PriorCourses'

groupingLevels <- c(0,1,2)
groupingLabels <- c('Zero', 'One', 'Two +')
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Prior Courses')
png(file="../images/temp/Mod2Performance.png",width=1900,height=500)
#multiplot(p1,p2,p3, p4, cols=2)
multiplot(p1,p2,p3,p3A, cols=4)
dev.off()


##########################################################################################
#####################################Module # 3###########################################
##########################################################################################
xLabel<- 'Measures Module 3'
yLabel<- 'Normalized Number of Explanations'
fillLabel <- 'Level'
groupingLabels <- c('Low', 'Mid', 'High')
plotingVarsNames<- c('Categories', 'Explanations', 'Words')
plotingVars <- c('NumCategoriesMod3', 'NumExplanationsMod3', 'wordsMod3')
groupingLevels <- c(1,2,3)
colName <- 'GroupAbility'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(a) Perceived Ability', 10, 0, FALSE)
colName <- 'GroupGain2'
p3 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(c) Post-Pre Gain', 10, 0, FALSE)
colName <- 'GroupPre2'
p2 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(b) Pretest Score', 10, 0, FALSE)
p2A <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, '(c) Pretest Score', 10, 0, 'legend')
colName <- 'PriorCourses'
groupingLevels <- c(0,1,2)
groupingLabels <- c('Zero', 'One', 'Two +')
p4 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels, 
                groupingLevels, 'RdYlGn', xLabel, yLabel, fillLabel, 'Prior Courses', 10, 0, 'legend')
png(file="../images/temp/Mod3Performance.png",width=1900,height=500)
 multiplot(p1,p2,p3, p2A, cols=4)
#multiplot(p1,p2,p3, cols=3)
dev.off()
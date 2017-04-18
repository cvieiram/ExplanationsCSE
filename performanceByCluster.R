##########################################################################################
#########################################CPMSE############################################
##################################PERFORMANCE BY CLUSTER###################################

##########################################################################################
#####################################Activity #2#########################################
##########################################################################################
xLabel<- 'Performance Measures'
yLabel<- 'Normalized Average Scores'
fillLabel <- 'Explainers Act 2'
plotingVarsNames<- c('Perceived Ability', 'Course Grade', 'Midterm One', 'Midterm Two', 'Quiz Score')
plotingVars <- c('PerceivedAbility', 'CourseGrade', 'Midterm1', 'Midterm2','RubricAct5')

groupingLabels <- c('Problem-oriented', 'Reasoners',  'Procedural',  'Conceptual','Limited')
groupingLevels <- c(1,2,3,4,5)
colName <- 'ClusterAct5'
p1 <- plotMeans(dataSE, colName, plotingVars, plotingVarsNames, groupingLabels,
                groupingLevels, 'BrBG', xLabel, yLabel, fillLabel)
png(file="../images/Act2Cluster.png",width=1000,height=800)
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
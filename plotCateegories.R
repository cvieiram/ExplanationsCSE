##########################################################################################
#########################################CPMSE############################################

##########################################################################################
#####################################Activity #2#########################################
##########################################################################################
# Types of explanations by student - Activity 2
fileName <- "studentsAct2.csv"
expDF2 <- organizeStudentsExplanations(fileName, categoriesNew)
colnames(expDF2)<-c('Student', categoriesNew)
mergedData <- merge(expDF2, dataSE[,c('Student','GroupAbility', 'GroupMid1', 'GroupMid2', 'GroupGrade')], by='Student')
groupingVble <- 'GroupAbility'
fill<- 'Perceived Ability'
act2AbilityrPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF2), groupingVble), 
                                           fill,'Categories per Perceived Ability - Activity 2')

groupingVble <- 'GroupMid1'
fill<- 'Midterm 1 Performance'
act2M1Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF2), groupingVble),
                                     fill,'Categories per Midterm 1 - Activity 2')

groupingVble <- 'GroupMid2'
fill<- 'Midterm 2 Performance'
act2M2Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF2), groupingVble),
                                     fill,'Categories per Midterm 2 - Activity 2')

groupingVble <- 'GroupGrade'
fill<- 'Course Performance'
act2CoursePlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF2), groupingVble),
                                         fill,'Categories per Course Performace - Activity 2')


##########################################################################################
#####################################Activity #5#########################################
##########################################################################################
# Types of explanations by student - Activity 5
fileName <- "studentsAct5.csv"
expDF5 <- organizeStudentsExplanations(fileName, categoriesNew)
colnames(expDF5)<-c('Student', categoriesNew)
mergedData <- merge(expDF5, dataSE[,c('Student','GroupAbility', 'GroupMid1', 'GroupMid2', 'GroupGrade', 'Group2Act5')], by='Student')
groupingVble <- 'GroupAbility'
fill<- 'Perceived Ability'
act5AbilityrPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF5), groupingVble),
                                           fill,'Categories per Perceived Ability - Activity 5')

groupingVble <- 'GroupMid1'
fill<- 'Midterm 1 Performance'
act5M1Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF5), groupingVble),
                                     fill,'Categories per Midterm 1 - Activity 5')

groupingVble <- 'GroupMid2'
fill<- 'Midterm 2 Performance'
act5M2Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF5), groupingVble),
                                     fill,'Categories per Midterm 2 - Activity 5')

groupingVble <- 'GroupGrade'
fill<- 'Course Performance'
act5CoursePlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF5), groupingVble),
                                         fill,'Categories per Course Performace - Activity 5')

groupingVble <- 'Group2Act5'
fill<- 'Quiz 5 Performance'
act5Q5Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF5), groupingVble),
                                     fill,'Categories per Quiz 5 Performance - Activity 5')

png(file="../images/temp/categoriesQuiz5CPMSE.png",width=1400,height=500)
multiplot(act5Q5Plot, cols=1)
dev.off()
##########################################################################################
#####################################Activity #11#########################################
##########################################################################################
# Types of explanations by student - Activity 11
fileName <- "studentsAct11.csv"
expDF11 <- organizeStudentsExplanations(fileName, categoriesNew)
colnames(expDF11)<-c('Student', categoriesNew)
mergedData <- merge(expDF11, dataSE[,c('Student','GroupAbility', 'GroupMid1', 'GroupMid2', 'GroupGrade', 'Group2Act11')], by='Student')
groupingVble <- 'GroupAbility'
fill<- 'Perceived Ability'
act11AbilityrPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF11), groupingVble),
                                            fill,'Categories per Perceived Ability - Activity 11')

groupingVble <- 'GroupMid1'
fill<- 'Midterm 1 Performance'
act11M1Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF11), groupingVble),
                                      fill,'Categories per Midterm 1 - Activity 11')

groupingVble <- 'GroupMid2'
fill<- 'Midterm 2 Performance'
act11M2Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF11), groupingVble),
                                      fill,'Categories per Midterm 2 - Activity 11')

groupingVble <- 'GroupGrade'
fill<- 'Course Performance'
act11CoursePlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF11), groupingVble),
                                          fill,'Categories per Course Performace - Activity 11')

groupingVble <- 'Group2Act11'
fill<- 'Quiz 5 Performance'
act11Q11Plot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDF11), groupingVble),
                                       fill,'Categories per Quiz 11 Performance - Activity 11')

png(file="../images/temp/categoriesQuiz11CPMSE.png",width=1400,height=500)
multiplot(act11Q11Plot, cols=1)
dev.off()
##########################################################################################
###################################SAVE ALL PLOTS#########################################
##########################################################################################
png(file="../images/categoriesAbilityCPMSE.png",width=1000,height=1000)
multiplot(act2AbilityrPlot, act5AbilityrPlot, act11AbilityrPlot, cols=1)
dev.off()

png(file="../images/categoriesMid1CPMSE.png",width=1000,height=1000)
multiplot(act2M1Plot, act5M1Plot, act11M1Plot, cols=1)
dev.off()

png(file="../images/categoriesMid2CPMSE.png",width=1000,height=1000)
multiplot(act2M2Plot, act5M2Plot, act11M2Plot, cols=1)
dev.off()

png(file="../images/categoriesCourseGradeCPMSE.png",width=1000,height=1000)
multiplot(act2CoursePlot, act5CoursePlot, act11CoursePlot, cols=1)
dev.off()

png(file="../images/categoriesQuizCPMSE.png",width=1400,height=1000)
multiplot( act5Q5Plot, act11Q11Plot, cols=1)
dev.off()




##################THERMO####################################
# Types of explanations by student - Module 1
fileName <- "studentsMod1.csv"
expDFM1 <- organizeStudentsExplanations(fileName, categories)
colnames(expDFM1)<-c('Student', categories)

##########################################################################################
#######################################Module #2##########################################
##########################################################################################
# Types of explanations by student - Module 2
fileName <- "studentsMod2.csv"
expDFM2 <- organizeStudentsExplanations(fileName, categoriesNew)
colnames(expDFM2)<-c('Student', categoriesNew)

mergedData <- merge(expDFM2, dataSE[,c('Student','GroupAbility', 'GroupPre2', 'GroupGain2', 'GroupPost2')], by='Student')
groupingVble <- 'GroupGain2'
fill<- 'Post-Pre Gain Performance'
mod2GainPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDFM2), groupingVble),
                                       fill,'Categories per Gain Performance - Module 2', 'RdYlGn')
png(file="../images/temp/categoriesGain2Thermo.png",width=1400,height=500)
multiplot(mod2GainPlot, cols=1)
dev.off()

groupingVble <- 'GroupPost2'
fill<- 'Posttest Performance'
mod2PostPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDFM2), groupingVble),
                                       fill,'Categories per Posttest Performance - Module 2', 'RdYlGn')

groupingVble <- 'GroupPre2'
fill<- 'Pretest Gain Performance'
mod2PrePlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDFM2), groupingVble),
                                      fill,'Categories per Pretest Performance - Module 2', 'RdYlGn')


##########################################################################################
#######################################Module #3##########################################
##########################################################################################
# Types of explanations by student - Module 3
fileName <- "studentsMod3.csv"
expDFM3 <- organizeStudentsExplanations(fileName, categoriesNew)
colnames(expDFM3)<-c('Student', categoriesNew)
mergedData <- merge(expDFM3, dataSE[,c('Student','GroupAbility', 'GroupPre3', 'GroupGain3', 'GroupPost3')], by='Student')

groupingVble <- 'GroupGain3'
fill<- 'Post-Pre Gain Performance'
mod3GainPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDFM3), groupingVble),
                                       fill,'Categories per Gain Performance - Module 2', 'RdYlGn')
png(file="../images/temp/categoriesGain3Thermo.png",width=1400,height=500)
multiplot(mod3GainPlot, cols=1)
dev.off()

groupingVble <- 'GroupPost3'
fill<- 'Posttest Performance'
mod3PostPlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDFM3), groupingVble),
                                       fill,'Categories per Posttest Performance - Module 2', 'RdYlGn')

groupingVble <- 'GroupPre3'
fill<- 'Pretest Gain Performance'
mod3PrePlot <- plotCategoriesPerGroup(mergedData, groupingVble, c(colnames(expDFM3), groupingVble),
                                      fill,'Categories per Pretest Performance - Module 2', 'RdYlGn')


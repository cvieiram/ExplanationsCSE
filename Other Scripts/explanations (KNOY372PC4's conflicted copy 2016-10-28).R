

# Types of explanations by student - Activity 2
fileName <- "studentsAct2.csv"
expDF2 <- organizeStudentsExplanations(fileName, categoriesNew)

# Types of explanations by student - Activity 5
fileName <- "studentsAct5.csv"
expDF5 <- organizeStudentsExplanations(fileName, categoriesNew)


# Types of explanations by student - Activity 11
fileName <- "studentsAct11.csv"
expDF11 <- organizeStudentsExplanations(fileName, categoriesNew)

# Types of explanations by student - Module 1
fileName <- "studentsMod1.csv"
expDFM1 <- organizeStudentsExplanations(fileName, categoriesNew)

# Types of explanations by student - Module 2
fileName <- "studentsMod2.csv"
expDFM2 <- organizeStudentsExplanations(fileName, categoriesNew)

# Types of explanations by student - Module 3
fileName <- "studentsMod3.csv"
expDFM3 <- organizeStudentsExplanations(fileName, categoriesNew)


distributionKnow <- read.table("distributionTypesOfKnowledge.csv",header=TRUE,sep=",")
distributionKnowThermo <- read.table("distributionTypesOfKnowledgeThermo.csv",header=TRUE,sep=",")
pCPMSE <- plotKnowledgeDist(distributionKnow)
png(file="../images/temp/ToKCPMSE.png",width=1000,height=800)
pCPMSE
dev.off()

pTHERMO <- plotKnowledgeDist(distributionKnowThermo, 'THERMO')
png(file="../images/temp/ToKTHERMO.png",width=1000,height=800)
pTHERMO
dev.off()


# CPMSE - Categories within types of knowledge
categoriesType <- as.data.frame(colSums(rbind(expDF2,expDF5, expDF11)[,-1]))
categoriesType <- as.data.frame(cbind(rownames(categoriesType), categoriesType[,1]))
categoriesType$V2 <- as.numeric(levels(categoriesType$V2))[categoriesType$V2]
categoriesType$V1 <- factor(categoriesType$V1, levels=categoriesNew) 

LK <- cbind('Limited Knowledge',categoriesType[1:4,])
CK <- cbind('Conceptual Knowledge',categoriesType[5:9,])
PK <- cbind('Procedural Knowledge',categoriesType[10:11,])
SK <- cbind('Schematic Knowledge',categoriesType[12:17,])
TK <- cbind('Strategic Knowledge',categoriesType[18:21,])

#PlotCategoriesDist(cbind('All Categories',categoriesType), 'All Categories')
LKPlot <- plotCategoriesDist(LK, 'Limited Knowledge')
CKPlot <- plotCategoriesDist(CK, 'Conceptual Knowledge')
PKPlot <- plotCategoriesDist(PK, 'Procedural Knowledge')
SKPlot <- plotCategoriesDist(SK, 'Schematic Knowledge')
TKPlot <- plotCategoriesDist(TK, 'Strategic Knowledge')

png(file="../images/temp/CategoriesKnowledgeCPMSE.png",width=800,height=1000)
multiplot(LKPlot,CKPlot,TKPlot,PKPlot, SKPlot, cols=2)
dev.off()


# THERMO - Categories within types of knowledge
categoriesType <- as.data.frame(colSums(rbind(expDFM1,expDFM2, expDFM3)[,-1]))
categoriesType <- as.data.frame(cbind(rownames(categoriesType), categoriesType[,1]))
categoriesType$V2 <- as.numeric(levels(categoriesType$V2))[categoriesType$V2]
categoriesType$V1 <- factor(categoriesType$V1, levels=categoriesNew) 

LK <- cbind('Limited Knowledge',categoriesType[1:4,])
CK <- cbind('Conceptual Knowledge',categoriesType[5:9,])
PK <- cbind('Procedural Knowledge',categoriesType[10:11,])
SK <- cbind('Schematic Knowledge',categoriesType[12:17,])
TK <- cbind('Strategic Knowledge',categoriesType[18:21,])

#PlotCategoriesDist(cbind('All Categories',categoriesType), 'All Categories')
LKPlot <- plotCategoriesDist(LK, 'Limited Knowledge')
CKPlot <- plotCategoriesDist(CK, 'Conceptual Knowledge')
PKPlot <- plotCategoriesDist(PK, 'Procedural Knowledge')
SKPlot <- plotCategoriesDist(SK, 'Schematic Knowledge')
TKPlot <- plotCategoriesDist(TK, 'Strategic Knowledge')

png(file="../images/temp/CategoriesKnowledgeThermo.png",width=800,height=1000)
multiplot(LKPlot,CKPlot,TKPlot,PKPlot, SKPlot, cols=2)
dev.off()

distExplainers <- read.table("newDistributionExplainers.csv",header=TRUE,sep=",")
colorList<-colorsCPMSE
explainersPlot <- plotExplainerDist(distExplainers)
png(file="../images/temp/ExplainerDistC.png",width=800,height=600)
explainersPlot
dev.off()

distExplainersThermo <- read.table("newDistributionExplainersThermo.csv",header=TRUE,sep=",")
colorList<-colorsCPMSE
explainersPlot <- plotExplainerDist(distExplainersThermo,'BrBG', 'Thermo')
png(file="../images/temp/ExplainerDistThermoC.png",width=800,height=600)
explainersPlot
dev.off()


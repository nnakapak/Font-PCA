# MATH 6350 HW 3

################################ Data Cleaning #################################

attach(BODONI)
library(dplyr) # Package for subseting data
bodoni_clean <- select(BODONI,-c(2,3,6,7,8,9,10,11,12)) # Discard the 9 columns
# View(bodoni_clean)
# 3964x403

attach(REFERENCE)
ref_clean <- select(REFERENCE,-c(2,3,6,7,8,9,10,11,12))
# View(ref_clean)
# 4652x403

attach(ROMAN)
roman_clean <- select(ROMAN,-c(2,3,6,7,8,9,10,11,12))
# View(roman_clean)
# 4776x403

# Discarding row containing missing numerical data
BODONI_clean <- na.omit(bodoni_clean)
REF_clean <- na.omit(ref_clean)
ROMAN_clean <- na.omit(roman_clean)

# Defining three classes of images of normal characters
# cl1 = all rows of BODONI_clean.csv file for which (strength = 0.4 and italic =0)
# cl2 = all rows of REF_clean.csv file for which (strength = 0.4 and italic =0)
# cl3 = all rows of ROMAN_clean.csv file for which (strength = 0.4 and italic =0)

BODONI_clean <- data.frame(BODONI_clean)#creating a data frame to add conditional statements to filter out non needed i features
BODONI_clean$CL = ifelse((BODONI_clean$strength == 0.4 & BODONI_clean$italic == 0),"CL1","NA")
BODONI_CLEAN = BODONI_clean[which(BODONI_clean$CL =="CL1"),] #labeling the new filter data as cl1
# 404 columns
# 991 rows

REF_clean <- data.frame(REF_clean)
REF_clean$CL = ifelse((REF_clean$strength == 0.4 & REF_clean$italic == 0),"CL2","NA")
REF_CLEAN = REF_clean[which(REF_clean$CL =="CL2"),]
# 404 columns
# 1163 rows

ROMAN_clean <- data.frame(ROMAN_clean)
ROMAN_clean$CL = ifelse((ROMAN_clean$strength == 0.4 & ROMAN_clean$italic == 0),"CL3","NA")
ROMAN_CLEAN = ROMAN_clean[which(ROMAN_clean$CL =="CL3"),]
# 404 columns
# 1194 rows

# Combine CL1, CL2, CL3 into DATA
DATA <- rbind(BODONI_CLEAN,REF_CLEAN,ROMAN_CLEAN)
# View(DATA)

# Binded all 3 data sets to a full data set (DATA) which is the union of 3 classes (CL1, CL2, CL3)
# where N = 716
# 404 columns

# Create standardize data set without font, strength, and italics (include CL)
library(standardize)
SFONT <- DATA %>% mutate_if(is.numeric, function (x) as.vector(scale(x))) # scaling by (xj-mj)/sd
SFONT = SFONT[,-c(1,2,3)] # taking out numerical functions of font, strength, and italics
# View(SFONT)
# 401 columns
# 716 rows

# Create data set for correlation matrix
CDATA <- data.matrix(SFONT) # creating it into a data matrix for correlation matrix beforehand
CDATA = scale(CDATA[,-c(401)]) #  removing non-numerical values CL and standardize the set

############################## Correlation matrix ##############################

cor(CDATA)
cor.df = data.frame(cor(CDATA)) # renaming to view actual full matrix
write.table(cor.df, file = "HW3_CorrMatrix.csv", sep = ",", row.names = TRUE)

#################################### PCA #######################################

# Compute PCA
library(factoextra)
font.pca <- prcomp(SFONT[,-c(401)],scale = TRUE) # pca of SFONT without CL

# Extracting eigenvalues from corr matrix
CCOR_font <- eigen(cor.df)$values
write.table(CCOR_font, file = "HW3_Eigenvalues.csv", sep = ",", row.names = TRUE)

# Computing eigenvectors
WCOR_font <- eigen(cor.df)$vectors
write.table(WCOR_font, file = "HW3_Eigenvectors.csv", sep = ",", row.names = TRUE)

# Transposing vectors
TCOR_font <- t(WCOR_font)

# Extract PC, eigenvalues, variance %, and cumulative variance %
get_eig(font.pca) # Dim = 108 for 95% cutoff point

############################## Plot eigenvalues vs r  ##########################

fviz_eig(font.pca, choice = "eigenvalue", addlabels=TRUE, 
         main = "Eigenvalue vs r", barfill = rainbow(10))

#################################### PVE vs r ##################################

# How to find PVE
pr.var = font.pca$sdev^2 # match with eigenvalues vs r plot
PVE = pr.var/sum(pr.var) # match with the PVE vs r plot

# Plot % of variance explained PVE vs r  (Scree Plot)
fviz_eig(font.pca,ncp = 28, # number of dim to be shown
         addlabels=TRUE, hjust = 0.1, # horiz adjustment of labels
         barfill=rainbow(28), barcolor ="darkblue",
         linecolor ="black") + ylim(0, 20) +
  theme_minimal()

################################# Contributions ################################

# Total contribution of features to 1:114 dimensions
fviz_contrib(font.pca, choice = "var",axes = 1:108,top = 108,fill = "lightblue")

######################### Original train and test sets #########################

# BEFORE PCA TAKE OUT FEATURES
# Creating the 80% random train set interval by taking ONLY using CL1, we replicate 
# this for the other CL functions
CL1 = SFONT[which(SFONT$CL =="CL1"),]
n <- nrow(CL1[which(CL1$CL=="CL1"),])
trainset <- sample(1:n, 0.8*n)
trainsetcl1 <- CL1[trainset,]
testsetcl1 <- CL1[-trainset,]

CL2 = SFONT[which(SFONT$CL =="CL2"),]
n <- nrow(CL2[which(CL2$CL=="CL2"),])
trainset <- sample(1:n, 0.8*n)
trainsetcl2 <- CL2[trainset,]
testsetcl2 <- CL2[-trainset,]

CL3 = SFONT[which(SFONT$CL =="CL3"),]
n <- nrow(CL3[which(CL3$CL=="CL3"),])
trainset <- sample(1:n, 0.8*n)
trainsetcl3 <- CL3[trainset,]
testsetcl3 <- CL3[-trainset,]

# Combining the sets to full train and test sets
TRAIN_SET <-rbind(trainsetcl1,trainsetcl2,trainsetcl3)
TEST_SET <- rbind(testsetcl1,testsetcl2,testsetcl3)
# View(TRAIN_SET)
# View(TEST_SET)

# Train and test labels
library(class)
SFONT_no <- SFONT[,-c(401)]
SFONT_label <- SFONT[,"CL"]
TRAIN_no <- TRAIN_SET[,-c(401)]
TRAIN_label <- TRAIN_SET[, "CL"]
TEST_no <- TEST_SET[,-c(401)]
TEST_label <- TEST_SET[,"CL"]

################################# Find best K ##################################

set.seed(1)
i=1
k.optm=1
for (i in seq(5, 100, by = 5)){
  knn.mod<- knn(train = TRAIN_no, test = TEST_no, cl = TRAIN_label, k= i)
  k.optm[i] <- 100 * sum(TEST_label == knn.mod)/ NROW(TEST_label)
  k=i
  cat(k,"=", k.optm[i],'\n')
}
# Best K = 5 

################################## KNN before PCA ###############################

# BEFORE PCA TAKE OUT FEATURES
set.seed(1)
knn.predtrain3 <- knn(train=TRAIN_no,
                       test=TRAIN_no,
                       cl = TRAIN_label,
                       k=5)

set.seed(1)
knn.predtest3 <- knn(train=TRAIN_no,
                      test=TEST_no,
                      cl = TRAIN_label,
                      k=5)

mean(knn.predtrain3 == TRAIN_label) # 0.8827045 
mean(knn.predtest3 == TEST_label) # 0.8256334

# Displaying confusion matrix
train_cm <- table(data.frame(knn.predtrain3, TRAIN_label))
print(train_cm)
test_cm <- table(data.frame(knn.predtest3, TEST_label))
print(test_cm)

###################### New train and test sets after PCA #######################

NewFeatures<-TCOR_font[,]
n=SFONT[,-401]# taking out class column
n=data.matrix(t(n)) #tranposing the dataset

pcad<-NewFeatures%*%n #multiplying transpose correlation matrix with dataset
pcad<-(pcad[1:108,])#108x3348 taking out until dimension 108
pcad = t(pcad) #untranpose
pcadataset=cbind(pcad,SFONT[,401]) # Binding class back into this
write.table(pcadataset, file = "HW3_PC.csv", sep = ",", row.names = TRUE)
pcadataset=data.frame(pcadataset) # dataframe

#splitting each class into 80% train and 20% test

CL1pca = pcadataset[which(pcadataset$X109 =="CL1"),]
n<-nrow(CL1pca[which(CL1pca$X109=="CL1"),])
trainset<-sample(1:n, 0.8*n)

trainsetcl1pca <- CL1pca[trainset,]
testsetcl1pca <- CL1pca[-trainset,]

CL2pca = pcadataset[which(pcadataset$X109 =="CL2"),]
n<-nrow(CL2pca[which(CL2pca$X109=="CL2"),])
trainset<-sample(1:n, 0.8*n)

trainsetcl2pca <- CL2pca[trainset,]
testsetcl2pca <- CL2pca[-trainset,]

CL3pca = pcadataset[which(pcadataset$X109 =="CL3"),]
n<-nrow(CL3pca[which(CL3pca$X109=="CL3"),])
trainset<-sample(1:n, 0.8*n)

trainsetcl3pca <- CL3pca[trainset,]
testsetcl3pca <- CL3pca[-trainset,]

# Combining the sets to full trainset and testset
TRAIN_SETpca<-rbind(trainsetcl1pca,trainsetcl2pca,trainsetcl3pca)
TEST_SETpca<- rbind(testsetcl1pca,testsetcl2pca,testsetcl3pca)
#View(TRAIN_SETpca)
#View(TEST_SETpca)

# Train and test labels
library(class)
TRAIN_nopca <- TRAIN_SETpca[,-c(109)]
TRAIN_labelpca <- TRAIN_SETpca[,109]
TEST_nopca <- TEST_SETpca[,-c(109)]
TEST_labelpca <- TEST_SETpca[,109]

################################# KNN after PCA ################################

#running KNN on pca dimensions as k =5
set.seed(1)
knn.predtrainpca <- knn(train=TRAIN_nopca,
                        test=TRAIN_nopca,
                        cl = TRAIN_labelpca,
                        k=5)



set.seed(1)
knn.predtestpca <- knn(train=TRAIN_nopca,
                       test=TEST_nopca,
                       cl = TRAIN_labelpca,
                       k=5)



mean(knn.predtrainpca== TRAIN_labelpca) # 0.8827045
mean(knn.predtestpca == TEST_labelpca) # 0.8360656

# Displaying confusion matrix
train_cmpca <- table(data.frame(knn.predtrainpca, TRAIN_labelpca))
print(train_cmpca)
test_cmpca <- table(data.frame(knn.predtestpca, TEST_labelpca))
print(test_cmpca)

################################# Scatter Plots #################################

# Compute 6 color graphic scatterplot displays of your 3 classes in the 6 planes
# (Y1,Y2) (Y1,Y3) (Y1,Y4) (Y2,Y3) (Y2,Y4) (Y3,Y4)

# Graph individuals with a similar profile together (Dim 1,2)
fviz_pca_ind(font.pca, axes = c(1,2),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$CL, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
# Graph individuals with a similar profile together (Dim 1,3)
fviz_pca_ind(font.pca, axes = c(1,3),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$CL, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
# Graph individuals with a similar profile together (Dim 1,4)
fviz_pca_ind(font.pca, axes = c(1,4),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$CL, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
# Graph individuals with a similar profile together (Dim 2,3)
fviz_pca_ind(font.pca, axes = c(2,3),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$CL, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
# Graph individuals with a similar profile together (Dim 2,4)
fviz_pca_ind(font.pca, axes = c(2,4),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$CL, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)
# Graph individuals with a similar profile together (Dim 3,4)
fviz_pca_ind(font.pca, axes = c(3,4),
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = DATA$CL, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)



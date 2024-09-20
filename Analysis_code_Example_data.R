#
rm(list=ls())

library(shapr)
library(ggplot2)
library(shapviz)
library(ranger)
library(missRanger)
library(kernelshap)
# library(collapse) # only needed for example data

seed_to_set<-349867


# Study data not publicly available - Use the built-in "iris" data set as example
df.iris<-iris 
lcol<-ncol(df.iris) # Make sure that the last column is outcome

# introduce NA:s in x-variables to resemble real data
set.seed(seed_to_set)

df.iris.nas<-cbind(collapse::na_insert(df.iris[,1:lcol-1],prop=0.1),df.iris[,lcol]) 
colnames(df.iris.nas)[lcol]<-"Species" # Set the correct variable name
summary(df.iris.nas) # Verify that there are NA:s

# Imputation using the missRanger package
df.iris.impx<-missRanger(df.iris.nas[,-lcol])
df.iris.imp<-cbind(df.iris.impx,df.iris.nas[,lcol])
colnames(df.iris.imp)[lcol]<-"Species" # Again, set the correct variable name
summary(df.iris.imp) # Verify that NA:s have been filled in

# Specify the Explanatory variables/Features for the set of subjects for which to calculate Shap values
# Here, all subject are selected
x_test<-df.iris.imp[,-lcol]

set.seed(seed_to_set)
# Classification forest with mostly default settings, except number of trees.
rfr<-ranger(x=df.iris.imp[,-lcol],y=df.iris.imp[,lcol],probability = TRUE,num.trees = 5000)
# Check prediction to see that no error messages appear since Shapley value calculation will use predictions
pred.rfr <- predict(rfr, data = x_test) 

# estimate variable importance using corrected method from Nembrini et al. (2018).
# For prediction it is recommended to not use the "impurity_corrected" option, 
# thus a separate run for the variable importance estimation
set.seed(seed_to_set)
rfr2<-ranger(x=df.iris.imp[,-lcol],y=df.iris.imp[,lcol],probability = TRUE,importance = "impurity_corrected",num.trees = 5000) #
importance(rfr2)
df.importance<-data.frame(importance(rfr2))
df.importance$variable<-rownames(df.importance)
# Calculate p-values for importance values
# For example data, Altmann´s method must be used. For study data, default options work.
# 
set.seed(seed_to_set)
df.importance.pval<-data.frame(importance_pvalues(rfr2,method="altmann",formula= Species ~. , data = df.iris.imp))
df.importance.pval$variable<-rownames(df.importance.pval)
colnames(df.importance)[1]<-"importance"  
df.importance2<-merge(df.importance,df.importance.pval,by=c("variable","importance"))
df.importance2$p_val_FDR<-p.adjust(df.importance2$pvalue,method="fdr")

# Calculate Shapley values using kernelshap
# This is time consuming for the real data, hours or even days... 
# A minute or two for the example data
set.seed(seed_to_set)
shp<-kernelshap(rfr,X=x_test,bg_X = x_test)
# Prepare for plotting using Shapviz
shp2<-shapviz(shp)

sv_waterfall(shp2$versicolor, row_id = 93) 
sv_force(shp2$setosa, row_id = 93)
sv_importance(shp2$setosa) 
sv_importance(shp2$virginica,kind="both")



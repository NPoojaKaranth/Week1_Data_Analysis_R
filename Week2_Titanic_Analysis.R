# WEEK 2: TITANIC STATISTICAL ANALYSIS & PREDICTIVE MODELING
# Reuses titanic_data.csv from Week 1. Run from the repository folder.
set.seed(123)
packages <- c("ggplot2", "rmarkdown", "knitr")
for (p in packages) if (!requireNamespace(p, quietly=TRUE)) install.packages(p, repos="https://cloud.r-project.org")
library(ggplot2)
dir.create("outputs/plots", recursive=TRUE, showWarnings=FALSE)
dir.create("outputs/tables", recursive=TRUE, showWarnings=FALSE)
if (!file.exists("titanic_data.csv")) stop("Put this script beside titanic_data.csv.")
d <- read.csv("titanic_data.csv", stringsAsFactors=FALSE)
need <- c("Survived","Pclass","Sex","Age","SibSp","Parch","Fare","Embarked")
if (!all(need %in% names(d))) stop("The Titanic CSV is missing required columns.")
d <- d[, need]
d$Survived <- as.integer(d$Survived)
d$Pclass <- factor(d$Pclass); d$Sex <- factor(d$Sex)
d$Embarked[d$Embarked==""] <- NA
d$Embarked <- factor(d$Embarked)
write.csv(data.frame(Variable=names(d), Missing=sum(is.na(d)), MissingPercent=round(colMeans(is.na(d))*100,2)),
          "outputs/tables/missing_values.csv", row.names=FALSE)
capture.output(summary(d), file="outputs/summary_statistics.txt")
# EDA plots
p <- ggplot(d,aes(x=factor(Survived,levels=c(0,1),labels=c("Did not survive","Survived"))))+
  geom_bar()+labs(title="Titanic Survival Counts",x="Outcome",y="Passengers")+theme_minimal()
ggsave("outputs/plots/survival_counts.png",p,width=7,height=5)
p <- ggplot(d,aes(x=Sex,fill=factor(Survived)))+geom_bar(position="fill")+
  labs(title="Survival Proportion by Sex",y="Proportion",fill="Survived (0/1)")+theme_minimal()
ggsave("outputs/plots/survival_by_sex.png",p,width=7,height=5)
p <- ggplot(d,aes(x=factor(Pclass),y=Fare))+geom_boxplot()+
  labs(title="Fare by Passenger Class",x="Passenger class",y="Fare")+theme_minimal()
ggsave("outputs/plots/fare_by_class.png",p,width=7,height=5)
p <- ggplot(d,aes(x=Age))+geom_histogram(bins=30,na.rm=TRUE)+theme_minimal()+labs(title="Age Distribution")
ggsave("outputs/plots/age_distribution.png",p,width=7,height=5)
# Hypothesis tests: H0 sex independent of survival; H0 survival proportions equal by sex;
# H0 age distributions equal between survivors and non-survivors.
sex_tab <- table(d$Sex,d$Survived)
sex_test <- suppressWarnings(chisq.test(sex_tab))
if(any(sex_test$expected<5)) sex_test <- fisher.test(sex_tab)
female <- d[d$Sex=="female",]; male <- d[d$Sex=="male",]
prop_test <- prop.test(c(sum(female$Survived==1,na.rm=TRUE),sum(male$Survived==1,na.rm=TRUE)),
                       c(sum(!is.na(female$Survived)),sum(!is.na(male$Survived))),correct=FALSE)
age_test <- wilcox.test(Age~Survived,data=d,exact=FALSE)
capture.output({cat("Sex vs survival test\n");print(sex_tab);print(sex_test);
  cat("\nDifference in survival proportions by sex\n");print(prop_test);
  cat("\nAge by survival: Wilcoxon rank-sum test\n");print(age_test)},
  file="outputs/hypothesis_tests.txt")
write.csv(cor(d[,c("Age","SibSp","Parch","Fare")],use="pairwise.complete.obs",method="spearman"),
          "outputs/tables/spearman_correlations.csv")
capture.output({for(v in c("Age","SibSp","Parch","Fare")) {
 x<-na.omit(d[[v]]); if(length(x)>5000)x<-sample(x,5000)
 if(length(x)>=3){cat("\nShapiro-Wilk:",v,"\n");print(shapiro.test(x))}
}},file="outputs/normality_checks.txt")
# Stratified 80/20 split. Impute predictors using training data only.
set.seed(123)
idx <- unlist(lapply(split(seq_len(nrow(d)),d$Survived),function(z)sample(z,max(1,floor(.8*length(z))))))
tr0 <- d[idx,]; te0 <- d[-idx,]
impute <- function(a,b) {
 for(v in c("Age","Fare","SibSp","Parch")) {m<-median(a[[v]],na.rm=TRUE);a[[v]][is.na(a[[v]])]<-m;b[[v]][is.na(b[[v]])]<-m}
 modev<-names(which.max(table(a$Embarked,useNA="no")))
 if(length(modev)==0)modev<-"S"
 a$Embarked[is.na(a$Embarked)]<-modev;b$Embarked[is.na(b$Embarked)]<-modev
 for(v in c("Pclass","Sex","Embarked")) {a[[v]]<-factor(a[[v]]);b[[v]]<-factor(b[[v]],levels=levels(a[[v]]));b[[v]][is.na(b[[v]])]<-levels(a[[v]])[1]}
 list(a=a,b=b)
}
z<-impute(tr0,te0);tr<-z$a;te<-z$b
fit<-glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=tr,family=binomial())
prob<-predict(fit,newdata=te,type="response");pred<-as.integer(prob>=.5);actual<-te$Survived
cm<-table(Actual=factor(actual,levels=0:1),Predicted=factor(pred,levels=0:1))
write.csv(as.data.frame.matrix(cm),"outputs/tables/confusion_matrix.csv")
tp<-cm[2,2];fp<-cm[1,2];fn<-cm[2,1];tn<-cm[1,1]
precision<-if(tp+fp==0)NA else tp/(tp+fp);recall<-if(tp+fn==0)NA else tp/(tp+fn)
metrics<-data.frame(Metric=c("Accuracy","Precision","Recall/Sensitivity","Specificity","F1 score"),
 Value=round(c((tp+tn)/sum(cm),precision,recall,if(tn+fp==0)NA else tn/(tn+fp),
 if(is.na(precision)||is.na(recall)||precision+recall==0)NA else 2*precision*recall/(precision+recall)),4))
write.csv(metrics,"outputs/tables/test_metrics.csv",row.names=FALSE)
write.csv(data.frame(Actual=actual,Predicted=pred,SurvivalProbability=round(prob,4)),
          "outputs/tables/test_predictions.csv",row.names=FALSE)
capture.output(summary(fit),file="outputs/logistic_regression_summary.txt")
# 5-fold CV on training rows; stratified folds, with fold-specific imputation.
set.seed(123); fold<-integer(nrow(tr0))
for(cl in unique(tr0$Survived)){ii<-which(tr0$Survived==cl);fold[ii]<-sample(rep(1:5,length.out=length(ii)))}
cv<-data.frame(Fold=1:5,Accuracy=NA,Precision=NA,Recall=NA,F1=NA)
for(k in 1:5){
 a0<-tr0[fold!=k,];b0<-tr0[fold==k,];zz<-impute(a0,b0)
 mm<-glm(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked,data=zz$a,family=binomial())
 pp<-as.integer(predict(mm,newdata=zz$b,type="response")>=.5);yy<-zz$b$Survived
 cc<-table(factor(yy,levels=0:1),factor(pp,levels=0:1));t<-cc[2,2];f<-cc[1,2];n<-cc[2,1]
 pr<-if(t+f==0)NA else t/(t+f);re<-if(t+n==0)NA else t/(t+n)
 cv[k,2:5]<-c(sum(diag(cc))/sum(cc),pr,re,if(is.na(pr)||is.na(re)||pr+re==0)NA else 2*pr*re/(pr+re))
}
write.csv(cv,"outputs/tables/five_fold_cv_metrics.csv",row.names=FALSE)
write.csv(data.frame(Metric=names(cv)[-1],Mean=sapply(cv[-1],mean,na.rm=TRUE),SD=sapply(cv[-1],sd,na.rm=TRUE)),
 "outputs/tables/cv_summary.csv",row.names=FALSE)
png("outputs/plots/confusion_matrix.png",800,650);fourfoldplot(cm,color=c("gray80","gray50"),main="Confusion Matrix");dev.off()
png("outputs/plots/model_diagnostics.png",1000,800);par(mfrow=c(2,2));plot(fit,which=1);plot(fit,which=4);plot(fit,which=5);hist(residuals(fit,type="deviance"),main="Deviance residuals",xlab="Residual");dev.off()
capture.output({cat("Held-out test metrics:\n");print(metrics);cat("\n5-fold CV:\n");print(cv);
 cat("\nModel summary:\n");print(summary(fit))},file="outputs/run_summary.txt")
# Render a Word report if R Markdown is available.
if(requireNamespace("rmarkdown",quietly=TRUE) && file.exists("Week2_Titanic_Report.Rmd")) {
 try(rmarkdown::render("Week2_Titanic_Report.Rmd",output_file="Week2_Titanic_Report.docx",quiet=TRUE),silent=TRUE)
}
cat("Finished. Review outputs/ and Week2_Titanic_Report.docx (if rendered).\n")

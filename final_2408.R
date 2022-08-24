library(readr)
library(dplyr)
library(quantreg)
library(pls)
library(lubridate)
library(Cubist)

setwd("E:/R/_cp2022/tula")
train<-read_csv2("train_dataset_train.csv")
train$VISIT_MONTH_YEAR<-as.Date(paste0("01.", train$VISIT_MONTH_YEAR), format = "%d.%m.%y")
train<-train%>%arrange(desc(VISIT_MONTH_YEAR))

test<-read_csv2("test_dataset_test.csv")
test$VISIT_MONTH_YEAR<-as.Date(paste0("01.", test$VISIT_MONTH_YEAR), format = "%d.%m.%y")

sample<-read_csv2("sample_solution.csv")

#On test
valDate<-"2022-04-01"#for validation 2022-03-01 or 2022-02-01

val0<-test#train%>%filter(VISIT_MONTH_YEAR==valDate) for validation
tr<-train%>%filter(VISIT_MONTH_YEAR<valDate)
tr2<-tr%>% semi_join((val0%>%select (PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY)))

tr0.grp<-tr2%>%group_by(PATIENT_SEX, MKB_CODE,ADRES,AGE_CATEGORY)%>%
  summarise(last=first(PATIENT_ID_COUNT),
            sd=sd(PATIENT_ID_COUNT))
tr0.grp$sd[is.na(tr0.grp$sd)]<-0

tr3<-tr2%>%left_join(tr0.grp)%>%filter(sd>0)

val0$id<-1:nrow(val0)
val0<-left_join(val0, tr0.grp)
val0$last[is.na(val0$last)]<-1

val2<-val0%>%semi_join((tr3%>%select (PATIENT_SEX, MKB_CODE, ADRES, AGE_CATEGORY)))
##########3
all<-rbind(tr3[,c(1:5,7)],val2[,c(1:5,7)])
all$MKB_CODE<-as.factor(all$MKB_CODE)
all$ADRES<-as.factor(as.numeric(as.factor(all$ADRES)))
all$AGE_CATEGORY<-as.factor(all$AGE_CATEGORY)
all$year<-as.factor(year(all$VISIT_MONTH_YEAR))
all$month<-as.factor(month(all$VISIT_MONTH_YEAR))
all$H<-ifelse(all$VISIT_MONTH_YEAR=="2020-04-01",1,0)
all<-all%>%select(-VISIT_MONTH_YEAR)

tr0<-all[1:nrow(tr3),]
valnew<-all[(1+nrow(tr3)):nrow(all),]

Y<-tr3$PATIENT_ID_COUNT

set.seed(123)
model2<-cubist(x=tr0, y=Y, committees = 1) 
valnew$pred<-predict(model2,valnew)
valnew$id<-val2$id

valF<-val0%>%left_join(valnew[,c("pred","id")])
valF$pred[valF$pred<1|is.na(valF$pred)]<-1
sample$PATIENT_ID_COUNT<-round((valF$pred+valF$last)/2)
write_csv2(sample,"final_2408.csv")
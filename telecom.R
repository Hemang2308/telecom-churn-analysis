#Reading the DataSet
tele <-read.csv('C:\\DS Full stack\\Graded Assignments\\09 - Capstone Project  and Certitication\\telecomfinal.csv',header = TRUE,stringsAsFactors = TRUE)

#Creating Data Quality Report 
library(dataQualityR)

num_file<-paste(tempdir(),'Numerical_telecomfinal.csv',sep = "")
cat_file<-paste(tempdir(),'Categorical_telecomfinal.csv',sep = "")
checkDataQuality(data = tele,out.file.num = num_file,out.file.cat = cat_file)
num_file #Path for both of th files to check 

#Some Basic Sanity Checks
library(dplyr)        #For Data Manipulation 
options(scipen = 999) #For Scientific Notation 

#Column names with Ascending Order
colnames(tele)

#Churn has been calculated as categorical variable with two outcomes 0 and 1 .
#i.e. '1' = Churned and '0' = Not Churned
#Target or Dependent Variable = "CHURN"
summary(tele$churn)
tele$churn

#Unique Values in Each Variable of Dataset
unique(tele)

#Summary Check 
summary(tele)

#Checking for Missing Values in all the Columns 
is.na(tele)

#Total Missing Values in Dataset 
sum(is.na(tele))  #<-631020 numer of Missing Values

#To Get Datatypes of all Variables Available
str(tele)

# As we get following Variables as Factor 
#crclscod, asl_flag, prizm_social_one, area, refurb_new, hnd_webcap, marital, ethnic, dwlltype, dwllsize, mailordr, occu1
#wrkwoman,  solflag, proptype, mailresp, cartype, car_buy, children, csa, div_type .
#retdays contains high missing Values
#Missing Value Treatement for retdays and creating dummy Variables.
#some Values for retdays have numeric number that accounts that account is to be continuous variable 
#Missing Values for this Variable can be assumend to mean there have been no retention calls made by customer
summary(tele$retdays)
hist(tele$retdays)
str(tele$retdays)
is.na(tele$retdays)
sum(is.na(tele$retdays)) #<- 64143 number of missing values
sort(unique(tele$retdays),na.last = FALSE)
tele$retention<-ifelse(is.na(tele$retdays)==TRUE,0,1)
str(tele$retention) #Have Numeric Forms
summary(tele$retention)

#Since New Column Retention has Been Added so Checking for Column Names 
names(tele)
telecom <- tele[,colMeans(is.na(tele))<=0.15]
#14 variables rejected based on the percentage of missing values cutoff
#After Looking into Data Dictionary 
#Removing blck_dat_mean this variable is no longer useful
names(telecom)
telecom<-telecom[,-50]
names(telecom)
------------------------------------
#My Dataset has 67 Variables 
#Deciling continuous variables basis target variable churn 
str(telecom)
dim(telecom)

#Ratio od 0's and 1's in Complete Dataset
table(tele$churn)/nrow(tele)
table(telecom$churn)/nrow(telecom)
#Cut off Value for customers likely to churn is 0.2392114

#Continuous Variable Profilling 
#Denoting all Variables with prefix as data_(Name of Variable)

# 1.For mou_mean = data_mou_mean / Mean Number of Monthly  Mintues of Use
summary(telecom$mou_Mean)
#There is clear downword trend in event rate (churn) as mou_Mean increases
telecom%>%mutate(dec=ntile(mou_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(mou_Mean,na.rm = TRUE),LessThan=max(mou_Mean,na.rm = TRUE))->data_mou_Mean

# 2.For Variable totmrc_mean = data_totmrc_Mean / Monthly Recurring Charge is the base cost
#of the Calling Plan Regardless of Actual Minutes Used.
summary(telecom$totmrc_Mean)
#No Clear Trent
telecom%>%mutate(dec=ntile(totmrc_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(totmrc_Mean,na.rm = TRUE),LessThan=max(totmrc_Mean,na.rm = TRUE))->data_totmrc_Mean

# 3.For Variable rev_range=data_rev_Range / Range of Revenue(Charge Amount)
summary(telecom$rev_Range)
#No clear Trend
telecom%>%mutate(dec=ntile(rev_Range,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(rev_Range,na.rm = TRUE),LessThan=max(rev_Range,na.rm = TRUE))->data_rev_Range

# 4.For Variable mou_range = data_mou_Range / Range of number of Minutes of Use
summary(telecom$mou_Range)
#No Clear Trend 
telecom%>%mutate(dec=ntile(mou_Range,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(mou_Range,na.rm = TRUE),LessThan=max(mou_Range,na.rm = TRUE))->data_mou_Range

# 5.For Variable change_mou = data_change_mou / Percentage change in Monthly Minutes of use vs previous three month average
summary(telecom$change_mou)
#No Clear Trend 
telecom%>%mutate(dec=ntile(change_mou,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(change_mou,na.rm = TRUE),LessThan=max(change_mou,na.rm = TRUE))->data_change_mou

# 6.For Variable drop_blk_Mean = data_drop_blk_Mean / Mean Number of Dropped or Blocked Calls
summary(telecom$drop_blk_Mean)
#No Clear Trend 
telecom%>%mutate(dec=ntile(drop_blk_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(drop_blk_Mean,na.rm = TRUE),LessThan=max(drop_blk_Mean,na.rm = TRUE))->data_drop_blk_mean

# 7.For Variable drop_vce_range = data_drop_vce_Range / Range of Number of Dropped(Failed) voice calls
summary(telecom$drop_vce_Range)
#No Clear Trend 
telecom%>%mutate(dec=ntile(drop_vce_Range,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(drop_vce_Range,na.rm = TRUE),LessThan=max(drop_vce_Range,na.rm = TRUE))->data_drop_vce_Range

# 8.For Variable owylis_vce_Range = data_owylis_vce_Range / Range of number of Outbound Wireless to Wireless Voice Calls
summary(telecom$owylis_vce_Range)
#No Clear Trend 
telecom%>%mutate(dec=ntile(owylis_vce_Range,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(owylis_vce_Range,na.rm = TRUE),LessThan=max(owylis_vce_Range,na.rm = TRUE))->data_owylis_vce_Range

# 9.For Variable mou_opkv_Range = data_mou_opkv_Range / Range of unrounded minutes of use of off-peak voice calls
summary(telecom$mou_opkv_Range)
#Slightly Downwords trends in the Event Rate
telecom%>%mutate(dec=ntile(mou_opkv_Range,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(mou_opkv_Range,na.rm = TRUE),LessThan=max(mou_opkv_Range,na.rm = TRUE))->data_mou_opkv_Range

# 10.For Variable months = data_months / Total Number of Months in Service 
summary(telecom$months)
#No Clear Trend 
#One Well-Known Observation is that ,Many people (42%) tend to leave company between 10-12 months
telecom%>%mutate(dec=ntile(months,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(months,na.rm = TRUE),LessThan=max(months,na.rm = TRUE))->data_months

# 11.For Variable totcalls = data_totcalls / Total Number of calls over the life of the Customer
summary(telecom$totcalls)
#Slightly Increase Upward Trend in the event rate as totcalls increase
telecom%>%mutate(dec=ntile(totcalls,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(totcalls,na.rm = TRUE),LessThan=max(totcalls,na.rm = TRUE))->data_totcalls

# 12.For Variable eqpdays = data_eqpdays / Number of Days (Age) of current Equipment 
summary(telecom$eqpdays)
#Slightly Increase Upward Trend in the event rate as eqpdays
telecom%>%mutate(dec=ntile(eqpdays,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(eqpdays,na.rm = TRUE),LessThan=max(eqpdays,na.rm = TRUE))->data_eqpdays

# 13.For Variable custcare_Mean = data_custcare_Mean / Mean number of Customer care calls 
#Less than 4 Deciles
summary(telecom$custcare_Mean)
#Many Values are Zero and May not be Useful for the model 
telecom%>%mutate(dec=ntile(custcare_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(custcare_Mean,na.rm = TRUE),LessThan=max(custcare_Mean,na.rm = TRUE))->data_custcare_Mean
hist(telecom$custcare_Mean) #Positively Skewded

# 14.For Variable callwait_mean = data_callwait_mean / Mean Number of call waiting calls
summary(telecom$callwait_Mean)
#Many Values are Zero and May not be Useful for the model 
telecom%>%mutate(dec=ntile(callwait_Mean,4))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(callwait_Mean,na.rm = TRUE),LessThan=max(callwait_Mean,na.rm = TRUE))->data_callwait_Mean

# 15.For Variable iwylis_vce_mean = data_iwylis_vce_mean / Mean Number of inbound wireless voice calls 
summary(telecom$iwylis_vce_Mean)
telecom%>%mutate(dec=ntile(iwylis_vce_Mean,6))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(iwylis_vce_Mean,na.rm = TRUE),LessThan=max(iwylis_vce_Mean,na.rm = TRUE))->data_iwylis_vce_Mean

# 16.For Variable callwait_range = data_callwait_Range/ Range of Number of call Waiting Calls
#Medain is Zero which shows 50% is data is Zero 
summary(telecom$callwait_Range)
#Many Values are Zero and May not be Useful for the model 
telecom%>%mutate(dec=ntile(callwait_Range,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(callwait_Range,na.rm = TRUE),LessThan=max(callwait_Range,na.rm = TRUE))->data_callwait_Range

# 17.For Variable ccrndmou_Range = data_ccrndmou_Range / Mean rounded minutes of use of customer care calls
#Medain is Zero
#Need to Omitted
summary(telecom$ccrndmou_Range)
#Slightly Increase Upward Trend in the event rate  as ccrndmou_Range
telecom%>%mutate(dec=ntile(ccrndmou_Range,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(ccrndmou_Range,na.rm = TRUE),LessThan=max(ccrndmou_Range,na.rm = TRUE))->data_ccrndmou_Range

# 18.For Variable adjqty = data_adjqty / Billing Adjusted total number of calls over life of customer
summary(telecom$adjqty)
#Slightly Increase Upward Trend in the event rate 
telecom%>%mutate(dec=ntile(adjqty,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(adjqty,na.rm = TRUE),LessThan=max(adjqty,na.rm = TRUE))->data_adjqty

# 19.For Variable overrev_mean = data_overrev_mean /Mean overage revenue
summary(telecom$ovrrev_Mean)
#No Clear Trend
telecom%>%mutate(dec=ntile(ovrrev_Mean,4))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(ovrrev_Mean,na.rm = TRUE),LessThan=max(ovrrev_Mean,na.rm = TRUE))->data_overrev_mean

# 20.For Variable rev_mean = data_rev_Mean / Men Monthly Revenue (Charge amount)
summary(telecom$rev_Mean)
#No clear Trend
telecom%>%mutate(dec=ntile(rev_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(rev_Mean,na.rm = TRUE),LessThan=max(rev_Mean,na.rm = TRUE))->data_rev_Mean

# 21.For Variable ovrmou_Mean = data_ovrmou_Mean / Mean Overage Minutes of Use
summary(telecom$ovrmou_Mean)
#Many Values are Zero ,and Maynot be usedful for the model 
telecom%>%mutate(dec=ntile(ovrmou_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(ovrmou_Mean,na.rm = TRUE),LessThan=max(ovrmou_Mean,na.rm = TRUE))->data_ovrmou_Mean

# 22.For Variable comp_vce_mean = data_comp_vce_Mean / Mean Number of Completed voice calls 
summary(telecom$comp_vce_Mean)
#Slightly Downwards trend in the event rate as comp_vce_mean
telecom%>%mutate(dec=ntile(comp_vce_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(comp_vce_Mean,na.rm = TRUE),LessThan=max(comp_vce_Mean,na.rm = TRUE))->data_comp_vce_Mean

# 23.For Variable plcd_vce_mean = data_plcd_vce_Mean / Mean Number of attempted voice calls placed 
summary(telecom$plcd_vce_Mean)
#Slightly Downwards trend in the event rate as plcd_vce_mean
telecom%>%mutate(dec=ntile(plcd_vce_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(plcd_vce_Mean,na.rm = TRUE),LessThan=max(plcd_vce_Mean,na.rm = TRUE))->data_plcd_vce_Mean

# 24.For Variable avg3mou = data_avg3mou / Average Monthly Minutes of Use Over the previous three Months 
summary(telecom$avg3mou)
#Slightly Downwards trend in the event rate as plcd_vce_mean
telecom%>%mutate(dec=ntile(avg3mou,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avg3mou,na.rm = TRUE),LessThan=max(avg3mou,na.rm = TRUE))->data_avg3mou

# 25.For Variable avgmou = data_avgmou / Average Monthly Minutes of Use Over the Life of the Customer
summary(telecom$avgmou)
#No Clear Trend
telecom%>%mutate(dec=ntile(avgmou,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avgmou,na.rm = TRUE),LessThan=max(avgmou,na.rm = TRUE))->data_avgmou

# 26.For Variable avg3qty = data_av3qty / Average Monthly Number of calls over previous three months 
summary(telecom$avg3qty)
#Clear Downward trend in the event rate as avg3qty
telecom%>%mutate(dec=ntile(avg3qty,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avg3qty,na.rm = TRUE),LessThan=max(avg3qty,na.rm = TRUE))->data_avg3qty

# 27.For Variable avgqty = data_avgqty / Average Monthly Number of calls over the life of the customer
summary(telecom$avgqty)
telecom%>%mutate(dec=ntile(avgqty,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avgqty,na.rm = TRUE),LessThan=max(avgqty,na.rm = TRUE))->data_avgqty

# 28.For Variable avg6mou = data_avg6mou / Average Monthly Minutes of use over the previous six months 
summary(telecom$avg6mou)
#Clear Downward trend in the event rate as avg6mou increased 
telecom%>%mutate(dec=ntile(avg6mou,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avg6mou,na.rm = TRUE),LessThan=max(avg6mou,na.rm = TRUE))->data_avg6mou

# 29.For Variable avg6qty = data_avg6qty / Average Monthly number of calls over the previous six months 
summary(telecom$avg6qty)
#Clear Downward trend in the event rate as avg6qty increased 
telecom%>%mutate(dec=ntile(avg6qty,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avg6qty,na.rm = TRUE),LessThan=max(avg6qty,na.rm = TRUE))->data_avg6qty
--------------------------------------------
# 39.For Variable age1=data_age1 / Age of  First Householder Member
#00=default 
#Other values signify a valid age
summary(telecom$age1)
#Many values are Zero but not useful for this model
telecom%>%mutate(dec=ntile(age1,6))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(age1,na.rm = TRUE),LessThan=max(age1,na.rm = TRUE))->data_age1
  
# 40.For Variable age2 = data_age / Age of Secong Householder Member
#00 = default 
#Other Values Signify a valid age
#Conatins less than 4 deciles , needs to be omited
summary(telecom$age2)
telecom%>%mutate(dec=ntile(age2,4))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(age2,na.rm = TRUE),LessThan=max(age2,na.rm = TRUE))->data_age2

# 41.For Variable modles = data_models  / Number of Models issued 
#contains less than 4 deciles ,need to be omited but Categorical Variable deal with it later 
summary(telecom$models)
telecom%>%mutate(dec=ntile(models,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(models,na.rm = TRUE),LessThan=max(models,na.rm = TRUE))->data_models

# 42,For Variable hnd_price = data_hnd_price / Current handset price ##
summary(telecom$hnd_price)
#It turns out to be categorical need to be deal with it later.
telecom%>%mutate(dec=ntile(hnd_price,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(hnd_price,na.rm = TRUE),LessThan=max(hnd_price,na.rm = TRUE))->data_hnd_price

# 43.For Variable actvsubs = data_actvsubs  / Number of Models issued 
#contains less than 4 deciles , but cannot declined its a categorical variable
summary(telecom$actvsubs)
telecom%>%mutate(dec=ntile(actvsubs,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(actvsubs,na.rm = TRUE),LessThan=max(actvsubs,na.rm = TRUE))->data_actvsubs

# 44.For Variable uniqsubs = data_uniqsubs / Number of Unique Subscribers in the Household
#getting less than 4 deciles , needs to be omit 
summary(telecom$uniqsubs)
telecom%>%mutate(dec=ntile(uniqsubs,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(uniqsubs,na.rm = TRUE),LessThan=max(uniqsubs,na.rm = TRUE))->data_uniqsubs

# 45.For Variable forgntvl = data_forgntvl / Foreign travel dummy variable 
#0 = No
#1 = Yes
#Contains less than 4 deciles ,needs to be omited but deal with it later as it is categorical Variable
summary(telecom$forgntvl)
telecom%>%mutate(dec=ntile(forgntvl,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(forgntvl,na.rm = TRUE),LessThan=max(forgntvl,na.rm = TRUE))->data_forgntvl#Median is Zero

# 46.For Variable opk_dat_Mean =data_opk_dat_Mean / MEan number of off-peak data calls
#Medain is Zero 
#Contains less than 4 deciles 
summary(telecom$opk_dat_Mean)
telecom%>%mutate(dec=ntile(opk_dat_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(opk_dat_Mean,na.rm = TRUE),LessThan=max(opk_dat_Mean,na.rm = TRUE))->data_opk_dat_Mean

# 47.For Variable mtrcycle and  48. For Variable truck both are Categorical but stored as Numeric value

# 49.For Variable roam_Mean = data_roam_Mean / Mean Number of roaming calls 
#contains less than 4 deciles , need to be omit. as many values are Zero
summary(telecom$roam_Mean)
telecom%>%mutate(dec=ntile(roam_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(roam_Mean,na.rm = TRUE),LessThan=max(roam_Mean,na.rm = TRUE))->data_roam_mean

# 50.For Variable recv_sms_Mean = data_recv_sms_Mean / Mean Number of Received  sms calls
#Contains less than 4 Deciles ,need to be omit as many values are zero also
summary(telecom$recv_sms_Mean)
 #Median is Zero
telecom%>%mutate(dec=ntile(recv_sms_Mean,4))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(recv_sms_Mean,na.rm = TRUE),LessThan=max(recv_sms_Mean,na.rm = TRUE))->data_recv_sms_Mean

# 51.For Variable mou_pead_Mean = data_mou_pead_Mean
summary(telecom$mou_pead_Mean)
#Median is Zero
telecom%>%mutate(dec=ntile(mou_pead_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(mou_pead_Mean,na.rm = TRUE),LessThan=max(mou_pead_Mean,na.rm = TRUE))->data_mou_pead_Mean

# 52.For Variable da_mean = data_da_Mean / Mean number of Directory assisted calls 
summary(telecom$da_Mean)
#Many values are zero not useful for model 
telecom%>%mutate(dec=ntile(da_Mean,4))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(da_Mean,na.rm = TRUE),LessThan=max(da_Mean,na.rm = TRUE))->data_da_mean 

# 53.For Variable da_Range = data_da_Range / RAnge of Number of Directory assisted calls 
summary(telecom$da_Range)
#Many values are zero cannot be useful for model
telecom%>%mutate(dec=ntile(da_Range,4))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(da_Range,na.rm = TRUE),LessThan=max(da_Range,na.rm = TRUE))->data_da_Range

# 54.For Variable datovr_mean =data_datovr_mean / Mean revenue of Data Overage  
#Contains less than 4 deciles , need to be omit and many values are zero also
summary(telecom$datovr_Mean)
telecom%>%mutate(dec=ntile(datovr_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(datovr_Mean,na.rm = TRUE),LessThan=max(datovr_Mean,na.rm = TRUE))->data_datovr_Mean

# 55.For Variable datovr_Range = data_datovr_Range / RAnge of revenue of data overage
#Median is Zero
#Contains less than 4 deciles ,need to be omit
summary(telecom$datovr_Range)
telecom%>%mutate(dec=ntile(datovr_Range,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(datovr_Range,na.rm = TRUE),LessThan=max(datovr_Range,na.rm = TRUE))->data_datovr_Range

# 56.For variable drop_dat_mean = data_drop_dat_Mean / mean number of dropped (failed) data calls 
#Contains less than 4 deciles ,need to be omit and many values are zero also .
summary(telecom$drop_dat_Mean)
telecom%>%mutate(dec=ntile(drop_dat_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(drop_dat_Mean,na.rm = TRUE),LessThan=max(drop_dat_Mean,na.rm = TRUE))->data_drop_dat_Mean

# 57.For Variable drop_vce_Mean = data_drop_vce_Mean / mean number of dropped (Failed) voice calls 
summary(telecom$drop_vce_Mean)
telecom%>%mutate(dec=ntile(drop_vce_Mean,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(drop_vce_Mean,na.rm = TRUE),LessThan=max(drop_vce_Mean,na.rm = TRUE))->data_drop_vce_Mean

# 58.For Variable adjmou = data_adjmou / Billing adjusted total minutes of use over the life of the customer 
summary(telecom$adjmou)
telecom%>%mutate(dec=ntile(adjmou,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(adjmou,na.rm = TRUE),LessThan=max(adjmou,na.rm = TRUE))->data_adjmou

# 59.For Variable totrev=data_totrev / Total Revenue
summary(telecom$totrev)
telecom%>%mutate(dec=ntile(totrev,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(totrev,na.rm = TRUE),LessThan=max(totrev,na.rm = TRUE))->data_totrev

# 60.For variable adjrev=data_adjrev / Billing adjusted total revenue over the life of the customer
summary(telecom$adjrev)
#Slight upward trends 
telecom%>%mutate(dec=ntile(adjrev,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(adjrev,na.rm = TRUE),LessThan=max(adjrev,na.rm = TRUE))->data_adj_rev

# 61.For Var5iable avgrev = data_avgrev / Average monthly revenue over the life of the customer 
summary(telecom$avgrev)
#No clear trend
telecom%>%mutate(dec=ntile(avgrev,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(avgrev,na.rm = TRUE),LessThan=max(avgrev,na.rm = TRUE))->data_avgrev

# 62.For Variable comp_dat_mean = data_comp_dat_mean / Mean number of completed data calls 
#has to relted with complete mean then omit
summary(telecom$comp_dat_Mean)
telecom%>%mutate(dec=ntile(comp_dat_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(comp_dat_Mean,na.rm = TRUE),LessThan=max(comp_dat_Mean,na.rm = TRUE))->data_comp_dat_Mean
#Medain is Zero 

# 63.For Variable plcd_dat_mean = data_plcd_dat_Mean / Mean number of attempted data calls placed 
#has to relted with plcd attempt_mean then  omit.
summary(telecom$plcd_dat_Mean)
telecom%>%mutate(dec=ntile(plcd_dat_Mean,2))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(plcd_dat_Mean,na.rm = TRUE),LessThan=max(plcd_dat_Mean,na.rm = TRUE))->data_plcd_dat_Mean

#64. for Variable retention = data_retention
summary(telecom$retention)
telecom%>%mutate(dec=ntile(retention,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(retention,na.rm = TRUE),LessThan=max(retention,na.rm = TRUE))->data_retention

#One of the area to understand is that the effect of network and service quality influencing churn rate 
#To get the top idea about network and service quality we can drive the following variable
telecom$compl_vce_percentage<-(telecom$comp_vce_Mean/telecom$plcd_vce_Mean)
telecom$compl_dat_percentage<-(telecom$comp_dat_Mean/telecom$plcd_dat_Mean)

# 65.For variable compl_vce_percentage
summary(telecom$compl_vce_percentage)
telecom%>%mutate(dec=ntile(compl_vce_percentage,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(compl_vce_percentage,na.rm = TRUE),LessThan=max(compl_vce_percentage,na.rm = TRUE))->data_compl_vce_percentage

# 66.For variable compl_dat_percentage
summary(telecom$compl_dat_percentage)
telecom%>%mutate(dec=ntile(compl_dat_percentage,10))%>%group_by(dec)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,2),GreaterThan=min(compl_dat_percentage,na.rm = TRUE),LessThan=max(compl_dat_percentage,na.rm = TRUE))->data_compl_dat_percentage

#Removing compl_dat_percentage from the dataset
names(telecom)
telecom<-telecom[,-69]
telecom_Declined_continuous<-rbind(data_mou_Mean,data_totmrc_Mean,data_rev_Range,data_mou_Range,data_change_mou,data_drop_blk_mean,data_drop_vce_Mean,
                                   data_owylis_vce_Range,data_mou_opkv_Range,data_months,data_totcalls,data_eqpdays,data_iwylis_vce_Mean,data_adjqty,
                                   data_rev_Mean,data_comp_vce_Mean,data_plcd_vce_Mean,data_avg3mou,data_avgmou,data_avg3qty,data_avgqty,
                                   data_avg6mou,data_avg6qty,data_hnd_price,data_adjmou,data_adj_rev,data_avgrev,data_compl_vce_percentage)
str(telecom_Declined_continuous)
write.csv(telecom_Declined_continuous,'Declined Output Continuous Variables.csv',row.names = F)
#We can remove the variables based on declined binning outputs
#Lets Remove those variable which either have many zero  or no Variablity these variables do not add any significance to the model 
#custcare_Mean,callwait_Mean,call,wait_Range,ccrndmou_Range,ovrmou_Mean,opk_dat_Mean,roam_Mean,recv_sms_Mean,mou_pead_Mean,
#da_Mean,da_Range,datovr_Mean,datovr_Range,drop_dat_Mean,comp_dat_Mean,plcd_dat_Mean
names(telecom)
telecom<-telecom[,-c(13,14,16,17,21,45,48:50,54:58,65,66)]
dim(telecom)
names(telecom)

#Before checking event rates for the Categorical variables, lets convert the variable which are supposed to be categorical 
#But stored as numeric into Categorical Variable
telecom$models <- as.factor(telecom$models)
telecom$hnd_price <- as.factor(telecom$hnd_price)
telecom$actvsubs <- as.factor(telecom$actvsubs)
telecom$uniqsubs <- as.factor(telecom$uniqsubs)
telecom$forgntvl <- as.factor(telecom$forgntvl)
telecom$truck <- as.factor(telecom$truck)
telecom$mtrcycle <- as.factor(telecom$mtrcycle)

str(telecom)
names(telecom)
##########################################################
#CATEGORICAL VARIABLES PROFILLING AND DECILING 
##Event Rate For Each Level in a Categorical Variable

# 1. For the Variable crclscod=data_crclscod / Credit class code
#A Represtents best rating ,Z represents worst rating 
#Due to low Percent of churn which are less than 5%
#Which can show insignificant Behaviour 
library(dplyr)
summary(telecom$crclscod)
telecom%>%group_by(crclscod)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_crclscod
class(telecom$crclscod)
View(data_crclscod)

#2. For Variable asl_flag=data_asl_flag / Account Spending Limit
#N = NO
#Y = Yes
#There is somes differrence in the event rate across different levels 
summary(telecom$asl_flag)
telecom%>%group_by(asl_flag)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_asl_flag
class(telecom$asl_flag) #Trent in Churn Perc

#3. For Variable prizm_social_one = data_prizm_social_one / Social Group letter only 
#Based on degree of population density of area
#C = City
#R = Rural
#S = Suburban
#T = Town
#U = Urban
library(tidyverse) # for using  fct_explicit_na function 
summary(telecom$prizm_social_one) 
telecom%>%group_by(fct_explicit_na(prizm_social_one))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_prizm_social_one
class(telecom$prizm_social_one)

#4. For Variable area = data_area / Geographic Area
#There are many levels we will try  to find the levels with similar event rate 
#Event Rate Matches with few other levels ,we can use this info to treat missing values 
summary(telecom$area)
telecom%>%group_by(fct_explicit_na(area))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_area
class(telecom$area)

#5. For Variable refurb_new = data_refurb_new # Handset : Refurbished or new 
#N = New 
#R = Refurbished
#There is some difference in the event rate across different levels 
summary(telecom$refurb_new)
telecom%>%group_by(fct_explicit_na(refurb_new))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_refurb_new
class(telecom$refurb_new)

#6. For Variable hnd_webcap = data_hnd_webcap / Handset Web Capability
#WC = Web Capable
#WC = Web Capable Mini-Browser
#NA = Not Applicable
#Unkw = Unable to collect these data
#There is some difference in the event rate across different level
summary(telecom$hnd_webcap)
telecom%>%group_by(fct_explicit_na(hnd_webcap))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_hnd_webcap
class(telecom$hnd_webcap)

#7. For Variable Marital = data_marital / Marital Status 
#Indicates if Anyone in the household is Married 
#U = Unknown 
#M = Married
#S = Single
#B = Inferred Single
#A = Inferred Married
summary(telecom$marital)
telecom%>%group_by(fct_explicit_na(marital))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_marital
class(telecom$marital)

#8. For Variable ethnic = data_ethnic / Ethnicity roll-up code
#B = Asian (Non-Oriental)
#D = Southern European
#F = French
#G = German
#H = Hispanic
#I = Italian
#J = Jewish
#M = Miscellaneous
#N = Northern European
#O = Asian
#P = Polynesian
#R = Arab
#S = Scottish / Irish
#U = Unknown
#Z = African-American
summary(telecom$ethnic)
telecom%>%group_by(fct_explicit_na(ethnic))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_ethnic
class(telecom$ethnic)

#9. For Variable car_buy = data_car_buy / New or Used car buyer
#Indicates a History of new car buying in the household.
summary(telecom$car_buy)
telecom%>%group_by(fct_explicit_na(car_buy))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_car_buy
class(telecom$car_buy)


#10. For Variable models = data_models / Number of models issued 
summary(telecom$models)
telecom%>%group_by(fct_explicit_na(models))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_models
class(telecom$models)

#11. For Variable hnd_price = data_hnd_price / current handset price 
summary(telecom$hnd_price)
telecom%>%group_by(fct_explicit_na(hnd_price))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_hnd_price
class(telecom$hnd_price)

#12.For Variable actvsubs = data_actvsubs / Number of active subscribers in household
summary(telecom$actvsubs)
telecom%>%group_by(fct_explicit_na(actvsubs))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_actvsubs
class(telecom$actvsubs)

#13.For Variable uniqsubs = data_uniqsubs / Number of Unique Subscribers in the household
summary(telecom$uniqsubs)
telecom%>%group_by(fct_explicit_na(uniqsubs))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_uniqsubs
class(telecom$uniqsubs)

#14. For variable forgntvl
summary(telecom$forgntvl)
telecom%>%group_by(fct_explicit_na(forgntvl))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_forgntvl
class(telecom$forgntvl)

#15. For Variable mtrcycle = data_mtrcycle / Motorcycle indicator
#Indicates motocycle owner in household 
summary(telecom$mtrcycle)
telecom%>%group_by(fct_explicit_na(mtrcycle))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_mtrcycle
class(telecom$mtrcycle)

#16. For Variable truck = data_truck / Truck indicator 
#Indicator a truck owner in a household 
summary(telecom$truck)
telecom%>%group_by(fct_explicit_na(truck))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_truck
class(telecom$truck)

#For Variable csa = q10 / Communications local service area
#Refer to Specific Location of the customer , usually indication city
summary(telecom$csa)
telecom%>%group_by(fct_explicit_na(csa))%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_csa
class(telecom$csa) #Many Levels Showing less than 50% churn rate
#and having high levels may show insignificant nature during model running

#now those categorical variable which do not have factor nature 
#So also Converting those into factor datatype
#For Variable retention = data_retention 
#Retention calls include any calls from the customer regarding loyalty or retention , e.g. contract renewal , relating competitor's offer ,etc.
#Missing Values for this variable can be assumed to mean there have been no retention calls made by the customer
summary(telecom$retention)
telecom%>%group_by(retention)%>%summarise(n=sum(churn),N=n(),churn_perc=round(n/N,4))->data_retention
class(telecom$retention)
names(telecom)


#Omit where there is not enough change in the event rate at different levels
#Variables having churn rate less than 5% No need 
telecom<-telecom[,-c(39:41,43)] #car_buy,truck,mtrcycle,forgntvl
names(telecom)
dim(telecom)

#DATA PREPARATION
# (1) OUTLIER TREATMENT 

#First lets treat continuous variable 
#To detect the outlier 
#Graphical method we are using is boxlot .
colnames(telecom)

names(telecom)
list<-names(telecom)

#Removing categorical Variable 
list<-list[-c(25:42,50,51)]
list

#boxplot(telecom$churn,telecom$prizm_social_one)
#Plotting the outlier 
dev.off()
par(mfrow=c(3,11))
for (i in 1:length(list))
  {
  boxplot(telecom[,list[i]],main=list[i])
  } #Warnings 

#Outlier Treatment 
for (i in 1:length(list)) 
  {
  a<-boxplot(telecom[,list[i]],main=list[i])
  out<-a$out
  index<-which(telecom[,list[i]]%in%a$out)
  telecom[index,list[i]]<-mean(telecom[,list[i]],na.rm = TRUE)
  rm(a)
  rm(out)
  }

#Checking after treatment 
for (i in 1:length(list))
{
  boxplot(telecom[,list[i]],main=list[i])
}

for (i in 1:length(list))
{
  plot(telecom[,list[i]],main=list[i])
}

#lets treat  Categorical Variable 
colSums(is.na(telecom))

#For Variable crclsod
#There are no missing values 
sum(is.na(telecom$crclscod))

#For variable asl_flag
#There are no missing values 
sum(is.na(telecom$asl_flag))

#For variable prize_social_one
barplot(telecom$mou_Mean)
sum(is.na(telecom$prizm_social_one)) #<- 4751 missing values 
missing_p<-which(is.na(telecom$prizm_social_one))
telecom$prizm_social_one[missing_p]<-'T'
#Replacing with level T as there are similarity in the event rate
#As Observed above ,levels C,S and U have similar rate event we can combine them together 
index_p <-which(telecom$prizm_social_one=='S' | telecom$prizm_social_one=='U')
telecom$prizm_social_one[index_p]<-'C'
unique(telecom$prizm_social_one)
summary(telecom$prizm_social_one)

#For Variable area 
sum(is.na(telecom$area)) #<- 18 missing values 
summary(telecom$area)#<- Event rate for the missing value is same as the CENTRAL/SOUTH TEXAS AREA
missing_a<-which(is.na(telecom$area))
telecom$area[missing_a]<-'CENTRAL/SOUTH TEXAS AREA'

#For variable refurb_new
sum(is.na(telecom$refurb_new)) #Only one missing observation 
missing_r<-which(is.na(telecom$refurb_new))
telecom[missing_r,] #Dataset has either 0 or missing values for many variables hence we need to omit it from dataset
telecom[-missing_r,]->telecom
dim(telecom)

#For variable hnd_webcap
sum(is.na(telecom$hnd_webcap)) #<-6062 missing values 
summary(telecom$hnd_webcap) #We can replace missing values with new  'WC' level based on event rate Similarity
missing_hnd<-which(is.na(telecom$hnd_webcap))
telecom$hnd_webcap[missing_hnd]<- 'WC'

#For Variable Marital
sum(is.na(telecom$marital)) #<-1152 Missing Values 
summary(telecom$marital) #We can add missing values to level 'S' based on event rate similarity
missing_m<-which(is.na(telecom$marital))
telecom$marital[missing_m]<-'S'

#For Variable ethnic
sum(is.na(telecom$ethnic)) #<-1152 missing values 
summary(telecom$ethnic) #We can add missing values to level 'M' based on event rate similarity
missing_e<-which(is.na(telecom$ethnic))
telecom$ethnic[missing_e]<-'M'

#For Variable hnd_price
sum(is.na(telecom$hnd_price)) #<-635 missing values 
missing_hnd_price<-which(is.na(telecom$hnd_price))
telecom$hnd_price[missing_hnd_price]<-'299.9899902'
summary(telecom$hnd_price)

#For Variable csa
sum(is.na(telecom$csa)) #<-18 missing values 
missing_c<-which(is.na(telecom$csa))
summary(telecom$csa) #we can replace missing values with level 'DALFTW817'based on event rate similarity
telecom$csa[missing_c]<-'DALFTW817'

#Now Lets Deal with Continuous Variables 
colSums(is.na(telecom))

#For Variable mou_Mean
summary(telecom$mou_Mean)# 181 missing values Omitting them
missing_mou_Mean<-which(is.na(telecom$mou_Mean))
telecom<-telecom[-missing_mou_Mean,]
summary(telecom$mou_Mean)
dim(telecom)

#For Variable change_mou
summary(telecom$change_mou) #233 missing values Omitting them and have a clean dataset
missing_change_mou<-which(is.na(telecom$change_mou))
telecom<-telecom[-missing_change_mou,]
summary(telecom$change_mou)

library(dplyr)
#for variable avg6mou
summary(telecom$avg6mou) #<-2029 missing values
#Lets impute the missing values with the average of 10th Decile Obervation
telecom%>%mutate(dec=ntile(avg6mou,10))->telecom
telecom%>%filter(dec==10)%>%summarise(avg=sum(avg6mou/n()))
missing_avg6mou<-which(is.na(telecom$avg6mou))
telecom$avg6mou[missing_avg6mou]<-1688.485

#For Variable avg6qty
summary(telecom$avg6qty) #<-2029 missing Values 
#Lets impute the missing values with average of 10th Decile Observation
telecom%>%mutate(dec=ntile(avg6qty,10))->telecom
telecom%>%filter(dec==10)%>%summarise(avg=sum(avg6qty/n()))
missing_avg6qty<-which(is.na(telecom$avg6qty))
telecom$avg6qty[missing_avg6mou]<-610.5087

#For variable age1
summary(telecom$age1)#<-1144 missing values lets impute them with median as data is skewed 
missing_age1<-which(is.na(telecom$age1))
hist(telecom$age1) #Data is skewed imputing with median 
telecom$age1[missing_age1]<-36.00

telecom%>%filter(age1==0)%>%summarise(n=n()) #17888 values are zero and we know that can't be Zero 
#but we can convert age1 variable to categorical and named missing or 0 is equals None

telecom$age1_dummy<-ifelse(telecom$age1=='0','None',
                           ifelse(telecom$age1<=30,'Young',
                                  ifelse(telecom$age1>30 & telecom$age1<=55,'Mid-Age','Senior')))

unique(telecom$age1_dummy)
str(telecom$age1_dummy)
#Converting to Factor 
telecom$age1_dummy <- as.factor(telecom$age1_dummy)

#For Variable age2
summary(telecom$age2)
telecom%>%filter(age2==0)%>%summarise(n=n()) #Half of the 'age2' values seems to be zero may not be useful
#Lets omit age2 from the data and age1 also as i created dummy variable for age1 data
names(telecom)
telecom<-telecom[,-c(33,34)]

#We have Missing Values in  compl_vce_percentage due to the fact tht corrospondes to values of 'plcd_vce_Mean' and 'plcd_dat_mean'are Zeros
#While creating this variable due to zeros in numerator and denomiator ,the values become NaN
#We need to Omit this observation
summary(telecom$compl_vce_percentage)
missing_compl<-which(is.na(telecom$compl_vce_percentage))
telecom<-telecom[-missing_compl,]

#Let's just create another column optimum for checking the ratio of overage revenue / total revenue
#i.e overrev_mean / totrev
telecom$optimum <- telecom$ovrrev_Mean/telecom$totrev
summary(telecom$optimum)

#dec variable is longer useful
names(telecom)
telecom<-telecom[-47]

#Finally there is no NA's or missing values found in dataset 
colSums(is.na(telecom))

#checking overall data cleaning impacted on churn or not 
sum(telecom$churn)/nrow(telecom) #Churn rate is 23.35 we havent lost lot of information

#Making one copy of dataset in case of data loss 
final_telecom<-telecom
dim(final_telecom)
names(final_telecom)

#################################################################################################
##BUILDING LOGISTIC REGRESSION
#Splitting the data set into training an testing dataset
set.seed(200)
partition<-sample(nrow(telecom),0.70*nrow(telecom),replace = FALSE)
training<-telecom[partition,]
testing<-telecom[-partition,]

#Calculating Churn Rate for both training and testing 
table(training$churn)/nrow(training)
table(testing$churn)/nrow(testing)
#Almost Near values of both Training and Testing

colnames(telecom)
#Building a Model 
mod1<-glm(churn~.,data = training,family = 'binomial') #Error: cannot allocate vector of size 295.8 Mb
summary(mod1)
#Since step takes more time for individual step,doing it manually
#Lets create dummy variable for the representive of significant levels for Categorical Variable
summary(training$age1_dummy)
training$age1_midage <- ifelse(training$age1_dummy=='mid age',1,0)
testing$age1_midage <- ifelse(testing$age1_dummy=='mid age',1,0)

training$age1_old <- ifelse(training$age1_dummy=='old',1,0)
testing$age1_old <- ifelse(testing$age1_dummy=='old',1,0)


training$age1_young <- ifelse(training$age1_dummy=='young',1,0)
testing$age1_young <- ifelse(testing$age1_dummy=='young',1,0)

#for variable prizm_social_one_a
summary(training$prizm_social_one)

training$prizm_social_one_R <- ifelse(training$prizm_social_one=='R',1,0)
testing$prizm_social_one_R <- ifelse(testing$prizm_social_one=='R',1,0)

training$prizm_social_one_T <- ifelse(training$prizm_social_one=='T',1,0)
testing$prizm_social_one_T <- ifelse(testing$prizm_social_one=='T',1,0)

#For Variable uniqsubs 
summary(telecom$uniqsubs)
training$uniqsubs_2 <- ifelse(training$uniqsubs=='2',1,0)
testing$uniqsubs_2 <- ifelse(testing$uniqsubs=='2',1,0)

training$uniqsubs_3 <- ifelse(training$uniqsubs=='3',1,0)
testing$uniqsubs_3 <- ifelse(testing$uniqsubs=='3',1,0)

training$uniqsubs_4 <- ifelse(training$uniqsubs=='4',1,0)
testing$uniqsubs_4 <- ifelse(testing$uniqsubs=='4',1,0)

training$uniqsubs_5 <- ifelse(training$uniqsubs=='5',1,0)
testing$uniqsubs_5 <- ifelse(testing$uniqsubs=='5',1,0)

training$uniqsubs_6 <- ifelse(training$uniqsubs=='6',1,0)
testing$uniqsubs_6 <- ifelse(testing$uniqsubs=='6',1,0)

training$uniqsubs_7 <- ifelse(training$uniqsubs=='7',1,0)
testing$uniqsubs_7 <- ifelse(testing$uniqsubs=='7',1,0)

training$uniqsubs_9 <- ifelse(training$uniqsubs=='9',1,0)
testing$uniqsubs_9 <- ifelse(testing$uniqsubs=='9',1,0)

#For Variable hnd_price
summary(training$hnd_price)

training$hnd_price_129.98 <- ifelse(training$hnd_price =='129.9899902',1,0)
testing$hnd_price_129.98 <- ifelse(testing$hnd_price == '129.9899902',1,0)

training$hnd_price_199.98 <- ifelse(training$hnd_price =='199.9899902',1,0)
testing$hnd_price_199.98 <- ifelse(testing$hnd_price == '199.9899902',1,0)

training$hnd_price_249.98 <- ifelse(training$hnd_price =='249.9899902',1,0)
testing$hnd_price_249.98 <- ifelse(testing$hnd_price == '249.9899902',1,0)

training$hnd_price_299.98 <- ifelse(training$hnd_price =='299.9899902',1,0)
testing$hnd_price_299.98 <- ifelse(testing$hnd_price == '299.9899902',1,0)

#For Variable ethnic
summary(training$ethnic)

training$ethnic_C <- ifelse(training$ethnic == "C",1,0)
testing$ethnic_C <- ifelse(testing$ethnic == "C",1,0)

training$ethnic_N <- ifelse(training$ethnic == "N",1,0)
testing$ethnic_N <- ifelse(testing$ethnic == "N",1,0)

training$ethnic_O <- ifelse(training$ethnic == "O",1,0)
testing$ethnic_O <- ifelse(testing$ethnic == "O",1,0)

training$ethnic_S <- ifelse(training$ethnic == "S",1,0)
testing$ethnic_S <- ifelse(testing$ethnic == "S",1,0)

training$ethnic_U <- ifelse(training$ethnic == "U",1,0)
testing$ethnic_U <- ifelse(testing$ethnic == "U",1,0)

training$ethnic_Z <- ifelse(training$ethnic == "Z",1,0)
testing$ethnic_Z <- ifelse(testing$ethnic == "Z",1,0)

#For Variable Area 
summary(training$area)

training$area_calnrth <- ifelse(training$area == 'CALIFORNIA NORTH AREA',1,0)
testing$area_calnrth <- ifelse(testing$area == 'CALIFORNIA NORTH AREA',1,0)

training$area_texas <- ifelse(training$area == 'CENTRAL/SOUTH TEXAS AREA',1,0)
testing$area_texas <- ifelse(testing$area == 'CENTRAL/SOUTH TEXAS AREA',1,0)

training$area_nrthflorida <- ifelse(training$area == 'NORTH FLORIDA AREA',1,0)
testing$area_nrthflorida <- ifelse(testing$area == 'NORTH FLORIDA AREA',1,0)

training$area_nrthwst <- ifelse(training$area == 'NORTHWEST/ROCK MOUNTAIN AREA',1,0)
testing$area_nrthwst <- ifelse(testing$area == 'NORTHWEST/ROCK MOUNTAIN AREA',1,0)

training$area_southflorida <- ifelse(training$area == 'SOUTH FLORIDA AREA',1,0)
testing$area_southflorida <- ifelse(testing$area == 'SOUTH FLORIDA AREA',1,0)

training$area_southwest <- ifelse(training$area == 'SOUTHWEST AREA',1,0)
testing$area_southwest <- ifelse(testing$area == 'SOUTHWEST AREA',1,0)

training$area_tenese <- ifelse(training$area == 'TENNESSEE AREA',1,0)
testing$area_tenese <- ifelse(testing$area == 'TENNESSEE AREA',1,0)

#for variable asl_flag
summary(training$asl_flag)
training$asl_flag_y <- ifelse(training$asl_flag == 'Y',1,0)
testing$asl_flag_y <- ifelse(testing$asl_flag == 'Y',1,0)

#For Variable refurb_new 
summary(training$refurb_new)
training$refurb_new_r <- ifelse(training$refurb_new == 'R',1,0)
testing$refurb_new_r <- ifelse(testing$refurb_new == 'R',1,0)

#For Variable Marital
summary(training$marital)
training$marital_s<- ifelse(training$marital=='S',1,0)
testing$marital_s<-ifelse(testing$marital=='S',1,0)

training$marital_a<- ifelse(training$marital=='A',1,0)
testing$marital_a<-ifelse(testing$marital=='A',1,0)

training$marital_b<- ifelse(training$marital=='B',1,0)
testing$marital_b<-ifelse(testing$marital=='B',1,0)

training$marital_m<- ifelse(training$marital=='M',1,0)
testing$marital_m<-ifelse(testing$marital=='M',1,0)

training$marital_u<- ifelse(training$marital=='U',1,0)
testing$marital_u<-ifelse(testing$marital=='U',1,0)

#For Variable Models
summary(training$models)
summary(testing$models)
training$model_2<-ifelse(training$models == '2',1,0)
testing$model_2<-ifelse(testing$models == '2',1,0)

training$model_3<-ifelse(training$models == '3',1,0)
testing$model_3<-ifelse(testing$models == '3',1,0)

training$model_4<-ifelse(training$models == '4',1,0)
testing$model_4<-ifelse(testing$models == '4',1,0)

training$model_5<-ifelse(training$models == '5',1,0)
testing$model_5<-ifelse(testing$models == '5',1,0)

training$model_8<-ifelse(training$models == '8',1,0)
testing$model_8<-ifelse(testing$models == '8',1,0)

#Create for Event rate (churn rate) in trainting and testing datasets,to make sure the samples are not baised
sum(telecom$churn)/nrow(telecom)
sum(training$churn)/nrow(training)

colnames(training)
summary(training)
step(model1,direction = 'both')
model2 <- glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + drop_blk_Mean + drop_vce_Range + owylis_vce_Range
                    + mou_opkv_Range + months + totcalls + eqpdays + iwylis_vce_Mean + adjqty + ovrrev_Mean + rev_Mean + comp_vce_Mean 
                    + plcd_vce_Mean + avg3mou + avgmou + avg3qty + avgqty + avg6mou + avg6qty + crclscod + asl_flag_y +area_tenese + area_southwest
                    + area_southflorida +area_nrthwst +area_nrthflorida +area_texas +area_calnrth + refurb_new_r + hnd_price_129.98 + hnd_price_199.98
                    + hnd_price_249.98 + hnd_price_299.98 + marital_a + marital_b + marital_m +marital_s +marital_u + model_2 +model_3 +model_4
                    + model_5+ model_8 + ethnic_N + ethnic_S + ethnic_U + ethnic_Z +ethnic_C +ethnic_O +actvsubs + uniqsubs_2 +uniqsubs_3 +uniqsubs_4
                    + uniqsubs_5 + uniqsubs_6 +uniqsubs_7 + uniqsubs_9 + drop_vce_Mean +adjmou +totrev + adjrev+avgrev + retention + compl_vce_percentage
                    + prizm_social_one_R+prizm_social_one_T+optimum,data = training,family = binomial(link = 'logit'))
summary(model2)
#Aic is Decreased 44827
#memory.limit(size = 40000)
#Lets Remove the insignificant variables from the above model i.e p>0.05 are drop_blk_Mean,drop_vce_Range,mou_opkv_Range,totcalls,ovrrev_Mean
#comp_vce_Mean,plcd_vce_Mean,avg3qty,avgqty,area_nrthwst,area_nrthflorida,marital_a,marital_b,marital_u,model_8,actvsubs,uniqsubs_9
#totrev,adjrev,avgrev,uniqsubs_6,area_calnrth,area_southwest ,adjqty,avg6qty

#For variable crclscod
summary(training$crclscod)
training$crclscodE4<- ifelse(training$crclscod == 'E4',1,0)
testing$crclscodE4<-ifelse(testing$crclscod == 'E4',1,0)

training$crclscodEA<- ifelse(training$crclscod == 'EA',1,0)
testing$crclscodEA<-ifelse(testing$crclscod == 'EA',1,0)

model3 <- glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + owylis_vce_Range
              + months + eqpdays + iwylis_vce_Mean + rev_Mean  
              + avg3mou + avgmou + avg6mou + crclscodE4+crclscodEA + asl_flag_y +area_tenese
              + area_southflorida +area_texas + refurb_new_r + hnd_price_129.98 + hnd_price_199.98
              + hnd_price_249.98 + hnd_price_299.98 + marital_m +marital_s + model_2 +model_3 +model_4
              + model_5 + ethnic_N + ethnic_S + ethnic_U + ethnic_Z +ethnic_C +ethnic_O + uniqsubs_2 +uniqsubs_3 +uniqsubs_4
              + uniqsubs_5 + drop_vce_Mean +adjmou  + retention + compl_vce_percentage
              + prizm_social_one_R+prizm_social_one_T+optimum,data = training,family = binomial(link = 'logit'))
summary(model3)
#AIC is Decreased <-44811

#Lets Re-Run the model removing some insignificant variable which is model_5
model4 <- glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + owylis_vce_Range
              + months + eqpdays + iwylis_vce_Mean + rev_Mean  
              + avg3mou + avgmou + avg6mou + crclscodE4+crclscodEA + asl_flag_y +area_tenese
              + area_southflorida +area_texas + refurb_new_r + hnd_price_129.98 + hnd_price_199.98
              + hnd_price_249.98 + hnd_price_299.98 + marital_m +marital_s + model_2 +model_3 +model_4
              + ethnic_N + ethnic_S + ethnic_U + ethnic_Z +ethnic_C +ethnic_O + uniqsubs_2 +uniqsubs_3 +uniqsubs_4
              + uniqsubs_5 + drop_vce_Mean +adjmou  + retention + compl_vce_percentage
              + prizm_social_one_R+prizm_social_one_T+optimum,data = training,family = binomial(link = 'logit'))
summary(model4)
#AIC is  44812

#Lets remove some insignificants variables 
model5 <- glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou + owylis_vce_Range
              + months + eqpdays + iwylis_vce_Mean + rev_Mean  
              + avg3mou + avgmou + avg6mou + crclscodE4+crclscodEA + asl_flag_y +area_tenese
              + area_southflorida +area_texas + refurb_new_r + hnd_price_129.98 + hnd_price_199.98
              + hnd_price_249.98 + hnd_price_299.98 + marital_m +marital_s + model_2 +model_3 +model_4
              + ethnic_N + ethnic_S + ethnic_U + ethnic_Z +ethnic_C +ethnic_O + uniqsubs_2 +uniqsubs_3 +uniqsubs_4
              + uniqsubs_5 + drop_vce_Mean +adjmou  + retention + compl_vce_percentage
              + prizm_social_one_R+prizm_social_one_T+optimum,data = training,family = binomial(link = 'logit'))
summary(model5)

#No change in AIC i.e 44812 lets finalise this model 
#VIF::vif(model5) #multicollinearity check for >5 values 

plot(model5) #Plotting the model 
######################################################################################3
#MODEL VALIDATION
#Model 5 is the best model we have got till now with aic 44812
sum(testing$churn) #<- 4270
sum(training$churn) #<-9992

sum(testing$churn)/nrow(testing) #23%
predicted_values<-predict(model5,type = 'response',newdata = testing)
#type = 'response' link gives us probabilities it converts log of odd ratios values of dependent variables to probabilities of the event occurance
head(predicted_values)

confint(model5)
# we need to choose a cut off value for predicted probabilities to define a churn 
#There is no strict rule to decide on a cutoff value ,completely depends upon the study we are doing 

#If correctly identifying positives is important for us then, we should choose a model with higher Sensitivity 
#However ,if correctly identifying negatives is more important ,then we should choose Specificity 
#RORC curve
library(ROCR)
length(predicted_values)
pred<-predict(model5,type='response',newdata = testing)
head(pred)
#Setting the cutoff
#Assuming cutoff probabilities as per the churn rate in the dataset 
table(telecom$churn)/nrow(telecom)
#Proportion is 0.2335391
pred1<-ifelse(pred>=0.2335391,1,0)
#getting zero will be churn 
#Getting non-zero i.e 1 will not churn

library(lpSolve)
library(irr)
library(lattice)
library(caret)
predauc<-prediction(pred1,testing$churn)
predicted_values_cutoff<-ifelse(predicted_values>0.2335391,1,0)
head(predicted_values_cutoff)
kappa2(data.frame(testing$churn,predicted_values_cutoff)) #Kappa is 0.134 Level of Agrement None
testing$probability<-predict(model5,type = 'response',newdata = testing)
testing$result<-ifelse(predicted_values>0.2335391,1,0)
head(testing)

confusionMatrix(as.factor(testing$result),as.factor(testing$churn),positive = '1')
#57.47 % Accuracy

#AUC
auc<-performance(predauc,measure = 'auc')
auc
auc<-auc@y.values[[1]]
auc
#0.5918171 which is greater than 0.50 

#According to solution of this project auc 0.5918171 is very good 
#In this model validation ,model4 is turning out to be a good model . hence ,we can finalize it 
#Let's built the gains chart  
gain_chart<-gains::gains(testing$churn,predict(model5,type = 'response',newdata = testing),groups = 10)

#The gains chart showsthat by targetting top 30% customers by probabilities ,on an average we will target 43.1 % of customers who will churn 
class(gain_chart)
#plot.gains(gain_chart)
a=gain_chart[1]
b=gain_chart[6]
library(ggplot2)
gains=as.data.frame(c(a,b))
ggplot(data = gains)+geom_line(aes(x=gains$depth,y=gains$cume.pct.of.total))

###################################################################################################################################3
#Question and answers 
#1] What are the top five factors driving likelyhood of churn at mobicom ?
head(sort(abs(model5$coefficients),decreasing = TRUE),10)
model5$fitted.values
#Ans->#1.optimum(3.6377705) <- This variable is the ratio of overage revenue / total revenue #i.e overrev_mean / totrev
      #2.ethnic_C (1.4554729)
      #3.hnd_price_249.98(1.0732195)
      #4.retention(0.7593517)<-Retention calls include any calls from the customer regarding loyalty or retention , e.g. contract renewal , relating competitor's offer ,etc.
      #5.crclscodE4(0.6852267)

#2] Validation and Survey 
#2A] Whether "Cost and Billing " and "Network and Serive Quality" are important Factors Influencing the Churn Behaviour ?
#ANs-> #Analysis Shows the cost and billing is causing churn as mou overage is Constructing to churn 
       #Network and service Quality is also found to be impacting churn as call drop is an issue as shown in analysis
       #drop voice call and customer care call variable shows impact 
summary(model5)
#2B] Are the data usage connectivity issue turning out to be costly ? In other words ,is it leading to churn ?
#ANs-> #Data Usage connectivity is not causing Significance impact on churn .
#Variables assiocated with data usage and connectivity are 
#comp_dat_mean,plcd_dat_mean,opk_dat_Mean,blck_dat_Mean,datovr_Mean,datovr_range,drop_dat_Mean.



#3] Would you recommend rate plan migration as a proactive retention strategy ?
#ANS-> Yes, I would recommend rate plan migration as a proactive renteation strategy as there is direct relationship between 
      #Mean overrage revenue (overev_Mean) and churn rate . Also, as mentioned above subscribers with the non-optimal rate 
      #tend to ovrrage and have significantly higher odds of churn as compared to subscriber with optimal rate.

#4] What would be your Recommendation on how to use this churn model for prioritization of customers for proactive retention campaigns in the future ?
#ANS-> #For Used Based Promotions
View(data_adjmou)
#WE can Define Billing Adjusted total minutes of use over the life of the customer (adjmou) lessthan 5053 as low usage 
#5053 is the average of 5th percentile lower and upper bounds

Customer_low_usage<- testing[testing$probability>0.2335391 & testing$adjmou>5053,]
nrow(testing)
nrow(Customer_low_usage)

head(Customer_low_usage)
Customer_low_usage<-as.data.frame(Customer_low_usage)
write.csv(Customer_low_usage,'Target Customer for Usage Campaigns.csv')

#We can also Target Customers with less than 5053 adjmou and probability churn more than 0.2335391 (Cutoff Value) for proactive usage increase plan 
#Rate plan Migration : It is a strategy to move customers from non-optimal to optimal plans as it has been observed that subscribers on non-optimal rate plans 
#Have Significantly higher odds of churn relative to subscriber on optimal rate 
quantile(testing$optimum,c(p=1:10)/10)
#we can choose the customer for whom the 'Optimum and probability' is more than 0.2 (20%) ,which means their overage revenue contibutes more than 20% to the total Revenue 
customer_plan_migration<-testing[testing$probability>0.2335391 & testing$adjmou<5053 & testing$optimum>0.022644329,]
nrow(customer_plan_migration)
customer_plan_migration<-as.data.frame(customer_plan_migration)
write.csv(customer_plan_migration,'Target Customer for Plan Migration.csv')

#Family Bunding Offer 
#We can offer bundle offer to families to reduce churn 
#For this the target Cutomer can those whose family a minimum 4 unique subscriber in the household and high churn rate probability 

str(testing$actvsubs)
testing$actvsubs_int<-as.numeric(testing$actvsubs)
customer_FamilyBundel<- testing[testing$probability>0.2335391 & testing$actvsubs_int>=4,]
nrow(customer_FamilyBundel)
customer_FamilyBundel<-as.data.frame(customer_FamilyBundel)
write.csv(customer_FamilyBundel,'Target Customer for Family Bundel.csv')

#Annual Retention Offers: one observation from the analysis is that ,on an average 43% who are with Mobicom between 10-12 months tends to leave .
#Mobicom can target customers who are having high churn Probability and are with  the carrier for 10-12 months, to an annual offer to reduce the churn 
customer_annualRetention<-testing[testing$probability>0.2335391 & testing$months>=10 & testing$months<=12,]
nrow(customer_annualRetention)
summary(customer_annualRetention$months)
customer_annualRetention<-as.data.frame(customer_annualRetention)
write.csv(customer_annualRetention,'Target Customer for Annual Retention Offer.csv')

#5] What will be the target segment for proactive customer Campaigns ? Falling ARPU forecast is also a concern and therefore Mobicom would you like to save their high revenue customers besides managing churn 
#Giving a budget constraint of a contact list of 20% of the subscriber pool ,which Subscriber sholud prioritised if "Revenue Saves" is also a priority besides controlling Churn.
#In other words , Controlling Churn is the Primary Objective and Revenue saves it from secondary object .

#ANS-> Assuming that the customer in the test is our Subscriber pool
gain_chart
#By looking at the gain chart , if we target top 20% of the subscriber pool (test file) by churn probability ,we will able to reach 3664 Customers

#Lets find out who are the customer
quantile(testing$probability,c(p=(1:10)/10))

#Top 20 Customer have probability between 0.2984393 and 0.7993235
#Lets Extract their data into new vector 
customer_proactive_retention<-testing[testing$probability>0.2984393,]
nrow(customer_proactive_retention)
customer_proactive_retention<-as.data.frame(customer_proactive_retention)
write.csv(customer_proactive_retention,'Targeted Customer for Proactive Retention.csv')

#Lets see whom to target Based on Revenue as Priority 
#The idea is to focus more on customers who give high revenue to Mobicom and retain them on priority 
quantile(testing$probability,c(p=(1:100)/100))
quantile(customer_proactive_retention$probability,c(p=(1:10)/10))
customer_proactive_retention$Churn_Level<-ifelse(customer_proactive_retention$probability<=0.3200020,'Low(0.30-0.3200020)',ifelse(customer_proactive_retention$probability>0.3200020 & customer_proactive_retention$probability<=0.3711780,'Medium(0.3200020-0.3711780)','High(0.3711780-0.8437166)'))
customer_proactive_retention$Churn_Level<-as.factor(customer_proactive_retention$Churn_Level)
summary(customer_proactive_retention$Churn_Level)

quantile(customer_proactive_retention$totrev,c(p=(1:10)/10))
customer_proactive_retention$Revenue_Level<-ifelse(customer_proactive_retention$totrev<=719.226,'Low(420-719.226)',ifelse(customer_proactive_retention$totrev>719.226 & customer_proactive_retention$totrev<=1337.972,'Medium(719.226-1337.972)','High(1337.972-27321.500)'))
customer_proactive_retention$Revenue_Level<-as.factor(customer_proactive_retention$Revenue_Level)
summary(customer_proactive_retention$Revenue_Level)

table(customer_proactive_retention$Churn_Level,customer_proactive_retention$Revenue_Level)
customer_proactive_retention$Customer_ID
library(dplyr)
#Lets Extract the Customer Id's of those who give high and medium revenue as well as  have high and medium Probability of churn 
customer_proactive_retention%>%filter(Revenue_Level=='High(1337.972-27321.500)'& Churn_Level=='High(0.3711780-0.8437166)')->Customer_prio_Target1
nrow(Customer_prio_Target1)

customer_proactive_retention%>%filter(Revenue_Level=='High(1337.972-27321.500)' & Churn_Level=='Medium(0.3200020-0.3711780)')->Customer_prio_Target2
nrow(Customer_prio_Target2)

customer_proactive_retention%>%filter(Revenue_Level=='Medium(719.226-1337.972)' & Churn_Level=='High(0.3711780-0.8437166)')->Customer_prio_Target3
nrow(Customer_prio_Target3)

customer_prio_Target<-rbind(Customer_prio_Target1,Customer_prio_Target2,Customer_prio_Target3)
nrow(customer_prio_Target)
names(customer_prio_Target)
names(customer_proactive_retention)
#Lets Extract Customer Id of Priority target segments 
customer_prio_Target<-customer_prio_Target[,c(44,95,96)]

dim(customer_prio_Target)

write.csv(customer_prio_Target,'Target Customer for Priority ProActive Retention.csv')

getwd()






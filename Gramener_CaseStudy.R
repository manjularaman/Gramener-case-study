rm(list=ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)
loan<-read.csv("loan.csv",stringsAsFactors = F)
##View(loan)
##Remove the not needed columns##
#1 Remove columns with only one values and NA
nrow(loan) #=39717

length(unique(loan$policy_code)) #=1
loan$policy_code<-NULL
length(unique(loan$application_type))#=INDIVIDUAL
loan$application_type<-NULL

length(unique(loan$acc_now_delinq)) #=0
loan$acc_now_delinq<-NULL


length(unique(loan$delinq_amnt))#=0
loan$delinq_amnt<-NULL

length(unique(loan$pymnt_plan))#=n
loan$pymnt_plan<-NULL

length(unique(loan$initial_list_status)) #=f
loan$initial_list_status<-NULL


sum(is.na(loan$mths_since_last_major_derog)) #=39717
loan$mths_since_last_major_derog<-NULL

sum(is.na(loan$annual_inc_joint))#=39717
loan$annual_inc_joint<-NULL

sum(is.na(loan$dti_joint))#=39717
loan$dti_joint<-NULL

sum(is.na(loan$verification_status_joint))#=39717
loan$verification_status_joint<-NULL

sum(is.na(loan$tot_coll_amt))#=39717
loan$tot_coll_amt<-NULL

sum(is.na(loan$tot_cur_bal))#=39717
loan$tot_cur_bal<-NULL

sum(is.na(loan$open_acc_6m))#=39717
loan$open_acc_6m<-NULL

sum(is.na(loan$open_il_6m))#=39717
loan$open_il_6m<-NULL

sum(is.na(loan$open_il_12m))#=39717
loan$open_il_12m<-NULL

sum(is.na(loan$open_il_24m))#=39717
loan$open_il_24m<-NULL

sum(is.na(loan$mths_since_rcnt_il))#=39717
loan$mths_since_rcnt_il<-NULL

sum(is.na(loan$total_bal_il))#=39717
loan$total_bal_il<-NULL

sum(is.na(loan$il_util))#=39717
loan$il_util<-NULL

sum(is.na(loan$open_rv_12m))#=39717
loan$open_rv_12m<-NULL

sum(is.na(loan$open_rv_24m))#=39717
loan$open_rv_24m<-NULL

sum(is.na(loan$max_bal_bc))#=39717
loan$max_bal_bc<-NULL

sum(is.na(loan$all_util))#=39717
loan$all_util<-NULL

sum(is.na(loan$total_rev_hi_lim))#=39717
loan$total_rev_hi_lim<-NULL

sum(is.na(loan$inq_fi))#=39717
loan$inq_fi<-NULL

sum(is.na(loan$total_cu_tl))#=39717
loan$total_cu_tl<-NULL

sum(is.na(loan$inq_last_12m))#=39717
loan$inq_last_12m<-NULL

sum(is.na(loan$acc_open_past_24mths))#=39717
loan$acc_open_past_24mths<-NULL

sum(is.na(loan$avg_cur_bal))#=39717
loan$avg_cur_bal<-NULL

sum(is.na(loan$bc_open_to_buy))#=39717
loan$bc_open_to_buy<-NULL

sum(is.na(loan$bc_util))#=39717
loan$bc_util<-NULL

sum(is.na(loan$mo_sin_old_il_acct))#=39717
loan$mo_sin_old_il_acct<-NULL

sum(is.na(loan$mo_sin_old_rev_tl_op))#=39717
loan$mo_sin_old_rev_tl_op<-NULL

sum(is.na(loan$mo_sin_rcnt_rev_tl_op))#=39717
loan$mo_sin_rcnt_rev_tl_op<-NULL 

sum(is.na(loan$mo_sin_rcnt_tl))#=39717
loan$mo_sin_rcnt_tl<-NULL

sum(is.na(loan$mort_acc))#=39717
loan$mort_acc<-NULL

sum(is.na(loan$mths_since_recent_bc))#=39717
loan$mths_since_recent_bc<-NULL

sum(is.na(loan$mths_since_recent_bc_dlq))#=39717
loan$mths_since_recent_bc_dlq<-NULL

sum(is.na(loan$mths_since_recent_inq))#=39717
loan$mths_since_recent_inq<-NULL

sum(is.na(loan$mths_since_recent_revol_delinq))#=39717
loan$mths_since_recent_revol_delinq<-NULL

sum(is.na(loan$num_accts_ever_120_pd))#=39717
loan$num_accts_ever_120_pd<-NULL

sum(is.na(loan$num_actv_bc_tl))#=39717
loan$num_actv_bc_tl<-NULL

sum(is.na(loan$num_actv_rev_tl))#=39717
loan$num_actv_rev_tl<-NULL

sum(is.na(loan$num_bc_sats))#=39717
loan$num_bc_sats<-NULL

sum(is.na(loan$num_bc_tl))#=39717
loan$num_bc_tl<-NULL

sum(is.na(loan$num_il_tl))#=39717
loan$num_il_tl<-NULL

sum(is.na(loan$num_op_rev_tl))#=39717
loan$num_op_rev_tl<-NULL

sum(is.na(loan$num_rev_accts))#=39717
loan$num_rev_accts<-NULL

sum(is.na(loan$num_rev_tl_bal_gt_0))#=39717
loan$num_rev_tl_bal_gt_0<-NULL

sum(is.na(loan$num_sats))#=39717
loan$num_sats<-NULL

sum(is.na(loan$num_tl_120dpd_2m))#=39717
loan$num_tl_120dpd_2m<-NULL

sum(is.na(loan$num_tl_30dpd))#=39717
loan$num_tl_30dpd<-NULL

sum(is.na(loan$num_tl_90g_dpd_24m))#=39717
loan$num_tl_90g_dpd_24m<-NULL 

sum(is.na(loan$num_tl_op_past_12m))#=39717
loan$num_tl_op_past_12m<-NULL 

sum(is.na(loan$pct_tl_nvr_dlq))#=39717
loan$pct_tl_nvr_dlq<-NULL  

sum(is.na(loan$percent_bc_gt_75))#=39717
loan$percent_bc_gt_75<-NULL  

sum(is.na(loan$tot_hi_cred_lim))#=39717
loan$tot_hi_cred_lim<-NULL 

sum(is.na(loan$total_bal_ex_mort))#=39717
loan$total_bal_ex_mort<-NULL

sum(is.na(loan$total_bc_limit))#=39717
loan$total_bc_limit<-NULL

sum(is.na(loan$total_il_high_credit_limit))#=39717
loan$total_il_high_credit_limit<-NULL


##Remove columns with more than 90% one values(0) and rest NA  
table(loan$tax_liens) #=0->39678
loan$tax_liens<-NULL

table(loan$chargeoff_within_12_mths) #=0->39661
loan$chargeoff_within_12_mths<-NULL

table(loan$collections_12_mths_ex_med)#=0->39661
loan$collections_12_mths_ex_med<-NULL

##Remove columns that are auto generated (Id's) & duplicated
length(unique(loan$id)) #=39717
loan$id<-NULL

##Remove columns that are not relevant for the analysis
loan$url<-NULL
loan$desc<-NULL
loan$zip_code<-NULL

##Remove column that carry duplicate information or redundant
loan$int_rate<-NULL # Grade can be used
loan$sub_grade<-NULL # Grade can be used
loan$funded_amnt<-NULL # Loan Amount can be used

##Remove columns with calculated ( agregrate, summary)  or not unique for analysis 
loan$Installment<-NULL
loan$funded_amnt_inv<-NULL
loan$total_pymnt<-NULL
loan$total_pymnt_inv<-NULL
loan$total_rec_prncp<-NULL
loan$total_rec_int<-NULL
loan$total_rec_late_fee<-NULL
loan$recoveries<-NULL
loan$collection_recovery_fee<-NULL
loan$last_pymnt_amnt<-NULL
loan$next_pymnt_d<-NULL
loan$out_prncp<-NULL
loan$out_prncp_inv<-NULL
loan$last_credit_pull_d<-NULL
loan$title<-NULL

length(loan) #=27

###checking for empty values
##sapply (loan, function (x) length(which(loan$x == "" | loan$x == " " | loan$x == "0")))

summary(loan$term)
loan$term <- str_extract(loan$term, "[[:digit:]]+")
loan$term <- factor(loan$term)
summary(loan$term)

####Checking induvidual columns
sum(duplicated(loan$member_id)) #=Duplicated Columns=0
sum(is.na(loan$loan_amnt)) #=NA values=0

summary (loan$installment)
summary(loan$grade)
loan$grade <- factor(loan$grade)

length(which(loan$emp_title == ""))
###2453 empty rows, replacing with string UNKNOWN
loan$emp_title <- sub("^$","UNKNOWN",loan$emp_title)

###1075 rows with n/a, replacing it with NA
length(which(loan$emp_length == "n/a"))
loan$emp_length[which(loan$emp_length == "n/a")] <- 0 #only 6 records

#Converting the emp length to numeric factors
#loan$emp_length<-str_extract(loan$emp_length, "[[:digit:]]+")
#loan$emp_length<-as.factor(loan$emp_length)

####replacing "years" and "10+" with 11 and "< 1" with 0 to make the column a numeric
loan$emp_length <- str_replace_all(loan$emp_length,"years","")
loan$emp_length <- str_replace_all(loan$emp_length,"year","")
loan$emp_length <- str_replace_all(loan$emp_length,"10[+]","10")
loan$emp_length <- str_replace_all(loan$emp_length,"[<] 1","0")
loan$emp_length <- as.numeric(loan$emp_length)

summary(loan$home_ownership)
loan$home_ownership <- factor(loan$home_ownership)

summary(loan$verification_status) ###3 levels Not Verified,Source Verified,Verified
loan$verification_status <- factor(loan$verification_status) 

sum(is.na(loan$annual_inc)) #=0
length(which(loan$annual_inc == "" | loan$annual_inc == " " | loan$annual_inc == "0")) #=0

summary(loan$issue_d)
loan$issue_d <- factor(loan$issue_d) #=55 levels

summary(loan$loan_status) #=3 Levels Charged off,Current,Fully Paid
loan$loan_status <- factor(loan$loan_status)

summary(loan$purpose)
length(loan$purpose[which(loan$purpose == "" | loan$purpose == " " )]) #=0
sum(is.na(loan$purpose)) #=0
unique(loan$purpose) # 14 Levels, no blanks and NA

summary(loan$dti)
sum(is.na(loan$dti)) #=0

summary(loan$delinq_2yrs)
loan$delinq_2yrs <- factor(loan$delinq_2yrs)
summary(loan$delinq_2yrs) # 0 to 11 levels

summary(loan$inq_last_6mths)
loan$inq_last_6mths <- factor(loan$inq_last_6mths)
summary(loan$inq_last_6mths) # 0 to 8 levels

summary(loan$open_acc) # 2 - 44

summary(loan$revol_util)
sum(is.na(loan$revol_util)) #=0
length(loan$revol_util[which(loan$revol_util=="")]) # 50 blanks
####Removing blanks from revol_util and replacing with 0
loan$revol_util[which(loan$revol_util=="")] <- 0
####Removing % sign 
loan$revol_util <- as.numeric(str_replace_all(loan$revol_util,"[%]",""))

summary(loan$pub_rec_bankruptcies) # 0,1,2 , NA=697
loan$pub_rec_bankruptcies[which(is.na(loan$pub_rec_bankruptcies))] <- 0

summary(loan$pub_rec) # 0-4

summary(loan$last_pymnt_d)
summary(loan$total_acc) # 2- 90
summary(loan$revol_bal)
summary(loan$mths_since_last_record) # NA=36931
summary(loan$mths_since_last_delinq) # NA=25682
summary(loan$earliest_cr_line)
summary(loan$addr_state)
loan$addr_state=factor(loan$addr_state)


#Filter only the Fully Paid and Charged customers
loan<-loan[which(loan$loan_status!="Current"),]
nrow(loan) # 38577

# Data Cleaning completed

#PLOTS

# Loan Status vs Loan Amount

####Frequency plot
ggplot(loan,aes(loan_amnt)) + geom_histogram(stat="bin",binwidth = 1000) 
ggplot(loan,aes(loan_amnt)) + geom_histogram(stat="bin",binwidth = 1000) + facet_grid(loan_status~.)

ggplot(loan,aes(loan_amnt,fill=loan_status)) + geom_histogram(stat="bin",binwidth = 1000,position="dodge")
ggplot(loan,aes(loan_amnt,fill=loan_status)) + geom_histogram(stat="bin",binwidth = 1000,position="fill")
##The % charged off loans increase as amount is higher
##Comparing the charged_off and Fully_paid loan Amount stats
ggplot(loan,aes(y=loan_amnt,x=loan_status)) + geom_boxplot()
###the upper quartile of charged off is a bit higher

# Loan Status Vs Annaul Income

#Annual income. For a large income the loan is fully paid off. 
#So a high income is a driver of a paid loan

#Outlier treatment
q95 <- quantile(loan$annual_inc, seq(0, 1, 0.95))
q95[2] #q95 is = 1,30,000
loan_AINC_95Q <- loan[which(loan$annual_inc < q95[2]),]

ggplot(loan_AINC_95Q,aes(annual_inc)) + geom_histogram(stat="bin",binwidth = 10000) 
ggplot(loan_AINC_95Q,aes(annual_inc,fill=loan_status)) + geom_histogram(stat="bin",position="fill",binwidth = 10000) 
ggplot(loan_AINC_95Q,aes(y=annual_inc,x=loan_status)) + geom_boxplot()

loan_group <- group_by(loan_AINC_95Q,loan_status,annual_inc) 
loan_group_summary <- summarise(loan_group,avg_annual_inc=mean(annual_inc)) 
ggplot(loan_group_summary,aes(y=avg_annual_inc,x=loan_status))+geom_bar(stat="identity")  + labs(x = "loan_status", y = "average annual income" )

#-->clearly shows higher income more chances of fully paid

# Loan Statu Vs Term

ggplot(loan,aes(x=term,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
ggplot(loan,aes(x=term,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
#-->For shorter terms of 36 months the loan is fully paid off

# Loan Status Vs Grade

ggplot(loan,aes(x=grade,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
#-->For grade A,B,C, D and E the loan has a high chance of being paid off. Grades and interest rates
#-->are related by low interest rates mapping to A and B and C and high interest rates mapping to D E F and G
#-->So for low interest rates the loan is likely to be paid off fully
#-->the above plot shows that the % charged off increased as the load grade and hence the int_rate increases


# Loan Status Vs Purpose

ggplot(loan,aes(x=purpose)) + geom_bar(stat="count")+labs(x="purpose") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(loan,aes(x=purpose,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")+labs(x="purpose") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Largest number of loans are for the purpose of debt consolidation and credit card payment 
#Loans that are for renewable energy and small bussiness have the highest % of charged off loans 20-30%
#Loans for medical,car,credit card,wedding have 85% fully paid loans

# Loan Status Vs Emp_Length

ggplot(loan,aes(x=emp_length,fill=factor(loan_status))) + geom_bar(stat="count",position = "stack")
#-->The plot below shows that most loans are taken at 10# years of employment and < 1year, then 2 and 3 years

ggplot(loan,aes(x=emp_length,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
#-->The same plot showing the relative loan status shows that loan being charged off has no dependence on the emp_length

# Loan Status Vs Home ownership

ggplot(loan,aes(x=loan$home_ownership,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(loan,aes(x=loan$home_ownership,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
###More loans by people on rent and mortgage than own
###No trend observed on loan status based on home_ownership

# Loan Status Vs Installment

ggplot(loan,aes(installment)) + geom_histogram(stat="bin",binwidth = 100) 
ggplot(loan,aes(y=installment,x=loan_status)) + geom_boxplot()
ggplot(loan,aes(installment,fill=loan_status)) + geom_histogram(stat="bin",position="fill",binwidth = 100) 
#--> As the Installment goes up, there is a slight increasing tendency for charger off


# Loan Status Vs State
ggplot(loan,aes(x=addr_state)) + geom_bar(stat="count") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(loan,aes(x=addr_state,fill=factor(loan_status))) + geom_bar(stat="count",position="fill") +theme(axis.text.x = element_text(angle = 90, hjust = 1))
#-->State NE has 60% charged off and second largest is NV 20% loans charged off
#-->CA,NY,TX,FL largest number of loan takers


# Loan Status Vs DTI [ Debt to Income Ratio]

ggplot(loan,aes(dti)) + geom_histogram(stat="bin",binwidth = 5) 
ggplot(loan,aes(dti)) + geom_histogram(stat="bin",binwidth = 5) + facet_grid(loan_status~.)

ggplot(loan,aes(dti,fill=loan_status)) + geom_histogram(stat="bin",position="dodge",binwidth = 5)
ggplot(loan,aes(dti,fill=loan_status)) + geom_histogram(stat="bin",position="fill",binwidth = 5)

ggplot(loan,aes(y=dti,x=loan_status)) + geom_boxplot()

#-->A high ratio of dti is proportional to the debt payments and inversely to the income
#--> A slight increasing tendency of charged off when the dti goes up. The charged off
#--> is high between 10-20 dti values


# Loan Status Vs Revolving Utilization -> Credit used to Available Credit

ggplot(loan,aes(revol_util)) + geom_histogram(stat="bin",binwidth = 30) 
ggplot(loan,aes(revol_util)) + geom_histogram(stat="bin",binwidth = 30) + facet_grid(loan_status~.)

ggplot(loan,aes(revol_util,fill=loan_status)) + geom_histogram(stat="bin",position="dodge",binwidth = 30)
ggplot(loan,aes(revol_util,fill=loan_status)) + geom_histogram(stat="bin",position="fill",binwidth = 30)

ggplot(loan,aes(y=revol_util,x=loan_status)) + geom_boxplot()
#-->Higer revolving utilization tends to more charged off

# Loan Status Vs Total Accounts -> No of credit lines available for the borrower

ggplot(loan,aes(total_acc,fill=loan_status)) + geom_bar(stat="count",position="dodge")
ggplot(loan,aes(total_acc,fill=loan_status)) + geom_bar(stat="count",position="fill")
ggplot(loan,aes(y=total_acc,x=loan_status)) + geom_boxplot()
#--> Lower credit lines leads to higer charged off . Interesting tendency. As the total credit 
#--> lines goes up, charged off goes down  and once it
#--> crosses the median, it charged of goes up ( credit seeking behavior)


# Loan Status Vs Open Accounts

#Open accounts. 
ggplot(loan,aes(open_acc)) + geom_histogram(stat="bin",binwidth = 5) 
ggplot(loan,aes(open_acc)) + geom_histogram(stat="bin",binwidth = 5) + facet_grid(loan_status~.)

ggplot(loan,aes(open_acc,fill=loan_status)) + geom_histogram(stat="bin",binwidth = 5,position="dodge")
ggplot(loan,aes(open_acc,fill=loan_status)) + geom_histogram(stat="bin",binwidth = 5,position="fill")
ggplot(loan,aes(y=open_acc,x=loan_status)) + geom_boxplot()


# Loan Status Vs Months since last deliquency
#ggplot(loan,aes(x=loan$mths_since_last_delinq,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
#ggplot(loan,aes(x=loan$mths_since_last_delinq,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")

#-->Month since last delinquency. As the months from deliquncy increase the proportion of full paid off increases

# Loan Status Vs 30 day past due deliquency since last 2 years
ggplot(loan,aes(delinq_2yrs)) + geom_histogram(stat="count",binwidth = 5)
ggplot(loan,aes(x=delinq_2yrs,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
ggplot(loan,aes(x=delinq_2yrs,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")

##For 0 deliquencies 85% of the loans are fully paid off
###as the number of delinquincies incresed to 8, 50% are charged off 
#Fill plot is not good as the numbers are very less for increasing number of deliquencies
# Need to check the dodge and it shows . The number of past due incidences for majority of
#customers are 0. Very few customers has 30 past due incidenses of which single incidense are
#more.

# Loan Status Vs received bankruptcies
#Public received bankruptcies
loan_bankrupt <- filter(loan,pub_rec_bankruptcies!=0)

ggplot(loan_bankrupt,aes(pub_rec_bankruptcies)) + geom_histogram(stat="count")
ggplot(loan_bankrupt,aes(x=pub_rec_bankruptcies,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
ggplot(loan_bankrupt,aes(x=pub_rec_bankruptcies,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")

ggplot(loan,aes(pub_rec_bankruptcies)) + geom_histogram(stat="count")
ggplot(loan,aes(x=pub_rec_bankruptcies,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
ggplot(loan,aes(x=pub_rec_bankruptcies,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
#96% loans have 0 public recorded bankruptcies
#For 1 and 2 public record bankruptcies, 22% ,35% of total loans get charged off 

# Loan Status Vs Inquiries in last 6 months
ggplot(loan,aes(inq_last_6mths)) + geom_histogram(stat="count")
ggplot(loan,aes(x=loan$inq_last_6mths,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
ggplot(loan,aes(x=loan$inq_last_6mths,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
#Loans with 0 inquiries are 87% fully paid
# as number of inquiries increases to 7,  75% are fully paid and 25% are charged_off

# Loan Status Vs Public Records
#95% loans have 0 public record, but still have charged off
loan_public_rec <- filter(loan,loan$pub_rec!=0)
ggplot(loan_public_rec,aes(x=pub_rec,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
ggplot(loan_public_rec,aes(x=pub_rec,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
ggplot(loan,aes(x=pub_rec,fill=factor(loan_status))) + geom_bar(stat="count",position = "dodge")
ggplot(loan,aes(x=pub_rec,fill=factor(loan_status))) + geom_bar(stat="count",position = "fill")
#25% loans with 1 and 2 public records get charged off 


#Bivariate Analysis: Treemaps

#Quantifying the laon_status to reflect 1 for fully paid, -1 for charged off and 0 for current
loan$status <- ifelse(loan$loan_status == "Fully Paid",100,ifelse(loan$loan_status == "Charged Off",-100,50))
loan$status
loan$loan_amnt<- as.numeric(loan$loan_amnt)
loan$term <- as.numeric(loan$term)
loan$grade<-as.numeric(loan$grade)
loan$installment<-as.numeric(loan$installment)
loan$emp_length <- as.numeric(loan$emp_length)
loan$annual_inc<-as.numeric(loan$annual_inc)
loan$dti <- as.numeric(loan$dti)
loan$delinq_2yrs <- as.numeric(loan$delinq_2yrs)
loan$inq_last_6mths <- as.numeric(loan$inq_last_6mths)
loan$open_acc <- as.numeric(loan$open_acc)
loan$pub_rec <- as.numeric(loan$pub_rec)
loan$revol_bal <- as.numeric(loan$revol_bal)
loan$revol_util <- as.numeric(loan$revol_util)
loan$total_acc<-as.numeric(loan$total_acc)
loan$pub_rec_bankruptcies <- as.numeric(loan$pub_rec_bankruptcies)
loan$status <- as.numeric(loan$status)

mydata <- loan[,c(2,3,4,5,7,9,15,16,18,21,22,23,24,25,27,28)]
head(mydata)
cormat <- round(cor(mydata),2)
head(cormat,17)

melted_cormat <- melt(cormat)
head(melted_cormat)
#TReemap Expand the window to see the labels
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))


#Correlation matrix

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
  
}
# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
# Print the heatmap
print(ggheatmap)



ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))  

###Done!
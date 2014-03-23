library(gbm)
library(randomForest)
library(gplots)

clense_csv = function(csv_file_content) {
	# Ignore first line, if it contains info text.
	if (csv_file_content[1] != '.*,.*') {
		print("Skipping first line of CSV file")
		csv_file_content = csv_file_content[-1];
	}
	if (csv_file_content[1] != ".*,") {
		print("Adding missing , in the first line of CSV.")
		csv_file_content[1] = paste(csv_file_content[1], ",");
	}
	return (csv_file_content)
}

classify_purpose = function(purpose)
{  
  # PAST NEW
  #"car","Car financing"
  classified=as.character(purpose)
  classified[classified=="Wedding expenses"]="wedding"
  classified[classified=="Car financing"]="car"
  classified[classified=="Credit card refinancing"]="credit_card"
  classified[classified=="Debt consolidation"]="debt_consolidation"
  classified[classified=="Home improvement"]="home_improvement"
  classified[classified=="Home buying"]="house"
  classified[classified=="Debt consolidation"]="debt_consolidation"
  classified[classified=="Major purchase"]="major_purchase"
  classified[classified=="Medical expenses"]="medical"
  classified[classified=="Major purchase"]="major_purchase"
  classified[classified=="Moving and relocation"]="moving"
  classified[classified=="Other"]="other"
  classified[classified=="Business"]="small_business"
  classified[classified=="Vacation"]="vacation"
  #  "educational",
  #  "renewable_energy",  
  #  "wedding"
  
  return(classified)
}

new_work_dir <- paste(path.expand("~"), "\\lc\\", sep = "");
# Create the directory first time
dir.create(new_work_dir)
print(paste("Create work directory: ", new_work_dir))
setInternet2(TRUE)
in_funding_file <- paste(new_work_dir, "InFundingStats3.csv", sep="")
loan_stats_file <- paste(new_work_dir, "LoanStats.csv", sep="")
gbm_modal_file  <- paste(new_work_dir, "gbm.model", sep="")
download.file("http://www.lendingclub.com/fileDownload.action?file=InFundingStats3.csv&type=gen", in_funding_file, "internal", FALSE, "w", FALSE,)
# This is expensive process and we may want to cache this file.
# though this line can be easily commented out if the file already exist.
download.file("https://www.lendingclub.com/fileDownload.action?file=LoanStats.csv&type=gen", loan_stats_file, "internal", FALSE, "w", FALSE,)
# Load data
newloans=read.csv(textConnection(clense_csv(readLines(in_funding_file))), header=T, quote="\"")
#pastloans=read.csv(textConnection(clense_csv(readLines(loan_stats_file))), header=T)
# It is too expensive to clense this file.
pastloans=read.csv(loan_stats_file, header=T)

#########################################################################3
# Filtering criteria from the general looking at the loans
# 1. Loan purpose should be only Debt consolidation and Credit card refinancing
# 2. Loan amount is not more than 500 plus revolving balance (revol_bal)
# 3. Minimum annual income (annual_inc) is 48000
# To do - apply filters here, like loan type you are interested in.
newloans=newloans[newloans$purpose == 'Debt consolidation' | newloans$purpose == 'Credit card refinancing',]
newloans=newloans[newloans$loan_amnt  < newloans$revol_bal,]
newloans=newloans[newloans$annual_inc > 45000,]
newloans$earliest_cr_line <- as.Date(newloans$earliest_cr_line, format="%m/%d/%Y")
#newloans=newloans[newloans$earliest_cr_line < as.Date('01/01/2004', format="%m/%d/%Y"),]

pastloans=pastloans[pastloans$Status=='Charged Off' | pastloans$Status=='Fully Paid',]
pastloans$DV=as.numeric(pastloans$Status=='Charged Off')

# make sure factors are ok
pastlength=nrow(pastloans)
newlength=nrow(newloans)

trainrows=c(rep(T,pastlength),rep(F,newlength))
testrows=c(rep(F,pastlength),rep(T,newlength))

combined=data.frame(list(DV=c(pastloans$DV,rep(NA,newlength))))
combined$sub_grade=as.factor(c(as.character(pastloans$CREDIT.Grade), as.character(newloans$sub_grade)))
combined$emp_len=as.factor(c(as.character(pastloans$Employment.Length), as.character(newloans$emp_length)))
combined$loan_amt=as.numeric(c(pastloans$Amount.Requested, newloans$loan_amnt))
combined$addr_state=as.factor(c(as.character(pastloans$State), as.character(newloans$addr_state)))
combined$home_ownership=as.factor(c(as.character(pastloans$Home.Ownership), as.character(newloans$home_ownership)))
combined$term=as.factor(c(substr(pastloans$Loan.Length,0,2), as.character(newloans$term)))
combined$int_rate=as.numeric(
  c(
    substr(as.character(pastloans$Interest.Rate),1, nchar(as.character(pastloans$Interest.Rate))-1)
    , as.numeric(as.character(newloans$int_rate))
    )
  )
combined$purpose=as.factor(
  c(
    as.character(pastloans$Loan.Purpose)
    , classify_purpose(newloans$purpose)
  )
)
combined$income=c(as.numeric(pastloans$Monthly.Income)
                  , as.numeric(newloans$annual_inc/12)
                  )
#				
#combined$fico_range=as.factor(
  #c(
#    as.character(pastloans$FICO.Range)
#    , paste(newloans$fico_range_low,newloans$fico_range_high,sep='-')
#  )
#)
combined$revolving_line_utilization=c(
  as.numeric(substr(as.character(pastloans$Revolving.Line.Utilization),1,nchar(as.character(pastloans$Revolving.Line.Utilization))-1))
  ,as.numeric(as.character(newloans$revol_util))
  )

# New only
combined$url=''
combined$url[testrows]=as.character(newloans$url)
combined$funded_amnt=''
combined$funded_amnt[testrows]=as.character(newloans$funded_amnt)

# Derived features
combined$loan_amt.binned = round(as.numeric(combined$loan_amt)/1000)*1000
combined$loan_amt.mod500 = as.numeric(combined$loan_amt %% 500)
combined$loan_amt.mod100 = as.numeric(combined$loan_amt %% 100)
combined$loan_amt.mod1000z = as.numeric(0==as.numeric(combined$loan_amt %% 1000))

#,"Debt.To.Income.Ratio"
#,"Revolving.Line.Utilization"
#,"City"
whitelist=c(
  "sub_grade"
  #,"earliest_cr_line"
  ,"addr_state"
  ,"loan_amt"
  ,"loan_amt.binned"
  ,"loan_amt.mod1000z"
  ,"loan_amt.mod100"
  ,"loan_amt.mod500"
  ,"term"
  ,"income"
  ,"purpose"
  #,"fico_range"
  ,"revolving_line_utilization"
  ,"home_ownership" 
  ,"emp_len"

  #                               ,"Delinquencies..Last.2.yrs."
  #                               ,"Open.CREDIT.Lines"
  #                               ,"Inquiries.in.the.Last.6.Months" 
)


traindata=combined[trainrows,]

# Fit GBM
gbmmodel = gbm(DV ~ .
               , data=traindata[,c(whitelist,'DV')]
               , distribution="bernoulli"
               , n.trees = 100
               , shrinkage = 0.01
               , interaction.depth = 5
               , bag.fraction = 0.5
               , n.minobsinnode = 5
               , keep.data=TRUE
               , verbose=TRUE
)
save(file=gbm_modal_file, list=c('gbmmodel','traindata'))
summary(gbmmodel)

testdata=combined[testrows,]

testdata$pred_default_rate=predict.gbm(gbmmodel
                                       ,newdata=testdata[,whitelist]
                                       ,n.trees=100
                                       ,type='response'
                                       )
# pdefault * (-1) + (1-pdefault) * (1) * interest = 0 for "break even rate"
# interest = pdefault / (1-pdefault)
testdata$profitability=(testdata$int_rate/100)-(testdata$pred_default_rate/(1-testdata$pred_default_rate))
orderedloans = testdata[order(testdata$profitability,decreasing=T),]
head(orderedloans[orderedloans$term=='36',
                  c('term'
                    ,'profitability'
                    ,'pred_default_rate'
                    ,'int_rate'
                    ,'loan_amt'
                    , "purpose"
                    , "income"
                    , "funded_amnt"
                    ,'sub_grade'
                    , 'url'
                  )
                  ]
     , n=50
)

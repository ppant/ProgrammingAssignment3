## Programming assignment 3 -- First function
## This takes 2 arguments; the 2-char name of the state and an outcone name like "Heart attack" etc 
best<- function (state,outcome) {
  ## Read data from outcome-of-care-measures.csv file .. 
  ## outcome<-read.csv("outcome-of-care-measures.csv",colClasses = "character")
  csvdata<-read.csv("outcome-of-care-measures.csv", na.strings = "Not avaliable", stringsAsFactors = FALSE)
   
  ##print (cvsdata)
  ## Check the outcome is valid?
  ## Make a Probable outcome vector
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { stop("invalid outcome")}
  ## Check the state is valid?
  # 2 letter state code in which the hospital is located (7th col of .csv file)
  validState = unique(csvdata[,7])
  if (!state %in% validState) stop("invalid state")
  
  ## convert outcome name into column name
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  ##print (fullColName)
  ##print (outcome)
  ##print (validOutcome)
  colName <- fullColName[match(outcome,validOutcome)]
  ##print (colName)
  ## Returns hospital name in that state with lowest 30 days death rate
  fulldata.state <- csvdata[csvdata$State==state,]
  ##print (head(fulldata.state))
  id <- which.min(as.double(fulldata.state[,colName]))
  ## Return hospital name on given state and outcome, lowest mortality rate
  fulldata.state[id,"Hospital.Name"]
}
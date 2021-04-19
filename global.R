library(rjson)
library(RCurl)
library(dplyr)


data <-  RJSONIO::fromJSON("https://api.covid19india.org/data.json")

data_testing <- data$tested
df_testing <- data.frame(testpositivityrate=integer(), testedasof=as.Date(character()), testspermillion=integer(), totalindividualstested=integer(),
                         totalpositivecases=integer(), totalsamplestested=integer(), individualstestedperconfirmedcase=integer(), stringsAsFactors=FALSE)
for (i in 1: length(data_testing)){
  z = unlist(data_testing[i])
  row <- c(z["testpositivityrate"], z["testedasof"], z["testspermillion"], z["totalindividualstested"], z["totalpositivecases"], z["totalsamplestested"], z["individualstestedperconfirmedcase"])
  df_testing <- rbind(df_testing, row)
  names(df_testing) <- c("testpositivityrate", "testedasof", "testspermillion", "totalindividualstested", "totalpositivecases", "totalsamplestested", "individualstestedperconfirmedcase")
}

df_testing <- as.data.frame(df_testing)
df_testing$testedasof <- as.Date(df_testing$testedasof, "%d/%m/%Y")
df_testing <- df_testing[!is.na(df_testing$testedasof),]
df_testing$testspermillion <- as.numeric(df_testing$testspermillion)
df_testing$totalsamplestested <- as.numeric(df_testing$totalsamplestested)

data_statewise <- data$statewise

data <-  data["cases_time_series"]
df <- data.frame(dailyconfirmed=integer(), dailydeceased=integer(), dailyrecovered=integer(), reportdate=as.Date(character()),
                 totalconfirmed=integer(), totaldeceased=integer(), totalrecovered=integer(), stringsAsFactors=FALSE)

for (i in 1: length(data$cases_time_series)){
  z = unlist(data$cases_time_series[i])
  report_date = paste0(unlist(data$cases_time_series[i])["date"] ,"2020")
  #report_date = as.Date(x,"%d %B %Y")
  #print(typeof(report_date))
  row <- c(z["dailyconfirmed"], z["dailydeceased"], z["dailyrecovered"], report_date, z["totalconfirmed"], z["totaldeceased"], z["totalrecovered"])
  df <- rbind(df, row)
  names(df) <- c("dailyconfirmed", "dailydeceased", "dailyrecovered", "reportdate", "totalconfirmed", "totaldeceased", "totalrecovered")

}

df$reportdate <- as.Date(df$reportdate, "%d %B %Y")
df$dailyconfirmed <- as.numeric(df$dailyconfirmed)
df$dailydeceased <- as.numeric(df$dailydeceased)
df$dailyrecovered <- as.numeric(df$dailyrecovered)
df$totalconfirmed <- as.numeric(df$totalconfirmed)
df$totaldeceased <- as.numeric(df$totaldeceased)
df$totalrecovered <- as.numeric(df$totalrecovered)
df$totalactive <- df$totalconfirmed - (df$totalrecovered + df$totaldeceased)
df$dailyactive <- df$dailyconfirmed - (df$dailyrecovered + df$dailydeceased)

# state_daily <-  RJSONIO::fromJSON("https://api.covid19india.org/states_daily.json")
state_daily = read.csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv")  
state_daily$Date <- as.Date(state_daily$Date, "%d-%b-%y")

status = {}
status["c"] = "Confirmed"
status["r"] = "Recovered"
status["d"] = "Deceased"
status["a"] = "Active"

maxStateDate = max(state_daily$Date)
state_max = state_daily %>% filter(Date == maxStateDate)
## removed Date and TT columns
state_max <- state_max[, -c(1,3)]
state_max = t(state_max)
colnames(state_max) <- state_max[1,]
state_max <- cbind(State = rownames(state_max), state_max)

state_max <- state_max[(2:nrow(state_max)), ]
state_max <- as.data.frame(state_max)
state_max$Recovered <- as.numeric(state_max$Recovered)
state_max$Confirmed <- as.numeric(state_max$Confirmed)
state_max$Deceased <- as.numeric(state_max$Deceased)
state_max$Active <- state_max$Confirmed - (state_max$Recovered + state_max$Deceased)

## Patient wise data
if (!exists("patient_data1")){
  print("in patient data")
  patient_data1 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data1.json")
  patient_data2 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data2.json")
  patient_data3 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data3.json")
  patient_data4 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data4.json")
  patient_data5 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data5.json")
  patient_data6 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data6.json")
  patient_data7 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data7.json")
  patient_data8 <-  RJSONIO::fromJSON("https://api.covid19india.org/raw_data8.json")  
}

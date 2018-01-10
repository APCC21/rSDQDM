ReadStationData<-function(Path_OBS,STN_Num,Start_Year,End_Year,...){

  # Station data reading function
  # The period of observation is from 1973 to 2010, so users can extract observation by selecting starting and ending years
  # STN_Num: Stnation number (e.g. 090, 100,...),
  # Start_Year: Starting year
  # End_Year: Ending year
  # STN_OBS: PRCP, TMAX, TMIN

  srchstr = paste("*", STN_Num, "*.csv", sep="")
  STN_path=list.files(Path_OBS, pattern = glob2rx(srchstr), full.names = T)
  #STN_path=paste(Path_OBS,STN_Num,".daily.19730101-20151231_QC.asc",sep="")
  DATA_STN <-read.csv(STN_path, header = T)
  #names( DATA_STN ) <- c("IYY", "IMM", "IDD","PRCP","TAVG","TMAX","TMIN","DEWP","WAVG","WMAX","HAVE","HMIN")
  names( DATA_STN ) <- c("IYY", "IMM", "IDD","PRCP","TMAX","TMIN","WSPD","RHUM","RSDS","SHIN","CLOD","TAVG")
  missing<-which(DATA_STN<=-99,arr.ind=T)
  DATA_STN[missing]<-NA
  STN_OBS <- DATA_STN[DATA_STN$IYY>=Start_Year & DATA_STN$IYY<=End_Year,]
  STN_OBS <- cbind(STN_OBS$PRCP,STN_OBS$TMAX,STN_OBS$TMIN,STN_OBS$WSPD,STN_OBS$RHUM,STN_OBS$RSDS)
  colnames(STN_OBS) = c("PRCP", "TMAX", "TMIN", "WSPD", "RHUM", "RSDS")
  return(STN_OBS)
}

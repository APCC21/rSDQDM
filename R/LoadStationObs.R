LoadStationObs <- function(stnobsdir, stnfile, syear_obs, eyear_obs, syear_his, eyear_his)
{
  #Eum added emission scenario: it should be moved to input file (e.g. temp.txt)
  #Aug. 2017
  #==================================================================================================
#  print(file.path(stnobsdir, stnfile))
  Station_Info <- read.csv(file.path(stnobsdir, stnfile), header=TRUE, col.names = c("STN_Lon", "STN_Lat","Height", "STN_Num", "STN_Name", "STN_SYear"))
  attach(Station_Info)
  Number_STN=as.numeric(length(Station_Info[,1]))

  First_Yr_obs=EnvList$syear_obs
  End_Yr_obs=EnvList$eyear_obs
  First_Yr_Hist=EnvList$syear_his
  End_Yr_Hist=EnvList$eyear_his
  #=====================================================
  # These two variables need to move to input file
  First_Yr_Future=as.numeric(End_Yr_Hist)+1
  End_Yr_Future=2099
  #=====================================================
  bd.obs<-as.Date(paste(First_Yr_obs,"-01-01",sep=""))
  ed.obs<-as.Date(paste(End_Yr_obs,"-12-31",sep=""))
  bd.Ref<-as.Date(paste(First_Yr_Hist,"-01-01",sep=""))
  ed.Ref<-as.Date(paste(End_Yr_Hist,"-12-31",sep=""))
  Date.obs<-as.Date((seq(bd.obs,ed.obs,by="1 day")))
  Date.Ref<-as.Date((seq(bd.Ref,ed.Ref,by="1 day")))

  Num_days_obs<-as.numeric(length(Date.obs))
  Num_days_Ref<-as.numeric(length(Date.Ref))

  bd.All<-as.Date(paste(First_Yr_Hist,"-01-01",sep=""))  #1976
  ed.All<-as.Date(paste(End_Yr_Future,"-12-31",sep=""))      #2099
  Date.All<-as.Date((seq(bd.All,ed.All,by="1 day")))

  #=======================================================================================================================
  # Read observation station data
  #=======================================================================================================================
  STN_Data_PRCP=array(0,dim=c(Num_days_obs,Number_STN))
  STN_Data_TMAX=array(0,dim=c(Num_days_obs,Number_STN))
  STN_Data_TMIN=array(0,dim=c(Num_days_obs,Number_STN))
  STN_Data_WSPD=array(0,dim=c(Num_days_obs,Number_STN))
  STN_Data_RHUM=array(0,dim=c(Num_days_obs,Number_STN))
  STN_Data_RSDS=array(0,dim=c(Num_days_obs,Number_STN))

  for (i_STN in 1:Number_STN) {
    cat("Station data reading...STN:", i_STN, "out of", Number_STN, "\n")
    STN_Num_text = STN_Num[i_STN]
    STN_Data<-ReadStationData(stnobsdir,STN_Num_text,First_Yr_obs,End_Yr_obs)  #STN_Data_Read function

    STN_Data_PRCP[,i_STN]<-STN_Data[,1]
    STN_Data_TMAX[,i_STN]<-STN_Data[,2]
    STN_Data_TMIN[,i_STN]<-STN_Data[,3]
    STN_Data_WSPD[,i_STN]<-STN_Data[,4]
    STN_Data_RHUM[,i_STN]<-STN_Data[,5]
    STN_Data_RSDS[,i_STN]<-STN_Data[,6]
  }

  STN_Data_PRCP<-data.frame(STN_Data_PRCP); STN_Data_TMAX<-data.frame(STN_Data_TMAX);STN_Data_TMIN<-data.frame(STN_Data_TMIN)
  names(STN_Data_PRCP)<-STN_Num; names(STN_Data_TMAX)<-STN_Num; names(STN_Data_TMIN)<-STN_Num
  STN_Data_PRCP<-cbind(Date.obs,STN_Data_PRCP)
  STN_Data_TMAX<-cbind(Date.obs,STN_Data_TMAX)
  STN_Data_TMIN<-cbind(Date.obs,STN_Data_TMIN)
  STN_Data_WSPD<-data.frame(STN_Data_WSPD); STN_Data_RHUM<-data.frame(STN_Data_RHUM);STN_Data_RSDS<-data.frame(STN_Data_RSDS)
  names(STN_Data_WSPD)<-STN_Name; names(STN_Data_RHUM)<-STN_Name; names(STN_Data_RSDS)<-STN_Name
  STN_Data_WSPD<-cbind(Date.Ref,STN_Data_WSPD)
  STN_Data_RHUM<-cbind(Date.Ref,STN_Data_RHUM)
  STN_Data_RSDS<-cbind(Date.Ref,STN_Data_RSDS)

  detach(Station_Info)
}

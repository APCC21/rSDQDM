NCcombine<-function(Folder,GCM_name,Variable,Ems_sce,Start_Year,End_Year,...){
  #==============================================================================================================
  # Variable: "pr","tasmax","tasmin","sfcWind","rsds", "rhs"
  # Ems_sce: "historical", "rcp45", "rcp85"
  # Start_Year: Starting year (Target period)
  # End_Year: Ending year (Target period)
  # Folder <- folder
  # GCM_name <-GCM_Selected
  # Variable <- "pr"
  # Ems_sce <- rcp_sce
  # Start_Year <- First_Yr_Future
  # End_Year <- End_Yr_Future
  # Ems_sce <- "historical"
  # Start_Year <- 1976
  # End_Year <- 2005
  Pattern_word<-paste(Variable,".*",GCM_name,".",Ems_sce,".*\\.nc",sep="")
  filelist<-list.files(path=Folder,pattern=Pattern_word,full.names = FALSE)
  N_file<-length(filelist)
  if(GCM_name=="MIROC5"&Ems_sce=="historical"){N_file <- N_file-1}
  File.name.last<-unlist(strsplit(filelist[N_file],"_"))
  GCM_initial<-unlist(strsplit(filelist[N_file],"_"))[3]  #GCM name
  Period_name<-unlist(strsplit(filelist[N_file],"_"))[4]  #Period (e.g. historical, rcp45, rcp85)
  Ed.time<-unlist(strsplit(filelist[N_file],"_"))[6]      #Ending time period (e.g. 19760101-20051231.nc)

  if (substr(GCM_initial,1,6)=="HadGEM"){
    if(substr(Ed.time,10,17)=="20051130") {
      Pattern_word2<-paste(Variable,".*",GCM_name,".","rcp45",".*\\.nc",sep="")
      filelist2<-list.files(path=Folder,pattern=Pattern_word2,full.names = FALSE)
      filelist[N_file+1]<-filelist2[1]
    }
  }

  # Determine the array size of NCData.All
  Matrix.size.NC<-InfoNCfiles(Folder,filelist)   #list(lon,lat,Ndays_NC_Period)
  nlon<-as.numeric(length(Matrix.size.NC$lon))
  nlat<-as.numeric(length(Matrix.size.NC$lat))
  if(substr(Ed.time,14,17)=="1230" & !substr(GCM_initial,1,6)=="HadGEM"){
    NCData.All=array(0,dim=c(nlon,nlat,(Matrix.size.NC$Ndays_NC_Period+1)))
  } else {
    NCData.All=array(0,dim=c(nlon,nlat,Matrix.size.NC$Ndays_NC_Period))
  }


  Nday_NCData.All=0
  Nday_NCData.All.bg=0
  Nday_NCData.All.ed=0

  for (i in 1:N_file) {
    split_GCMlist<-unlist(strsplit(filelist[i],"_"))[3]  #GCM name
    split_datelist<-unlist(strsplit(filelist[i],"_"))[6] #time period (e.g. 19760101-20051231.nc)
    yy_bg<-substr(split_datelist,1,4); mm_bg<-substr(split_datelist,5,6);dd_bg<-substr(split_datelist,7,8)
    yy_ed<-substr(split_datelist,10,13); mm_ed<-substr(split_datelist,14,15);dd_ed<-substr(split_datelist,16,17)
    bd<-as.Date(paste(yy_bg,"-",mm_bg,"-",dd_bg,sep=""))
    ed<-as.Date(paste(yy_ed,"-",mm_ed,"-",dd_ed,sep=""))

    if (substr(split_GCMlist,1,6)=="HadGEM") {
      if (mm_ed=="10" && dd_ed=="30") {
        ed<-as.Date(paste(yy_ed,"-",mm_ed,"-","31",sep=""))
      } else if (mm_ed=="12" && dd_ed=="30") {
        ed<-as.Date(paste(yy_ed,"-",mm_ed,"-","31",sep=""))
		}
    }
	  if (mm_ed=="12" && dd_ed=="30") {
        ed<-as.Date(paste(yy_ed,"-",mm_ed,"-","31",sep=""))
    }
    if(N_file==1) {
      Date_start_NC<-bd
      Date_end_NC<-ed
    } else {
      if(i==1) {
        Date_start_NC<-bd
      } else if (i==N_file) {
        Date_end_NC<-ed
      }
    }

    Date_NC<-as.Date(seq(bd,ed,by="1 day"))
    Num_days_Cal<-as.numeric(length(Date_NC))
    Nday_NCData.All.bg=Nday_NCData.All+1
    Nday_NCData.All.ed=Nday_NCData.All+Num_days_Cal


      if(substr(split_GCMlist,1,6)=="HadGEM"){
        Read.NCData<-Read_HadGEM(Folder,filelist[i],Num_days_Cal,Variable,split_datelist)
        Temp.NCData<-Read.NCData$nc_Var_filling
        lon.NCData<-Read.NCData$lon
        lat.NCData<-Read.NCData$lat
      } else {
        Read.NCData<-ReadNC(Folder,filelist[i],Num_days_Cal,Variable,yy_bg,yy_ed,End_Year)
        Temp.NCData<-Read.NCData$nc_Var_filling
        lon.NCData<-Read.NCData$lon
        lat.NCData<-Read.NCData$lat
      }
      NCData.All[,,(Nday_NCData.All.bg:Nday_NCData.All.ed)]<-Temp.NCData
      Nday_NCData.All=Nday_NCData.All.ed


  }  # i
  #===================================================================
  Date_NC_all<-as.Date(seq(Date_start_NC,Date_end_NC,by="1 day"))
  Date_start_Tar<-as.Date(paste(Start_Year,"-01-01",sep=""))
  Date_end_Tar<-as.Date(paste(End_Year,"-12-31",sep=""))
  Date_Tar_Period<-as.Date(seq(Date_start_Tar,Date_end_Tar,by="1 day"))
  Num_days_NC_All<-as.numeric(length(Date_NC_all))
  Num_days_Tar_Period<-as.numeric(length(Date_Tar_Period))
  NCData.Tar.Period=array(0,dim=c(length(lon.NCData),length(lat.NCData),Num_days_Tar_Period))

  Nday_Tar_Period=0
  for (iDate in 1:Num_days_NC_All) {
    yr.NC.temp<-as.numeric(substr(Date_NC_all[iDate],1,4))
    if (yr.NC.temp>=Start_Year && yr.NC.temp<=End_Year) {
      Nday_Tar_Period=Nday_Tar_Period+1
      if(Variable=="pr") {
        NCData.Tar.Period[,,Nday_Tar_Period]<-NCData.All[,,iDate]*3600.0*24.0 #convert mm/s to mm/day
      } else if (Variable=="tasmax" || Variable=="tasmin") {
        NCData.Tar.Period[,,Nday_Tar_Period]<-NCData.All[,,iDate]-273.15  #convert Kelvin to Celsius
      } else {
        NCData.Tar.Period[,,Nday_Tar_Period]<-NCData.All[,,iDate]
      }
    }
  }

  return(list(NCData.Tar.Period=NCData.Tar.Period,lon.NCData=lon.NCData,lat.NCData=lat.NCData))
}

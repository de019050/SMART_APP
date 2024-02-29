# # connector from Marco
# # Snowflake odbc Treiber von Sanofi appstore muss lokal installiert werden
# 
# 
library(RODBC)
#library(tidyverse)
library(stringr)
library(tidyr)
library(readr)
library(dplyr)
library(moments)
library(ggplot2)
library(plotly)
# 
# #source('C:/Users/De019050/OneDrive - Sanofi/Documents/GitHub/3951_verification/ISO3951Verification/R/source_functions.R')
# 
# #connect <- odbcDriverConnect(connection="Driver={SnowflakeDSIIDriver};
#                                          Server=sanofi-emea_ia.snowflakecomputing.com;
#                                          uid=PATRIC.STRACKE@SANOFI.COM;
#                                          role=RIDR_DEV_SYSADMIN;
#                                          warehouse=RIDR_DEV_WH_ADHOC;
#                                          authenticator=externalbrowser")
# 
# 
# #SelectStatement = "SELECT * FROM RIDR_DEV.TMP_PS.MY_RIDR_RESULT"
# 
# 
# ALL_REFLEX = sqlQuery(connect,  SelectStatement)
# head(ALL_REFLEX)
# 
#####################################
# Get Material

# Function to check for Dupi 150mg R_D_J
# check_pattern1 <- function(s) {
#   contains_dupi <- str_detect(s, regex("dupi", ignore_case = TRUE))
#   contains_150mg <- str_detect(s, "150mg")
#   contains_2ml <- str_detect(s, "2ml")
#   contains_dupi & contains_150mg & contains_2ml
# }
# 
# #  Function to check for Dupi 175mg R_D_M
# check_pattern2 <- function(s) {
#   contains_dupi <- str_detect(s, regex("dupi", ignore_case = TRUE))
#   contains_175mg <- str_detect(s, "175mg")
#   contains_114ml <- str_detect(s, "1.14ml")
#   contains_dupi & contains_175mg & contains_114ml
# }
# 
# 
# #  Function to check for Praluent 150mg R_D_M
# check_pattern3 <- function(s) {
#   contains_pralu <- str_detect(s, regex("praluent", ignore_case = TRUE))
#   contains_150mg <- str_detect(s, "150mg")
#   contains_pralu & contains_150mg
# }
# 
# #  Function to check for Praluent 75mg R_D_M
# check_pattern4 <- function(s) {
#   contains_pral <- str_detect(s, regex("praluent", ignore_case = TRUE))
#   contains_75mg <- str_detect(s, "75mg")
#   contains_pral & contains_75mg
# }
# 
# 
# #  Function to check for sar445088,
# check_pattern5 <- function(s) {
#   contains_sar445088 <- str_detect(s, regex("sar445088", ignore_case = TRUE))
#   contains_2ml <- str_detect(s, "2ml")
#   contains_sar445088 & contains_2ml
# }
# 
# #  Function to check for sar440340
# check_pattern6 <- function(s) {
#   contains_sar440340 <- str_detect(s, regex("sar440340", ignore_case = TRUE))
#   contains_2ml <- str_detect(s, "2ml")
#   contains_sar440340 & contains_2ml
# }
# 
# 
# #  Function to check for Firusiran
# check_pattern7 <- function(s) {
#   contains_fit <- str_detect(s, regex("fit", ignore_case = TRUE))
#   contains_fit
# }
# 
# #  Function to check for Firusiran (sar439774)
# check_pattern8 <- function(s) {
#   contains_sar439774 <- str_detect(s, regex("sar439774", ignore_case = TRUE))
#   contains_sar439774
# }
# 
# # Function to check for Alirocumab 150mg R_D_J
# check_pattern9 <- function(s) {
#   contains_ali <- str_detect(s, regex("alirocumab", ignore_case = TRUE))
#   contains_150mg <- str_detect(s, "150mg")
#   contains_2ml <- str_detect(s, "2ml")
#   contains_ali & contains_150mg & contains_2ml
# }


# alldat <- ALL_REFLEX|>
#   #select(auftr_nr,SAP_PRUEFLOS,status,erst_datum,frei_datum,mat_nr,charge,mat_bez,chargengroesse,losgroesse,
#   #       met_bez,met_nr,met_d_nr,met_d_bez,param_nr,param_bez,e_wert,e_text,geraet)%>%
#   #mutate(Erst_Datum = strptime(erst_datum, format="%m/%d/%Y %H:%M", tz = ""))%>%
#   #mutate(Frei_Datum = strptime(frei_datum, format="%m/%d/%Y %H:%M", tz = ""))%>%
# 
#   mutate(MAT_BEZ= tolower(MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern1),'R_D_J',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern2),'R_D_M',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern3),'R_P_150',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern4),'R_P_75',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern5),'R_SAR1',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern6),'R_SAR2',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern7),'R_F_M',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern8),'R_F_M',MAT_BEZ))%>%
#   mutate(MAT_BEZ= ifelse(sapply(MAT_BEZ, check_pattern9),'R_A_J',MAT_BEZ))%>%
# 
# 
#   #mutate(mat_bez= ifelse(str_detect(mat_bez,"150MG")==TRUE,"R_D_J",mat_bez))%>%
#   #mutate(mat_bez= ifelse(str_detect(mat_bez,"175MG")==TRUE,"R_D_M",mat_bez))%>%
#   #mutate(mat_bez= ifelse(str_detect(mat_bez,"0.5ML")==TRUE,"R_F_M",mat_bez))%>%
#   filter(!is.na(SAP_PRUEFLOS)) %>%
#   filter(SAP_PRUEFLOS<1.5e11) #nur Freigabe, keine Stabis
# 
# # Dateiname fÃ¼r die neue RDS-Datei
# new_rds_filename <- paste0("C:/Users/De019050/OneDrive - Sanofi/Documents/GitHub/alldat_", format(current_date, "%Y-%m-%d"), ".rds")
# 
# # DataFrame als RDS-Datei speichern
# saveRDS(alldat, file = new_rds_filename)
# print("Neue RDS-Datei erstellt und gespeichert.")
# 
# 
# 
# 
# # # pivoting table
# # # necessary to ease plot an analysis tatsks+
# # #
# 
# ALLREFLEX<-alldat%>%
#   filter(str_detect(alldat$STATUS,"r_f")==TRUE)%>%
#   #start  to get only released ones 05/12/2023
#   #filter(!is.na(SAP_PRUEFLOS))%>%
#   #filter(!is.na(frei_datum))%>%
#   #end  to get only released ones
#   filter(!is.na(IMPORT_FILE))%>%
#   filter(!is.na(AQL_AQL))%>%
#   filter(!is.na(GERAET))%>%
#   dplyr::select(-c(REF_AUFTR_NR,AUFTR_TYP,STORNO_GRUND,STORNO_DATUM,FOUR_EYES_CHECK,PREISLI_NR,PREISLI_VERS,KULI_BEZ,ANZ_ZYKLEN,PROB_NR,ZYKL_NR,PARAM_ART,PARAM_BEZ,UNTERGRENZEN,OBERGRENZEN))%>%
#   #filter(str_detect(alldat$status,"r_f")==TRUE)%>%
#   #filter(charge != "2F026")%>%
#   #filter(charge != "3F222")%>%
#   pivot_wider(id_cols=c(STATUS,FREI_DATUM,AUFTR_NR,MAT_NR,CHARGE,MET_D_NR,MAT_BEZ,GERAET,CHARGENGROESSE,ANZAHL_MUSTER,IMPORT_FILE1),
#               names_from = PARAM_NR,
#               values_from = E_WERT)%>%
#   mutate(order=row_number())
# #
# 
# 
# 
# 
# #
# #
# #
# 
# gs=gi=ga=gn=gis=0.001
# 
# simstart_ALL_Reflex_Num<-ALLREFLEX %>%
#   group_by(MAT_BEZ,CHARGE,CHARGENGROESSE) %>%
#   # calculate aggregate values per batch
#   summarize(N=n(),
#             mdose=mean(`R-P001`,na.rm=T), sdose=sd(`R-P001`,na.rm=T),gdose=(gs/sdose), mindose=min(`R-P001`,na.rm=T), maxdose=max(`R-P001`,na.rm=T),skdose=skewness(`R-P001`,na.rm=T),kdose=kurtosis(`R-P001`,na.rm=T),
#             mitime=mean(`R-P027`,na.rm=T),sitime=sd(`R-P027`,na.rm=T),gitime=(gis/sitime), minitime=min(`R-P027`,na.rm=T), maxitime=max(`R-P027`,na.rm=T),skitime=skewness(`R-P027`,na.rm=T),kitime=kurtosis(`R-P027`,na.rm=T),
#             midepth=mean(`R-P028`,na.rm=T),sidepth=sd(`R-P028`,na.rm=T),gidepth=(gi/sidepth), minidepth=min(`R-P028`,na.rm=T), maxidepth=max(`R-P028`,na.rm=T),skidepth=skewness(`R-P028`,na.rm=T),kidepth=kurtosis(`R-P028`,na.rm=T),
#             mact=mean(`R-P026`,na.rm=T),sact=sd(`R-P026`,na.rm=T),gact=(ga/sact), minact=min(`R-P026`,na.rm=T), maxact=max(`R-P026`,na.rm=T),skact=skewness(`R-P026`,na.rm=T),kact=kurtosis(`R-P026`,na.rm=T),
#             mnpos=mean(`R-P029`,na.rm=T),snpos=sd(`R-P029`,na.rm=T),gnpos=(gn/snpos), minnpos=min(`R-P029`,na.rm=T), maxnpos=max(`R-P029`,na.rm=T),sknpos=skewness(`R-P029`,na.rm=T),knpos=kurtosis(`R-P029`,na.rm=T))|>
#   mutate(NEWN = ifelse(N>=497,110,ifelse(N<=310 & N<=320,58,60)))%>%mutate(NEWN20=ceiling(NEWN*(1+0.2^2)))|>
#   #added constnts from ISO
#   mutate(kfac = ifelse(NEWN == 58, 2.592, ifelse(NEWN == 60, 2.5730, 2.449)))%>%
#   mutate(pfac = ifelse(NEWN == 58, 0.003872, ifelse(NEWN == 60, 0.004150, 0.006602)))%>%
#   mutate(fsfac = ifelse(NEWN == 58, 0.179, ifelse(NEWN == 60, 0.180, 0.180)))%>%
#   # added speclimits from EDOs
#   #first  one sided
#   mutate(dose_lsl= ifelse(MAT_BEZ == "R_F_M", 0.5, ifelse(MAT_BEZ == "R_D_M", 1.14, 2.0)))%>%
#   mutate(itime_usl= ifelse(MAT_BEZ == "R_F_M", 15, ifelse(MAT_BEZ == "R_D_M", 15, 15))) %>%
#   mutate(npos_usl= ifelse(MAT_BEZ == "R_F_M", 4.5, ifelse(MAT_BEZ == "R_D_M", 4.5, 4.6)))%>%
#   # now 2-sided
#   mutate(act_lsl= ifelse(MAT_BEZ == "R_F_M", 3, ifelse(MAT_BEZ == "R_D_M", 3, 3)))%>%
#   mutate(act_usl= ifelse(MAT_BEZ == "R_F_M", 16, ifelse(MAT_BEZ == "R_D_M", 16, 24)))%>%
#   mutate(idepth_lsl= ifelse(MAT_BEZ == "R_F_M", 4, ifelse(MAT_BEZ == "R_D_M", 4, 4)))%>%
#   mutate(idepth_usl= ifelse(MAT_BEZ == "R_F_M", 8, ifelse(MAT_BEZ == "R_D_M", 8, 8)))|>
#   na.omit()
# 
# 
# 
# 
# 
# # PREPARING DATAFOR tRENDINGAND CHARTING
# 
# mt<-as_tibble(simstart_ALL_Reflex_Num)
# head(mt)
# 
# 
# mt_long<-pivot_longer(mt,cols=c(mdose,mact,mitime,midepth,mnpos), names_to="EDO",values_to="Results")
# 
# 
# # creating equidistant data
# # Equidistant Sampling function
# # Funktion sie Index ausgeben soll basierend auf Input-Size
# # Create sample dataframe
# # Define function to perform equidistant sampling
# 
# # e.g. like
# # split_func(row_number(my_df%>%filter(batch=="2F170")),max(row_number(my_df%>%filter(batch=="2F170")))/38)
# # num [1:39] 1 14 27 40 53 66 79 93 106 119 ...
# # 1 14 27 40 53 66 79 93 106 119 132 145 158 172 185 198 211 224 237 250 264 277 290 303 316 329 343 356 369 382 395 408 422 435 448 461 474 487 500
# 
# split_func <- function(x, by) {
#   r <- diff(range(x))
#   #out <- if(any(is.infinite(c(x,by)))) c(10, 10) else seq(0, r - by - 1, by = by)
#   out <-  seq(0, r - by - 1, by = by)
#   
#   #out <- seq(0, r - by - 1, by = by)
#   #as_tibble(c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x)))
#   (c(round(min(x) + c(0, out - 0.51 + (max(x) - max(out)) / 2), 0), max(x)))
# }
# #not working du to infinite values??
# # equidistant  sampling using split_func()
# # col 15 is sample size changed for gamma-factor=0.2
# newdata<-tibble()
# newdatatemp1<-tibble()
# newdatatemp2<-tibble()
# ALL_REFLEX<-ALLREFLEX
# k<-1
# for(i in unique(simstart_ALL_Reflex_Num$CHARGE) ) {
#   a1<-split_func(row_number(ALL_REFLEX%>%filter(CHARGE==i)%>%dplyr::select(CHARGE)),max(row_number(ALL_REFLEX%>%filter(CHARGE==i)%>%dplyr::select(CHARGE)))/unlist(simstart_ALL_Reflex_Num[k,35]))
#   newdata<-ALL_REFLEX%>%filter(CHARGE==i)%>%slice(a1-1)#%>%mutate(order=row_number())
#   newdatatemp2<-rbind(newdatatemp1,newdata)
#   newdatatemp1<-newdatatemp2
#   k = k+1
# }
# 
# equidata<-newdatatemp1 %>%
#   group_by(MAT_BEZ,CHARGE,CHARGENGROESSE) %>%
#   # calculate aggregate values per batch
#   # calculate aggregate values per batch
#   summarize(N=n(),
#             mdose=mean(`R-P001`,na.rm=T), sdose=sd(`R-P001`,na.rm=T), mindose=min(`R-P001`,na.rm=T), maxdose=max(`R-P001`,na.rm=T),skdose=skewness(`R-P001`,na.rm=T),kdose=kurtosis(`R-P001`,na.rm=T),
#             mitime=mean(`R-P027`,na.rm=T),sitime=sd(`R-P027`,na.rm=T), minitime=min(`R-P027`,na.rm=T), maxitime=max(`R-P027`,na.rm=T),skitime=skewness(`R-P027`,na.rm=T),kitime=kurtosis(`R-P027`,na.rm=T),
#             midepth=mean(`R-P028`,na.rm=T),sidepth=sd(`R-P028`,na.rm=T), minidepth=min(`R-P028`,na.rm=T), maxidepth=max(`R-P028`,na.rm=T),skidepth=skewness(`R-P028`,na.rm=T),kidepth=kurtosis(`R-P028`,na.rm=T),
#             mact=mean(`R-P026`,na.rm=T),sact=sd(`R-P026`,na.rm=T), minact=min(`R-P026`,na.rm=T), maxact=max(`R-P026`,na.rm=T),skact=skewness(`R-P026`,na.rm=T),kact=kurtosis(`R-P026`,na.rm=T),
#             mnpos=mean(`R-P029`,na.rm=T),snpos=sd(`R-P029`,na.rm=T), minnpos=min(`R-P029`,na.rm=T), maxnpos=max(`R-P029`,na.rm=T),sknpos=skewness(`R-P029`,na.rm=T),knpos=kurtosis(`R-P029`,na.rm=T))|>
#   mutate(NEWN = ifelse(N>=497,110,ifelse(N<=310 & N<=320,58,60)))%>%mutate(NEWN20=ceiling(NEWN*(1+0.2^2)))|>
#   #added constnts from ISO
#   mutate(kfac = ifelse(NEWN == 58, 2.592, ifelse(NEWN == 60, 2.5730, 2.449)))%>%
#   mutate(pfac = ifelse(NEWN == 58, 0.003872, ifelse(NEWN == 60, 0.004150, 0.006602)))%>%
#   mutate(fsfac = ifelse(NEWN == 58, 0.179, ifelse(NEWN == 60, 0.180, 0.180)))%>%
#   # added speclimits from EDOs
#   #first  one sided
#   mutate(dose_lsl= ifelse(MAT_BEZ == "R_F_M", 0.5, ifelse(MAT_BEZ == "R_D_M", 1.14, 2.0)))%>%
#   mutate(itime_usl= ifelse(MAT_BEZ == "R_F_M", 15, ifelse(MAT_BEZ == "R_D_M", 15, 15))) %>%
#   mutate(npos_usl= ifelse(MAT_BEZ == "R_F_M", 4.5, ifelse(MAT_BEZ == "R_D_M", 4.5, 4.6)))%>%
#   # now 2-sided
#   mutate(act_lsl= ifelse(MAT_BEZ == "R_F_M", 3, ifelse(MAT_BEZ == "R_D_M", 3, 3)))%>%
#   mutate(act_usl= ifelse(MAT_BEZ == "R_F_M", 16, ifelse(MAT_BEZ == "R_D_M", 16, 24)))%>%
#   mutate(idepth_lsl= ifelse(MAT_BEZ == "R_F_M", 4, ifelse(MAT_BEZ == "R_D_M", 4, 4)))%>%
#   mutate(idepth_usl= ifelse(MAT_BEZ == "R_F_M", 8, ifelse(MAT_BEZ == "R_D_M", 8, 8)))|>
#   na.omit()
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # Test plotter
# plotdataset <<- mt_long %>%filter (MAT_BEZ == "R_D_J")|> filter(EDO == "mitime")
# 
# gg_plot1 <- ggplot(plotdataset,
#                    aes(x = CHARGE,color = MAT_BEZ))+
#   geom_point(aes( y = Results)) +
#   #geom_point(aes( y = maxitime)) +
#   #geom_point(aes( y = minitime)) +
#   #geom_point(aes( y = sitime)) +
#   geom_point(aes( y = skitime),color="blue") +
#   geom_point(aes( y = kitime-3),color="green") +
#   #geom_point(aes( y = ((maxact-Results)-(Results-minact))/(maxact-minact)*100)) +
#   xlab("Batches") +
#   ylab("itime") +
#   #geom_hline(yintercept = ifelse(plotdataset$mat_bez=="JUPITER",2,1.14), color = "red", linetype = "dashed", alpha = 0.5) +
#   #ylim(c(ifelse(plotdataset$mat_bez=="JUPITER",1.5,0.5), ifelse(plotdataset$mat_bez=="JUPITER",2.5,1.5))) +
#   #geom_hline(yintercept = ifelse(plotdataset$mat_bez=="JUPITER",2,1.14), color = "red", linetype = "dashed", alpha = 0.5) +
#   theme_gray() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="None")
# gg_plot1
# 
# ggplotly(gg_plot1)
# 
# 
# 
# 
# 
# 
# 
# R_test<-rnorm(1000,0,1)
# skewness(R_test)
# 
# 
# 
# 

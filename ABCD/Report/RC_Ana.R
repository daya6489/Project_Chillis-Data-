library(data.table)
library(zoo)
library(ggplot2)
library(lubridate)
## Read new data
new_data <- readRDS('H:/THINKSTAT/Vibhu Project/Project/Chillis Data and Analysis/Root Cause analysis/daya.rds')
names(new_data)[1] <- "BRANCH_NAME"
new_data = data.frame(new_data)

## Change all the factors into characters
fact_var = names(new_data)[sapply(new_data,is.factor)]
new_data[,fact_var] <- sapply(new_data[,fact_var],as.character) 

## Read Oct, Nov, Dec 201 data
setDT(new_data)
Oct_17 <- fread("H:/THINKSTAT/Vibhu Project/Project/Chillis Data and Analysis/Root Cause analysis/OneDrive-2018-03-02/October/SKU DATA__NEW_OCT-17.csv")
Nov_17 <- fread("H:/THINKSTAT/Vibhu Project/Project/Chillis Data and Analysis/Root Cause analysis/OneDrive-2018-03-02/November/SKU DATA_NOV-17.csv")
Dec_17 <- fread("H:/THINKSTAT/Vibhu Project/Project/Chillis Data and Analysis/Root Cause analysis/OneDrive-2018-03-02/December/SKU DATA_DEC-17.csv")

Oct_17[,BILL_DATE:= as.POSIXct(BILL_DATE,format='%d-%b-%y')]
Nov_17[,BILL_DATE:= as.POSIXct(BILL_DATE,format='%d-%b-%y')]
Dec_17[,BILL_DATE:= as.POSIXct(BILL_DATE,format='%d-%b-%y')]
new_data[,BILL_DATE:=as.POSIXct(BILL_DATE)]

## Append data
match(sapply(new_data,class),sapply(Nov_17,class))
new_data <- rbind(new_data,Oct_17,Nov_17,Dec_17)

### adding Month and year variable
new_data[,Year_mon :=as.yearmon(BILL_DATE,format="%b %Y")]
new_data[,Naet_amnt :=as.numeric(paste0(NET))]

table(is.na(new_data$Naet_amnt))
View(new_data[is.na(Naet_amnt),])

Chillis_dec15_dec17<- readRDS("Chillis_dec15_dec17.rds")

## Remove 2015 data
new_data <- Chillis_dec15_dec17[year(Year_mon)>=2016,]
new_data[,wday :=wday(BILL_DATE,label = T,abbr = F)]
new_data[,Quarter :=quarter(BILL_DATE,with_year = T)]

## Overall level analysis
Sum_O <- new_data[,.(Orders = length(unique(BILL_NO)),
                     Revenue = sum(ITEM_AMOUNT,na.rm = T),
                    AOV = mean(ITEM_AMOUNT,na.rm = T)), .(Year=year(Year_mon))]

Sum_overall <- new_data[,.(Orders = length(unique(BILL_NO)),
                           Revenue = sum(ITEM_AMOUNT,na.rm = T),
                           AOV = mean(ITEM_AMOUNT,na.rm = T)), .(Year_mon)]

Sum_overall[,nDate:= as.Date(Year_mon)]

ggplot(Sum_overall,aes(x=nDate,y=Revenue))+geom_line()
ggplot(Sum_overall,aes(x=nDate,y=AOV))+geom_line()
ggplot(Sum_overall,aes(x=nDate,y=Orders))+geom_line()

## Summary by Weekly
Sum_overall_wday <- new_data[,.(Orders = length(unique(BILL_NO)),
                           Revenue = sum(ITEM_AMOUNT,na.rm = T),
                           AOV = mean(ITEM_AMOUNT,na.rm = T)), .(Year_mon,wday)]

Sum_overall_wday[,nDate:= as.Date(Year_mon)]

ggplot(Sum_overall_wday,aes(x=nDate,y=Revenue,color=wday))+geom_line()
ggplot(Sum_overall_wday,aes(x=nDate,y=AOV,color=as.factor(wday)))+geom_line()
ggplot(Sum_overall_wday,aes(x=nDate,y=Orders,color=as.factor(wday)))+geom_line()

ggplot(Sum_overall_wday[Year_mon %in% c('Dec 2016','Dec 2017'),],aes(x=Year_mon,y=Orders))+
  geom_col(aes(fill = as.factor(wday)), position = "fill")

## Summary by Quarter
Sum_overall_Qrt <- new_data[,.(Orders = length(unique(BILL_NO)),
                                Revenue = sum(ITEM_AMOUNT,na.rm = T),
                                AOV = mean(ITEM_AMOUNT,na.rm = T)), .(Quarter)]

Sum_overall_wday[,nDate:= as.Date(Year_mon)]

## Brnach level analysis
B_Sum_O <- new_data[,.(Num_Bills = length(unique(BILL_NO)),
                     Tot_Net_rev = sum(ITEM_AMOUNT,na.rm = T)), .(Year=year(Year_mon),BRANCH_NAME)]

B_Sum_overall <- new_data[,.(Num_Bills = length(unique(BILL_NO)),
                           Tot_Net_rev = sum(ITEM_AMOUNT,na.rm = T),
                           avg_Net_rev = mean(ITEM_AMOUNT,na.rm = T)), .(Year_mon,BRANCH_NAME)]

B_Sum_overall[,nDate:= as.Date(Year_mon)]

ggplot(B_Sum_overall,aes(x=nDate,y=Tot_Net_rev,color=BRANCH_NAME))+geom_line()
ggplot(B_Sum_overall,aes(x=nDate,y=avg_Net_rev,color=BRANCH_NAME))+geom_line()

ggplot(B_Sum_overall,aes(x=nDate,y=Num_Bills,color=BRANCH_NAME))+geom_line()

ggplot(Sum_overall,aes(x=nDate,y=avg_Net_rev))+geom_line()

saveRDS(new_data,"Chillis_dec15_dec17.rds")
getwd()

brnch =unique(Chillis_dec15_dec17$BRANCH_NAME)


for (j in brnch){
  data_1 = subset(Chillis_dec15_dec17,BRANCH_NAME==j)
filanme =paste0(gsub(" ","_",j),".rds")
    saveRDS(data_1,filanme)
    rm(data_1)
}



# Order ITEm --------------------------------------------------------------
# OrderItem = readRDS("OrderItem.rds")

## Order ITem new data period Sep - 2017 to Dec - 2017
sh1 <- fread("b1.csv")
sh2 <- fread("b2.csv")

setDT(final_Data)
setDT(OrderItem)

OrderItem <- rbind(sh1,sh2)
saveRDS(OrderItem,"New_OrderItem.rds")

OrderItem[,BILL_DATE:=as.character(BILL_DATE)]
OrderItem[,BILL_DATE:=as.POSIXct(BILL_DATE,format='%d-%b-%y')]
OrderItem[,YR_mnth:=as.yearmon(paste0(BILL_DATE))]

New_OrderItem[,BILL_DATE:=as.character(BILL_DATE)]
New_OrderItem[,BILL_DATE:=as.POSIXct(BILL_DATE,format='%d-%b-%y')]
New_OrderItem[,YR_mnth:=as.yearmon(paste0(BILL_DATE))]
New_OrderItem[,NET:=as.numeric(paste0(NET))]
New_OrderItem[,NET:=ifelse(is.na(NET),NET_WITHOUT_TAXES,NET)]


table(OrderItem$YR_mnth)
## Keep data in OrderItem 2017
OrderItem <- OrderItem [YR_mnth < "Jan 2018",]


## Order Item From Jul 2016 to Jul 2017
load("Chili_data.Rda")
## Keep data in final_data from Aug 2017 onwards
setDT(final_Data)
fname = names(final_Data)

fname <- stringr::str_replace_all(fname," ","_")
names(final_Data)<-fname

final_Data[,YR_mnth:=as.yearmon(paste0(BILL_DATE))]
final_Data <- final_Data [YR_mnth>= "Jan 2016" & YR_mnth <= "Jul 2017",]
setnames(final_Data,"NET_AMT","NET")

## Join OrderItem and Final data
oname <- names(OrderItem)
fname = names(final_Data)
com_name <- intersect(fname,oname)

Item_lev_dat = rbind(final_Data[,com_name,with=F],OrderItem[,com_name,with=F])
table(Item_lev_dat$YR_mnth)

## Load orderitem_Aug-Dec_17.rds
final_Data
OrderItem1 = readRDS("orderitem_Aug-Dec_17.rds")
setDT(OrderItem1)

OrderItem1[,BILL_DATE:= as.character(paste0(BILL_DATE))]
OrderItem1[,ITEM_NAME:= as.character(paste0(ITEM_NAME))]
OrderItem1[,BRANCH_NAME:= as.character(paste0(ï..BRANCH_NAME))]
OrderItem1[,BILL_NO:= as.character(paste0(BILL_NO))]

OrderItem1[,BILL_DATE:=as.POSIXct(BILL_DATE,format='%d-%b-%y')]
OrderItem1[,YR_mnth:=as.yearmon(paste0(BILL_DATE))]
OrderItem1 <- OrderItem1[YR_mnth=="Aug 2017",]

item_nam <- names(Item_lev_dat)
oname1 = names(OrderItem1)
com_name1 <- intersect(item_nam,oname1)

com_name1 <- c(com_name1,"CATEGORY","ITEM_AMOUNT","NET")

OrderItem1[,c("CATEGORY","ITEM_AMOUNT","NET")] <-NA

Item_lev_dat <- rbind(Item_lev_dat,OrderItem1[,com_name1,with=F])
Item_lev_dat[,wday :=wday(BILL_DATE,label = T,abbr = F)]
Item_lev_dat[,Quarter :=quarter(BILL_DATE,with_year = T)]


Item_lev_dat[,YR_mnth:= as.yearmon(paste0(BILL_DATE))]

# ## Read ITEM Menu list
menu_lst = fread("menu.csv")
menu_lst[,ITEM_NAME:=trimws(item_name)]

# menuLst = Item_lev_dat[,.(nr=.N),.(item_name=ITEM_NAME)]
# 
# write.csv(menuLst,"menuLst.csv")

# setkey(menuLst,item_name)
# setkey(menu_lst,item_name)
# 
# match(menu_lst$item_name,menuLst$item_name)
# 
# menuLst1 = menuLst[menu_lst]

saveRDS(Item_lev_dat,"Item_lev_dat_jul16_dec17.rds")

## Aggregate OrderITEm by ITEM and BILL Level
OrderItem_Bill <- Item_lev_dat[,.(NET =max(NET,na.rm = T),
                               TOTAL_QUANTITY = max(TOTAL_QUANTITY),
                               ITEM_AMOUNT  = sum(ITEM_AMOUNT,na.rm = T),
                               ITEM_AMOUNT_max  = max(ITEM_AMOUNT,na.rm = T),
                               # New_ITEM_AMOUNT = (ITEM_AMOUNT/TOTAL_QUANTITY),
                               TOTAL_QUANTITY_n = .N),.(BRANCH_NAME,BILL_DATE,BILL_NO,CATEGORY,ITEM_NAME,YR_mnth,Quarter)]

save(OrderItem_Bill,file="Order_cleaned_dat.RData")

## ADd Menu list
menu_lst[,item_name:=NULL]
menu_lst[,category_1:=NULL]
menu_lst<-menu_lst[!duplicated(ITEM_NAME),]

setkey(menu_lst,ITEM_NAME)
setkey(OrderItem_Bill,ITEM_NAME)

OrderItem_Bill <- merge(OrderItem_Bill,menu_lst,all.x=T)



rm(final_Data,OrderItem,OrderItem1)

ITem_Amount = OrderItem_Bill[,.(Nr=.N,
                              Tot_Item_amnt =sum(ITEM_AMOUNT,na.rm=T),
                              Item_amnt =max(New_ITEM_AMOUNT,na.rm=T),
                            qmax=max(`TOTAL_QUANTITY`,na.rm = T),
                            qmin=min(`TOTAL_QUANTITY`,na.rm = T),
                            max=max(`ITEM_AMOUNT`,na.rm = T),
                           min=min(`ITEM_AMOUNT`,na.rm = T),
                           mean=mean(`ITEM_AMOUNT`,na.rm = T)),.(`ITEM_NAME`,CATEGORY)]

## Aggregated by ITEM level

### Monthly number of bills and Item purcahsed
Item_purch_mon <- OrderItem_Bill[,.(Num_bill=length(unique(BILL_NO)),
                                   Num_ite=length(unique(ITEM_NAME))),.(YR_mnth)]

Item_purch_wk <- OrderItem_Bill[,.(Num_bill=length(unique(BILL_NO)),
                                  Num_ite=length(unique(ITEM_NAME))),.(wday)]

Item_purch_qt <- OrderItem_Bill[,.(Num_bill=length(unique(BILL_NO)),
                                  Num_ite=length(unique(ITEM_NAME))),.(Quarter)]



## aggregated by Bill level
Bill_no_agg <- Item_lev_dat[,.(Qnty=.N,
                               mn_qnty=min(TOTAL_QUANTITY,na.rm = T),
                               mx_qnty=max(TOTAL_QUANTITY,na.rm = T),
                               Item_cnt=length(unique(ITEM_NAME))),
                              .(BRANCH_NAME,BILL_DATE,BILL_NO,YR_mnth,NET,wday,Quarter)] #2889914       9

Bil_cnt_mn <- Bill_no_agg[,.(Tot_ite=sum(Item_cnt),
                             avg_itm=mean(Item_cnt)),.(YR_mnth)]

Bil_cnt_qt <- Bill_no_agg[,.(Tot_ite=sum(Item_cnt),
                             avg_itm=mean(Item_cnt)),.(Quarter)]

setkey(Bil_cnt_qt,Quarter)
setkey(Item_purch_qt,Quarter)

## Quarterly report - Overall
Quart_report <- Bil_cnt_qt[Item_purch_qt]

## Quarterly report - Powai

Item_purch_qt1 <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI",.(Num_bill=length(unique(BILL_NO)),
                                   Num_ite=length(unique(ITEM_NAME))),.(Quarter)]
Bil_cnt_qt1 <- Bill_no_agg[BRANCH_NAME=="CHILIS POWAI",.(Tot_ite=sum(Item_cnt),
                             avg_itm=mean(Item_cnt)),.(Quarter)]
setkey(Bil_cnt_qt1,Quarter)
setkey(Item_purch_qt1,Quarter)
Quart_report_b <- Bil_cnt_qt1[Item_purch_qt1]

## Monthly report - Overall
setkey(Bil_cnt_mn,YR_mnth)
setkey(Item_purch_mon,YR_mnth)

Mont_report <- Bil_cnt_mn[Item_purch_mon]
Mont_report[,YR_mnth:=as.character(paste0(YR_mnth))]
ggplot(Mont_report, aes(y=avg_itm, x=YR_mnth)) +
  geom_bar( stat="identity", position="dodge")+xlab("")

## Monthly report - Powai
Item_purch_mon1 <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI",.(Num_bill=length(unique(BILL_NO)),
                                    Num_ite=length(unique(ITEM_NAME))),.(YR_mnth)]

Bil_cnt_mn1 <- Bill_no_agg[BRANCH_NAME=="CHILIS POWAI",.(Tot_ite=sum(Item_cnt),
                             avg_itm=mean(Item_cnt)),.(YR_mnth)]

setkey(Bil_cnt_mn1,YR_mnth)
setkey(Item_purch_mon1,YR_mnth)

Mont_report_b <- Bil_cnt_mn1[Item_purch_mon1]

### Dec Month Comparision


Dec16_17_dat <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI" & (YR_mnth=="Dec 2016" | YR_mnth=="Dec 2017"),.(sum_qnty=sum(TOTAL_QUANTITY,na.rm = T),
                                 ITEM_AMOUNT = sum(ITEM_AMOUNT),
                                 ITEM_AMOUNT_max =sum(ITEM_AMOUNT_max),
                                 Num_bill =length(BILL_NO)),
                              .(ITEM_NAME,CATEGORY,category_2,category_3,YR_mnth)] 

Item_report <- dcast(Dec16_17_dat, CATEGORY+ITEM_NAME+category_2+category_3 ~ YR_mnth, fun=sum, value.var=c("sum_qnty","ITEM_AMOUNT","Num_bill"))

Item_report[,GP:= ifelse(`Num_bill_Dec 2016`==0 & `Num_bill_Dec 2017`>0,"Only in 2017",
                         ifelse(`Num_bill_Dec 2016`>0 & `Num_bill_Dec 2017`==0,"Only in 2016","Both"))]

Item_2016_dec <- Item_report[GP=="Only in 2016",]$ITEM_NAME
Item_2017_dec <- Item_report[GP=="Only in 2017",]$ITEM_NAME
Item_both_dec <- Item_report[GP=="Both",]$ITEM_NAME

write.csv(Item_report,"Item_report_Dec_16_17_Powai1.csv")


## Category Level

Cat1_report <- dcast(Item_report, CATEGORY ~ GP, fun=length,value.var = "GP")
Cat1_report[,Grand_tot:= apply(Cat1_report[,-1],1,sum)]

Cat2_report <- dcast(Item_report, category_2+category_3 ~ GP, fun=length,value.var = "GP")
Cat2_report[,Grand_tot:= apply(Cat2_report[,c(-1,-2)],1,sum)]


## Category 2 level

Dec16_17_dat2 <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI" & (YR_mnth=="Dec 2016" | YR_mnth=="Dec 2017"),.(sum_qnty=sum(TOTAL_QUANTITY,na.rm = T),
                                                                                                           ITEM_AMOUNT = sum(ITEM_AMOUNT),
                                                                                                           ITEM_AMOUNT_max =sum(ITEM_AMOUNT_max),
                                                                                                           Num_bill =length(BILL_NO)),
                               .(category_2,category_3,YR_mnth)] 

Item_report2 <- dcast(Dec16_17_dat, category_2+category_3 ~ YR_mnth, fun=sum, value.var=c("sum_qnty","ITEM_AMOUNT","Num_bill"))

Item_report2[,GP:= ifelse(`Num_bill_Dec 2016`==0 & `Num_bill_Dec 2017`>0,"Only in 2017",
                         ifelse(`Num_bill_Dec 2016`>0 & `Num_bill_Dec 2017`==0,"Only in 2016","Both"))]
write.csv(Item_report2,"Item_report_Dec_16_17_Powai_cat2.csv")



### Tripple Dipper
OrderItem_TD <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI" & ITEM_NAME=="TRIPLE DIPPER" & (YR_mnth=="Dec 2016" | YR_mnth=="Dec 2017"),]
OrderItem_TD1 <- Item_lev_dat[BRANCH_NAME=="CHILIS POWAI" & ITEM_NAME=="TRIPLE DIPPER" & (YR_mnth=="Dec 2016" | YR_mnth=="Dec 2017"),]

length(unique(OrderItem_Bill$BILL_NO))

tt=OrderItem_TD[YR_mnth=="Dec 2016" | YR_mnth=="Dec 2017" | YR_mnth=="Sep 2017",
                  .(bil_cnt=length(unique(BILL_NO)),
                    Amnt_avg =mean(New_ITEM_AMOUNT),
                    Amnt_sum =sum(New_ITEM_AMOUNT),
                    Amnt_max =max(New_ITEM_AMOUNT)),
               .(BRANCH_NAME,YR_mnth)]
tt=OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI" & (YR_mnth=="Dec 2016" | YR_mnth=="Dec 2017"),.(bil_cnt=length(unique(BILL_NO)),
                                                                                              uni_item = length(unique(ITEM_NAME)),
                                                                                              trip_dip = length(unique(BILL_NO[ITEM_NAME=="TRIPLE DIPPER"])),
                                                                                              Food_cat = length(unique(BILL_NO[CATEGORY=="FOODS"])),
                                                                                              Liq_cat = length(unique(BILL_NO[CATEGORY=="LIQUOR"])),
                                                                                              Nab_cat = length(unique(BILL_NO[CATEGORY=="NA BEVERAGES"])),
                                                                                              Amnt_sum17 =mean(New_ITEM_AMOUNT[ITEM_NAME %in% Item_2017_dec]),
                                                                                              Amnt_sum16 =mean(New_ITEM_AMOUNT[ITEM_NAME %in% Item_2016_dec]),
                                                                                              Amnt_both =mean(New_ITEM_AMOUNT[ITEM_NAME %in% Item_both_dec])),
                  .(BRANCH_NAME,YR_mnth)]


## Dec 2107 file for Powai

CHILIS_POWAI_dec17 <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI" & YR_mnth=="Dec 2017",] 
CHILIS_POWAI_dec16 <- OrderItem_Bill[BRANCH_NAME=="CHILIS POWAI" & YR_mnth=="Dec 2016",] 


tt <- CHILIS_POWAI_dec17[,.(sum_qnty=sum(TOTAL_QUANTITY,na.rm = T),
                                 ITEM_AMOUNT = sum(ITEM_AMOUNT),
                                ITEM_AMOUNT_max =sum(ITEM_AMOUNT_max),
                                 Num_bill =length(BILL_NO)),
                               .(ITEM_NAME,CATEGORY)] 


sum(CHILIS_POWAI_dec16$TOTAL_QUANTITY)

length(unique(CHILIS_POWAI_dec16$ITEM_NAME))
length(unique(CHILIS_POWAI_dec17$ITEM_NAME))


CHILIS_POWAI_dec16_Item <- CHILIS_POWAI_dec16[,.(Qnty=.N),
                               .(ITEM_NAME,CATEGORY)] 



### AOV value amount per bill
Item_lev_dat [,NET:= as.numeric(paste0(NET))]


AOV_value <- Item_lev_dat[BRANCH_NAME=="CHILIS POWAI",.(Net_amnt=max(NET)),.(BILL_NO,YR_mnth)]


Dec_2017 <- Item_lev_dat[BRANCH_NAME=="CHILIS POWAI" & YR_mnth == "Dec 2017",]



AOV_value_sum <- AOV_value[,.(Mean_AOV=mean(Net_amnt,na.rm = T),
                              Median_AOV=median(Net_amnt,na.rm = T),
                              Total =sum(Net_amnt,na.rm = T),
                              Num_bill=.N),.(YR_mnth)]


names(Item_lev_dat)

tt = New_OrderItem[BRANCH_NAME=="CHILIS POWAI",]
tt1 =tt[,.(NET=max(NET,na.rm=T)),.(BILL_NO,Year = year(BILL_DATE),month=month(BILL_DATE))]

tt <- tt[,.(AOV=mean(NET,na.rm = T),
                              Total =sum(NET,na.rm = T),
                              Num_bill=length(unique(BILL_NO))),.(Year = year(BILL_DATE),month=month(BILL_DATE))]

tttt = Chillis_dec15_dec17[BRANCH_NAME=="CHILIS POWAI", .(Naet_amnt=mean(Naet_amnt,na.rm = T),
                                                          Bill_num =length(unique(BILL_NO)),
                                                          amnt =sum(Naet_amnt)),.(Year_mon)]



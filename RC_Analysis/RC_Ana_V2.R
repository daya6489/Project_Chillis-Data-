## Library
library(data.table)
library(stringr)
library(SmartEDA)
library(zoo)

## Input Data
Master_data <- readRDS("C:/Users/dubrangala/OneDrive - VMware, Inc/Project_PropModel/Materials/R Case study 2 retail/CEda/Latest_data/POS_Jul16_Feb18.rds")
nam=c("BILL_NO","BRANCH_NAME","BILL_DATE","TOTAL_QUANTITY","MOBILE_NO","PHONE_NO","DISCOUNT","SUBTOTAL","NET_AMT","PAX","ITEM_NAME","category_1","category_2","category_3","is_use")
names(Master_data) <- nam

setDT(Master_data)

Master_data[,category_3:=ifelse(category_3=="Non Veg","Non-Veg",ifelse(
  category_3=="veg","Veg",category_3))]

Master_data[,Year_mnth:= format(BILL_DATE,"%Y-%b")]
Master_data[ ,Year_mnth:= as.yearmon(Year_mnth,"%Y-%b")]

Master_data <- Master_data[BILL_DATE>="2016-07-01",]

Master_data <- Master_data[,BRANCH_NAME:= ifelse(BRANCH_NAME=="CHILIS MALAD","CHILIS INORBIT MALAD",BRANCH_NAME)]

saveRDS(Master_data,"Master_data_Final_Jul16_Feb18.rds")

## Transaction level data
Trans_data <- Master_data[,.(category_1_Food = sum(TOTAL_QUANTITY[category_1=="FOODS"]),
                              category_1_Liq = sum(TOTAL_QUANTITY[category_1=="LIQUOR"]),
                              category_1_Bev = sum(TOTAL_QUANTITY[category_1=="NA BEVERAGES"]),
                              category_1_Tob = sum(TOTAL_QUANTITY[category_1=="TOBACCO"]),
                              NET_AMT =max(NET_AMT,na.rm = T),
                              TOTAL_QUANTITY = sum(TOTAL_QUANTITY),
                              PAX = max(PAX,na.rm = T),
                              Num_Uniq_Item = length(unique(ITEM_NAME))),
                           .(BRANCH_NAME,BILL_DATE,BILL_NO,Year_mnth)]

### ITEM AMOUNT
# Master_data_July16_Feb18 <- readRDS("C:/Users/dubrangala/OneDrive - VMware, Inc/Project_PropModel/Materials/R Case study 2 retail/CEda/Master_data_July16_Feb18.rds")
# Master_data_July16_Feb18 <- Master_data_July16_Feb18[,c("BRANCH_NAME","BILL_NO","ITEM_NAME","ITEM_AMOUNT"),with=F]
# Master_data_July16_Feb18 <- Master_data_July16_Feb18[ITEM_AMOUNT>0,]
# 
# Master_data_July16_Feb18 <- Master_data_July16_Feb18[,.(ITEM_AMOUNT=max(ITEM_AMOUNT)),
#                                                        .(BRANCH_NAME,BILL_NO,ITEM_NAME)]
# 
# setkey(Master_data_July16_Feb18,BRANCH_NAME,BILL_NO,ITEM_NAME)
# setkey(Master_data,BRANCH_NAME,BILL_NO,ITEM_NAME)
# 
# Master_data1 <- Master_data_July16_Feb18[Master_data]
# 
# ITem_amount <- Master_data1[,.(ITEM_AOUNT = max(ITEM_AMOUNT,na.rm = T),
#                                ITEM_AOUNT_min = min(ITEM_AMOUNT,na.rm = T)),.(ITEM_NAME)]

# Trans_data1 =Trans_data[NET_AMT<2,]

## Analysis Overall
#AOV analysis
AOV_value_sum <- Trans_data[,.(Mean_AOV=mean(NET_AMT,na.rm = T),
                               Median_AOV=median(NET_AMT,na.rm = T),
                               Total =sum(NET_AMT,na.rm = T),
                               Num_Trans=length(unique(BILL_NO)),
                               Pax =mean(PAX,na.rm = T)),.(Year_mnth)]

## Branch level Overall
AOV_value_sum_branch <- Trans_data[,.(Mean_AOV=mean(NET_AMT,na.rm = T),
                                      Median_AOV=median(NET_AMT,na.rm = T),
                                      Total =sum(NET_AMT,na.rm = T),
                                      Num_Trans=length(unique(BILL_NO)),
                                      Pax =mean(PAX,na.rm = T)),.(BRANCH_NAME,Year_mnth)]


Uniq_Food_Drink <- Master_data[,.(Unique_item = length(unique(ITEM_NAME)),
                                   Qnty_ordered = sum(TOTAL_QUANTITY),
                                   Num_bills = length(unique(BILL_NO)),
                                   Unique_no_Food_item = length(unique(ITEM_NAME[category_1=="FOODS"])),
                                   Qnty_ordered_Food_item = sum(TOTAL_QUANTITY[category_1=="FOODS"]),
                                   Unique_no_Liqu_item = length(unique(ITEM_NAME[category_1=="LIQUOR"])),
                                   Qnty_ordered_Liqu_item = sum(TOTAL_QUANTITY[category_1=="LIQUOR"]),
                                   Unique_no_Toba_item = length(unique(ITEM_NAME[category_1=="TOBACCO"])),
                                   Qnty_ordered_Toba_item = sum(TOTAL_QUANTITY[category_1=="TOBACCO"]),
                                  Unique_no_NABe_item = length(unique(ITEM_NAME[category_1=="NA BEVERAGES"])),
                                   Qnty_ordered_NABe_item = sum(TOTAL_QUANTITY[category_1=="NA BEVERAGES"])),
                                .(Year_mnth)]

setkey(Uniq_Food_Drink,Year_mnth)
setkey(AOV_value_sum,Year_mnth)
Uniq_Food_Drink <- AOV_value_sum[Uniq_Food_Drink]
write.csv(Uniq_Food_Drink,"Uniq_Food_Drink.csv")

Uniq_Food_Drink_Branch<- Master_data[,.(Unique_item = length(unique(ITEM_NAME)),
                                        Qnty_ordered = sum(TOTAL_QUANTITY),
                                        Num_bills = length(unique(BILL_NO)),
                                        Unique_no_Food_item = length(unique(ITEM_NAME[category_1=="FOODS"])),
                                        Qnty_ordered_Food_item = sum(TOTAL_QUANTITY[category_1=="FOODS"]),
                                        Unique_no_Liqu_item = length(unique(ITEM_NAME[category_1=="LIQUOR"])),
                                        Qnty_ordered_Liqu_item = sum(TOTAL_QUANTITY[category_1=="LIQUOR"]),
                                        Unique_no_Toba_item = length(unique(ITEM_NAME[category_1=="TOBACCO"])),
                                        Qnty_ordered_Toba_item = sum(TOTAL_QUANTITY[category_1=="TOBACCO"]),
                                        Unique_no_NABe_item = length(unique(ITEM_NAME[category_1=="NA BEVERAGES"])),
                                        Qnty_ordered_NABe_item = sum(TOTAL_QUANTITY[category_1=="NA BEVERAGES"])),
                                     .(BRANCH_NAME,Year_mnth)]

setkey(Uniq_Food_Drink_Branch,BRANCH_NAME,Year_mnth)
setkey(AOV_value_sum_branch,BRANCH_NAME,Year_mnth)
Uniq_Food_Drink_Branch <- AOV_value_sum_branch[Uniq_Food_Drink_Branch]

write.csv(Uniq_Food_Drink_Branch,"Uniq_Food_Drink_Branch.csv")


# ITEM Level report -------------------------------------------------------
Dec16_17_dat <- Master_data[(Year_mnth=="Dec 2016" | Year_mnth=="Dec 2017"),.(sum_qnty=sum(TOTAL_QUANTITY,na.rm = T),
                                                                               # ITEM_AMOUNT = sum(ITEM_AMOUNT),
                                                                               Num_bill =length(unique(BILL_NO))),
                             .(BRANCH_NAME,ITEM_NAME,category_1,category_2,category_3,Year_mnth)] 

Item_report <- dcast(Dec16_17_dat, BRANCH_NAME+category_1+ITEM_NAME+category_2+category_3 ~ Year_mnth, fun=sum, value.var=c("sum_qnty","Num_bill"))

Item_report[,GP:= ifelse(`Num_bill_Dec 2016`==0 & `Num_bill_Dec 2017`>0,"Only in 2017",
                         ifelse(`Num_bill_Dec 2016`>0 & `Num_bill_Dec 2017`==0,"Only in 2016","Both"))]

Item_2016_dec <- Item_report[GP=="Only in 2016",]$ITEM_NAME
Item_2017_dec <- Item_report[GP=="Only in 2017",]$ITEM_NAME
Item_both_dec <- Item_report[GP=="Both",]$ITEM_NAME

write.csv(Item_report,"Item_report_Dec_16_17_All.csv")

# ITEM Level report -------------------------------------------------------
Dec16_17_dat1 <- Master_data[(Year_mnth=="Dec 2016" | Year_mnth=="Dec 2017"),.(sum_qnty=sum(TOTAL_QUANTITY,na.rm = T),
                                                                                # ITEM_AMOUNT = sum(ITEM_AMOUNT),
                                                                                Num_bill =length(unique(BILL_NO))),
                              .(BRANCH_NAME,category_2,category_3,Year_mnth)] 

Item_report1 <- dcast(Dec16_17_dat1, BRANCH_NAME+category_2+category_3 ~ Year_mnth, fun=sum, value.var=c("sum_qnty","Num_bill"))

Item_report1[,GP:= ifelse(`Num_bill_Dec 2016`==0 & `Num_bill_Dec 2017`>0,"Only in 2017",
                          ifelse(`Num_bill_Dec 2016`>0 & `Num_bill_Dec 2017`==0,"Only in 2016","Both"))]

write.csv(Item_report1,"Item_report_Dec_16_17_cat.csv")

## Overall by category_1
Cat1_report <- dcast(Item_report, BRANCH_NAME+category_1 ~ GP, fun=length,value.var = "GP")
Cat1_report[,Grand_tot:= apply(Cat1_report[,-1],1,sum)]

Cat2_report <- dcast(Item_report, category_1_2+category_1_3 ~ GP, fun=length,value.var = "GP")
Cat2_report[,Grand_tot:= apply(Cat2_report[,c(-1,-2)],1,sum)]


Item_level_report_overall <- Master_data[,.(sum_qnty=.N,
                                             Num_bill =length(unique(BILL_NO))),
                                          .(ITEM_NAME,category_1,category_2,category_3,Year_mnth)] 

Item_level_report_overall_1 <- dcast(Item_level_report_overall, ITEM_NAME+category_1+category_2+category_3 ~ Year_mnth, fun=sum, value.var=c("Num_bill"))


write.csv(Item_level_report_overall_1,"Item_level_report_overall.csv")

Item_level_report_store <- Master_data[,.(sum_qnty=.N,
                                            Num_bill =length(unique(BILL_NO))),
                                         .(BRANCH_NAME,ITEM_NAME,category_1,category_2,Year_mnth)] 

Item_level_report_store_1 <- dcast(Item_level_report_store, BRANCH_NAME+ITEM_NAME+category_1+category_2 ~ Year_mnth, fun=sum, value.var=c("Num_bill"))
write.csv(Item_level_report_store_1,"Item_level_report_store.csv")

### Order construct month on month
OC_MOM <- Master_data[,.(sum_qnty=sum(TOTAL_QUANTITY,na.rm = T),
                             # ITEM_AMOUNT = sum(ITEM_AMOUNT),
                             Num_bill =length(unique(BILL_NO))),
                             .(BRANCH_NAME,category_2,category_3,Year_mnth)] 

OC_MOM1 <- dcast(OC_MOM, BRANCH_NAME+category_2 ~ Year_mnth, fun=sum, value.var=c("sum_qnty","Num_bill"))

# OC_MOM1[,GP:= ifelse(`Num_bill_Dec 2016`==0 & `Num_bill_Dec 2017`>0,"Only in 2017",
#          ifelse(`Num_bill_Dec 2016`>0 & `Num_bill_Dec 2017`==0,"Only in 2016","Both"))]

write.csv(OC_MOM1,"Order_construct_MOM.csv")

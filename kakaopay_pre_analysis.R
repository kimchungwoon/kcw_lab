##################################사전과제##########################################
install.packages("sqldf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("reshape2")
install.packages("corrplot")
install.packages("pheatmap")
install.packages("randomForest")
install.packages("Ranger")
install.packages("vip")
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('cluster')
install.packages('factoextra')

library(factoextra)
library(cluster)
library(rpart.plot)
library(caret)
library(rpart)
library(vip)
library(randomForest)
library(pheatmap)
library(corrplot)
library(dplyr)
library(sqldf)
library(ggplot2)
library(lubridate)
library(reshape2)
## 데이터 EDA pre ##
## 데이터 불러 들이기##

a_payment_trx<-read.csv("C:/Users/yello/Desktop/카카오사전과제/a_payment_trx.csv", sep = ",")
dutchpay_claim<-read.csv("C:/Users/yello/Desktop/카카오사전과제/dutchpay_claim.csv", sep = ",")
dutchpay_claim_detail<-read.csv("C:/Users/yello/Desktop/카카오사전과제/dutchpay_claim_detail.csv", sep = ",")
users<-read.csv("C:/Users/yello/Desktop/카카오사전과제/users.csv", sep = ",")

########################### 1. 더치페이 요청에 대한 응답률이 높을 수록 더치페이 서비스를 더 많이 사용한다 ###########################

## 가설 검정 시 필요사항 : 응답에 대한 정의 및 사용에 대한 정의를 해야함
## 응답1) : dutchpay_claim_detail 에 recv_user_id를 기준으로 status 가 check or send가 되었을 때 응답이라고 정의함
## 응답2) : dutchpay_claim_detail의 status 중 send 건수를 응답이라고 정의함
## 사용 : dutchpay_claim 에 claim_user_id를 기준으로 요청 claim_id 건수를 사용이라고 정의함



##사용 테이블 : dutchpay_claim, dutchpay_claim_detail, users

head(users)
head(dutchpay_claim)
head(dutchpay_claim_detail)


### 1_가. users data 확인
summary(users) # 평균 나이 29.46 

users$gender_cd<-as.factor(users$gender_cd) # 성별 변수 numeric->string으로 변환

users%>%summarise(n=n(), n_user = n_distinct(user_id)) # user_id 중복 없음

table(users%>%select(foreigner_yn,os_type)) # os type 비율 : 66% : 34% / 외국인 비율 거의 없음
round(prop.table(table(users%>%select(foreigner_yn,os_type))),2)

table(users$gender_cd)
round(prop.table(table(users$gender_cd)),2) # 유저 성별 비율 (1) 51% : (2) 49% 

table(users%>%select(gender_cd, os_type))
round(prop.table(table(users%>%select(gender_cd, os_type))),2) # 성별에 따라 os_type 비율이 다름 / (1)의 경우 a:b = 59% : 41% , (2)의 경우 a:b = 73% : 27%
#추후 성별/os 타입별 더치페이 빈도에 대한 분포 확인이 필요함




### 1_나. dutchpay_claim data 확인
summary(dutchpay_claim)

dutchpay_claim%>%summarise(n_claim=n_distinct(claim_id)
                           ,n_claim_user=n_distinct(claim_user_id)
                           ,min_at = min(claim_at)
                           ,max_at = max(claim_at)) #더치페이 요청 수 159194, 요청 유저 수 : 53558, 기간 19.12 ~ 20.2 3개월 데이터 



dutchpay_claim2 <- dutchpay_claim%>%mutate(yy = year(claim_at) 
                                           ,mm = month(claim_at)
                                           ,dd =day(claim_at)
                                           ,yymmdd=make_date(yy,mm,dd)
                                           ,yyyymm = substr(make_date(yy,mm),1,7)
                                           ,week = wday(claim_at)) # claim_at 년월일 변환 및 요일 추가

dutchpay_claim2%>%group_by(yyyymm)%>%
  summarise(n_users=n_distinct(claim_user_id)
            ,n_claim = n_distinct(claim_id)) #월별 더치페이 요청자 및 요청건수(20년도 설날 : 1월 24일 ~ 1월 27일)

#ggplot(data=dutchpay_claim2%>%group_by(yyyymm)%>%summarise(n_users=n_distinct(claim_user_id),n_claim = n_distinct(claim_id)) , aes(x= yyyymm,y=n_users))+geom_bar(stat='identity')

dutchpay_claim2%>%group_by(week)%>%
  summarise(n_users=n_distinct(claim_user_id)
            ,n_claim = n_distinct(claim_id)) # 1~7 : 월 ~ 일 -> 전체 건수의 경우 월화의 빈도가 낮고 나머지 요일의 빈도가 높음  

week_user<-dutchpay_claim2%>%group_by(yyyymm,week)%>%
             summarise(n_users=n_distinct(claim_user_id)
                       ,n_claim = n_distinct(claim_id))%>%data.frame() # 월별로 나누어 보았을 때 월별 요일에 대한 차이가 있어보임 추후 확인 필요


dut_startmon<-dutchpay_claim2%>%group_by(claim_user_id)%>%mutate(start_mon = min(yyyymm))
dut_startmon%>%group_by(start_mon)%>%summarise(new_user = n_distinct(claim_user_id)) #신규유입 
dut_user_master<-left_join(dut_startmon,users%>%rename(claim_user_id=user_id), by= 'claim_user_id')
dut_user_master<-dut_user_master%>%mutate(age_cate = ifelse(age < 10, 0,
                                                            ifelse(age<20,1,
                                                                   ifelse(age<30,2,
                                                                          ifelse(age<40,3,
                                                                                 ifelse(age<50,4,
                                                                                        ifelse(age<60,5,
                                                                                               ifelse(age<70,6,7))))))))

dut_user_master%>%group_by(start_mon)%>%summarise(cnt_1 = n_distinct(claim_user_id[which(gender_cd=='1')])
                                  ,cnt_2 = n_distinct(claim_user_id[which(gender_cd=='2')]))

dut_user_master%>%group_by(age_cate)%>%summarise(n=n_distinct(claim_user_id))
dut_user_master%>%group_by(age_cate,gender_cd)%>%summarise(n=n_distinct(claim_user_id))

dut_user_master%>%group_by(yyyymm,age_cate)%>%summarise(n=n_distinct(claim_user_id))
#z<-dut_user_master%>%group_by(yyyymm,age_cate)%>%summarise(n=n_distinct(claim_user_id))
View(z)
dut_user_master%>%group_by(start_mon,age_cate)%>%summarise(n=n_distinct(claim_user_id))





dutchpay_claim2%>%group_by(claim_user_id,yyyymm)%>%
  summarise(n_clm=n_distinct(claim_id))%>%
  group_by(yyyymm)%>%summarise(mean_clm_cnt = mean(n_clm)) # 평균적으로 월별 1인당 사용 건수는 2회 정도로 비슷함


dutchpay_claim_per_cnt <-data.frame(dutchpay_claim2 %>% group_by(claim_user_id)%>%summarise(n_clm = n_distinct(claim_id)))
table(dutchpay_claim_per_cnt$n_clm)
dutchpay_claim_per_cnt %>% summarise(sum_clm_over30 = sum(n_clm[which(n_clm >30)])
                                     ,sum_clm_under30 = sum(n_clm[which(n_clm <=30)])
                                     ,sum_user_cnt_over30 = n_distinct(claim_user_id[which(n_clm>=30)])
                                     ,sum_user_cnt_under30 = n_distinct(claim_user_id[which(n_clm<30)])
) # 3개월간 더치페이 사용 빈도 30회 이상인 고객 수 총 53379명 중 179명 0.3% 수준으로 빈도수 30회 이상 고객을 하나의 그룹으로 변경

dutchpay_claim_per_cnt2<- dutchpay_claim_per_cnt%>%mutate(n_clm = ifelse(n_clm < 30, n_clm, 30)) #30회 이상의 고객을 30으로 변경

ggplot(data = dutchpay_claim_per_cnt2, aes(x=(n_clm))) +
  geom_histogram(aes(y=..count..),position = 'identity') +
  stat_bin(aes(y=..count.., label = round(..density..,3)*100) ,geom ='text',vjust=-.5) 
# 3개월 간 1~6회 사용자의 합이 전체의 90%(1회 : 50.1%, 2회 : 18.2%, 3회 : 9.5%) 이상으로 나타남


## 1_다. dutchpay_claim_detail data 확인 -> 응답률 정의필요


head(dutchpay_claim_detail)
head(users)

users_recv<- rename(users, recv_user_id = user_id) # recv 유저에 대한 고객정보 추가를 위한 컬럼명 변경

dutch_recv_detail<-left_join(dutchpay_claim_detail,users_recv, by = 'recv_user_id') #고객 정보 추가

# 요청을 받고 수락 등의 긍정 action을 취하지 않으면 유저 정보가 없다??

dutch_recv_detail%>%filter(status =='CLAIM')%>%group_by(gender_cd)%>%summarise(n=n())



dutch_master<-left_join(dutchpay_claim, dutchpay_claim_detail, by = 'claim_id')
dutch_master %>%filter(status =='CHECK')

dutch_master%>%summarise(n=n_distinct(claim_id))
b<-dutch_master%>%group_by(claim_id)%>%filter(!(status %in% ('CHECK')))%>%data.frame()
bb<-b%>%group_by(claim_id)%>%summarise(n=n())%>%filter(n>=2)

bbb<- inner_join(bb,b,by = 'claim_id')
View(bbb)

# claim table과 detail table에 claim_id로 join 시 check가 없을 수 있음 -> 본인 제외한 요청?










##### 더치페이 데이터에 대한 응답률 정의 #####

summary(dutchpay_claim_detail) ## send_amount Na값 존재
dutchpay_claim_detail$send_amount[which(is.na(dutchpay_claim_detail$send_amount))]<-0 # send_amount Na값 0으로 변환
summary(dutchpay_claim_detail) ## na값 변환 완료


dutchpay_recv_cnt<-dutchpay_claim_detail%>%group_by(recv_user_id,status)%>%summarise(status_cnt = n())%>%data.frame() # recv_user_id 기준으로 status별 건수 확인 
dutchpay_recv_cnt2<-dcast(dutchpay_recv_cnt, recv_user_id ~ status,value.var = "status_cnt") #레이블 인코딩에서 피벗형태로 변경
dutchpay_recv_cnt2[is.na(dutchpay_recv_cnt2)]<-0 # 피벗 형태로 변경 시 Na값 발생 ->0으로 대체체
dutchpay_recv_cnt3<-dutchpay_recv_cnt2%>%mutate(tot_n = CHECK+CLAIM+SEND
                                                ,CHECK_SEND = CHECK+SEND
                                                ,prob_recv1 = round((CHECK+SEND)/tot_n,2)
                                                ,prob_recv2 = round(SEND/tot_n,2)) # 1: check + send / 2 : send


dutchpay_recv_cnt3 ## 더치페이 응답에 대한 건수 테이블


dutchpay_claim_cnt <- dutchpay_claim2%>%select(-yy,-mm,-dd,-claim_at) #더치페이 사용 테이블 구성

dutchpay_claim_cnt2 <-dutchpay_claim_cnt%>%group_by(claim_user_id)%>%summarise(claim_cnt = n_distinct(claim_id))%>%data.frame() #월별 고려 x
dutchpay_claim_cnt2 # 더치페이 사용에 대한 건수(월별 고려 x)




dutch_RC <- left_join(dutchpay_recv_cnt3,dutchpay_claim_cnt2%>%rename(recv_user_id =claim_user_id), by = 'recv_user_id') #recv에 대한 claim 건수 조인
dutch_RC$claim_cnt[is.na(dutch_RC$claim_cnt)]<-0 # claim 건수 Na 값 ->0 치환

ggplot(data = dutch_RC, aes(x = CHECK_SEND, y =claim_cnt )) + geom_point()+geom_smooth(method=lm)+labs(x="CHECK+SEND 건수", y="더치페이 사용 건수")

# send + check의 경우 단순 scattor plot으로 봤을 경우 선형성을 띄는 것으로 보임
#ggplot(data = dutch_RC, aes(x = CHECK_SEND, y =claim_cnt )) + geom_bin2d()+geom_smooth(method=lm) # send + check의 경우 단순 scattor plot으로 봤을 경우 선형성을 띄는 것으로 보임

ggplot(data = dutch_RC, aes(x = SEND, y =claim_cnt )) + geom_point()+labs(x="SEND 건수", y="더치페이 사용 건수")+geom_smooth(method=lm)
 # send만 허용했을 경우 선형성을 띄지 않는 것으로 보임



dutch_RC2 <- left_join(dutchpay_recv_cnt3,dutchpay_claim_cnt2%>%rename(recv_user_id =claim_user_id), by = 'recv_user_id') #recv에 대한 claim 건수 조인
dutch_RC2_nona<-dutch_RC2%>%filter(!is.na(claim_cnt))
dutch_RC2_nona

ggplot(data = dutch_RC2_nona, aes(x = SEND, y =claim_cnt )) + geom_point()+labs(x="SEND 건수", y="더치페이 사용 건수")+geom_smooth(method=lm)


##상관분석으로 recv_cnt1과 claim_cnt / recv_cnt2와 claim_cnt 간의 상관성 확인 


a<-cor(dutch_RC[,c(4,9)]) 
a # send와 claim_cnt 의 상관계수 = 0.014 / send+check와 claim_cnt의 상관계수 = 0.766

corrplot(a)
cor.test(dutch_RC$SEND,dutch_RC$claim_cnt) #피어슨 상관계수를 통해 유의성 확인




dut_lm<-lm(data=dutch_RC, claim_cnt~SEND)
plot(dut_lm) #선형성을 띄지 않는 것으로 보여 회귀모델 x
summary(dut_lm) 

quantile(dutch_RC$SEND)
table(dutch_RC$SEND)           


######################더치페이 재사용율(retention)#############
dutchpay_claim3<-dutchpay_claim2%>%select(-yy,-mm,-dd)



dutchpay_claim3<-dutchpay_claim2%>%mutate(claim_mon = make_date(yy,mm,'01'))%>%select(-claim_at,-yy,-mm,dd)
dutchpay_claim3<-dutchpay_claim3%>%group_by(claim_user_id)%>%mutate(mon_start = min(claim_mon)
                                                                    ,mon_pass = (year(claim_mon)-year(mon_start))*12 + month(claim_mon)-month(mon_start))%>%data.frame()

dutch_retention<-dutchpay_claim3%>%group_by(mon_start,mon_pass)%>%summarise(n=n_distinct(claim_user_id))
dutch_re_result<-dcast(dutch_retention, mon_start ~ mon_pass, value.var = 'n')
dutch_re_result[is.na(dutch_re_result)]<-0
dutch_re_result[,c(2:ncol(dutch_re_result))]<-round(dutch_re_result[,c(2:ncol(dutch_re_result))]/(dutch_re_result[,2]),3)*100
dutch_re_result # 월별 사용자에 대한 리텐션


reten_dutch<-dutchpay_claim2%>%select(-yy,-mm,-dd)%>%group_by(claim_user_id)%>%mutate(claim_start = min(yymmdd),start_mon= format(claim_start,'%Y-%m'))%>%data.frame()
reten_dutch2<-reten_dutch%>%mutate(claim_pass = difftime(yymmdd,claim_start,units='days')
                                   ,pass_cate = ifelse(claim_pass ==0, 0,
                                                       ifelse( claim_pass <=30,1,
                                                               ifelse(claim_pass <=60,2,3
                                                               ))))

reten_dutch3<-reten_dutch2%>%group_by(start_mon,pass_cate)%>%summarise(n = n_distinct(claim_user_id))
day_retention<-dcast(reten_dutch3, start_mon ~ pass_cate, value.var = 'n') 
day_retention[is.na(day_retention)]<-0
day_retention[,c(2:ncol(day_retention))]<-round(day_retention[,c(2:ncol(day_retention))]/(day_retention[,2]),3)*100
day_retention_prob<-round(day_retention[,c(2:ncol(day_retention))]/(day_retention[,2]),3)*100
day_retention_prob_table<-cbind(day_retention[,1],day_retention_prob)  #30일 기준으로 사용자에 대한 리텐션
day_retention_prob_table

#reten_dutch2%>%group_by(claim_user_id)%>%summarise(n=n())%>%summarise(claim_2 = n_distinct(claim_user_id[which(n>=2)]),claim_all = n_distinct(claim_user_id))
#전체 사용자 53558 / 2회 이상 사용자 26728 / 재사용율 49.9
reten_dutch2_demo<-left_join(reten_dutch2,users%>%rename(claim_user_id = user_id), by = 'claim_user_id')
reten_dutch2_demo2<-reten_dutch2_demo%>%arrange(claim_user_id,claim_at)%>%data.frame()
head(reten_dutch2_demo2,n=20)

reten_dutch2_demo3<-reten_dutch2_demo2%>%mutate(user_lag = lag(claim_user_id,1)
                                                ,lag_1 = lag(claim_at,1))%>%filter(claim_user_id == user_lag)
head(reten_dutch2_demo3,n=20)
reten_dutch2_demo4<-reten_dutch2_demo3%>%select(claim_user_id,user_lag,claim_start,claim_at,lag_1,gender_cd,age)%>%mutate(diff_day = round(difftime(claim_at,lag_1,units='days')))
head(reten_dutch2_demo4,n=20)
reten_dutch2_demo5<-reten_dutch2_demo4%>%filter(diff_day!=0)%>%group_by(claim_user_id)%>%filter(lag_1==min(lag_1))%>%data.frame()
head(reten_dutch2_demo5,n=20)
table(reten_dutch2_demo5$diff_day) # 재사용까지 걸리는 일수
t<-reten_dutch2_demo5%>%group_by(diff_day)%>%summarise(n=n_distinct(claim_user_id))%>%data.frame()
ggplot(reten_dutch2_demo5,aes(x=diff_day))+geom_histogram(colour = 'black')

View(t)


 ##### who use dutchpay next month #####

user_1912 <- dutchpay_claim3%>%filter(yyyymm =='2019-12') #2019-12월 이용자
user_2001 <- dutchpay_claim3%>%filter(yyyymm =='2020-01') #2020-01월 이용자
user_2002 <- dutchpay_claim3%>%filter(yyyymm =='2020-02') #2020-02월 이용자

user_1912_cnt <- user_1912%>%group_by(claim_user_id)%>%summarise(claim_cnt = n_distinct(claim_id))%>%data.frame()
user_2001_cnt <- user_2001%>%group_by(claim_user_id)%>%summarise(claim_cnt = n_distinct(claim_id))%>%data.frame()
user_2002_cnt <- user_2002%>%group_by(claim_user_id)%>%summarise(claim_cnt = n_distinct(claim_id))%>%data.frame()

user_1912_demo <- left_join(user_1912_cnt,users%>%rename(claim_user_id = user_id), by = 'claim_user_id')
user_2001_demo <- left_join(user_2001_cnt,users%>%rename(claim_user_id = user_id), by = 'claim_user_id')
user_2002_demo <- left_join(user_2002_cnt,users%>%rename(claim_user_id = user_id), by = 'claim_user_id')

user_detail_1912

head(dutchpay_claim_detail)

user_1912_detail <- left_join(user_1912,dutchpay_claim_detail, by = 'claim_id')
user_2001_detail <- left_join(user_2001,dutchpay_claim_detail, by = 'claim_id')
user_2002_detail <- left_join(user_2002,dutchpay_claim_detail, by = 'claim_id')


user_1912_send_cnt <- user_1912_detail%>%filter(status == 'SEND')%>%
  group_by(recv_user_id)%>%summarise(send_cnt = n_distinct(claim_id)
                                     ,sum_amount = sum(send_amount)
                                     ,aps = sum_amount/send_cnt)%>%data.frame()

user_2001_send_cnt <- user_2001_detail%>%filter(status == 'SEND')%>%
  group_by(recv_user_id)%>%summarise(send_cnt = n_distinct(claim_id)
                                     ,sum_amount = sum(send_amount)
                                     ,aps = sum_amount/send_cnt)%>%data.frame()

user_2002_send_cnt <- user_2002_detail%>%filter(status == 'SEND')%>%
  group_by(recv_user_id)%>%summarise(send_cnt = n_distinct(claim_id)
                                     ,sum_amount = sum(send_amount)
                                     ,aps = sum_amount/send_cnt)%>%data.frame()


head(user_1912_send_cnt)
head(user_2001_send_cnt)
head(user_2002_send_cnt)


dutch_month_cnt<-dutchpay_claim3%>%group_by(claim_user_id, yyyymm)%>%summarise(n=n_distinct(claim_id))%>%data.frame()
dutch_month_cnt2<-dcast(dutch_month_cnt, claim_user_id ~ yyyymm, value.var = 'n')
dutch_month_cnt2[is.na(dutch_month_cnt2)]<-0
dutch_month_cnt2 # 더치페이 월별 Claim 건수 

dutch_month_cnt3<-left_join(dutch_month_cnt2,users%>%rename(claim_user_id = user_id), by = 'claim_user_id') #월별 건수 , 데모 정보 조인



dut_c<-left_join(dutch_month_cnt3,dutch_RC%>%rename(claim_user_id =recv_user_id), by = 'claim_user_id')
colSums(is.na(dut_c)) # 요청자 기준으로 응답에 대한 table 조인 시 na = 1469개 발생

dut_c[is.na(dut_c)]<-0
dut_c<-dut_c%>%rename(cnt_12 = '2019-12', cnt_01 = '2020-01',cnt_02 = '2020-02')

dut_c_info <- dut_c[,c(2:ncol(dut_c))]

rg <- ranger(data = dut_c_info, claim_cnt ~. , num.trees=1000, importance="impurity") # ranger 모델을 사용하여 claim_cnt에 대한 데이터의 중요 변수 확인
rg

rf$variable.importance

vip(rg) # 변수가 적어 대부분 claim_cnt에 관한 변수들이 중요변수 상위에 나타남

rg2<-ranger(data = dut_c_info%>%select(claim_cnt,gender_cd,age,foreigner_yn,os_type), claim_cnt ~. , num.trees=1000, importance="impurity") 
rg2

vip(rg2) # claim_cnt 관련된 count 변수들을 제외하고 데모정보만 입력 -> age 변수가 가장 중요변수로 들어감

dut_c_type<-dut_c_info%>%select(claim_cnt,gender_cd,age,foreigner_yn,os_type)
du_tree<-rpart(data = dut_c_type, claim_cnt ~.,control = rpart.control(cp=0.0003))
du_tree
du_tree$cp


rpart.plot(du_tree, type=2,extra = 1,digit =4 )

ggplot(data = dut_c_type, aes(x=age, y=claim_cnt)) +geom_point()

#####


dut_ccc<-dut_c%>%select(2,3,4,5,6,7,8,SEND,claim_cnt)%>%mutate(foreigner_yn = ifelse(foreigner_yn=='N',0,1)
                                                              ,os_type = ifelse(os_type=='A',0,1))


str(dut_ccc)
cluster_dut<-kmeans(dut_ccc , centers= 3,nstart=25)
cluster_dut$centers
View(cluster_dut$centers
)

clu<-as.factor(cluster_dut$cluster)

dut_cccc<-cbind(dut_ccc,clu)


ggplot(dut_cccc,aes(x=age,y=claim_cnt,color=clu))+geom_point()+labs(x="연령", y="더치페이 사용 건수")
 ## 3개월 간 평균 5회 정도 사용한 고객 그룹 2그룹






# 필요한 패키지 설치
library(stringi)
library(stringr)
library(arules)
library(ggplot2)
library(readxl)
library(dplyr)
library(arulesViz)
# 데이터 불러오기
pet<-read_xlsx("pet.xlsx")
pet<-data.frame(pet)

# 요약통계량 확인
summary(pet)

# 결측치 제거
pet<-na.omit(pet)

# 카테고리 분리하기
category<-strsplit(pet$카테고리명,">")

# 대분류에 해당하는 첫번째값만 가져오기
category_big<-unlist(lapply(category,function(dc){
  return(dc[1])}))

category_big<-data.frame(category_big,pet$주문수량)
colnames(category_big)<-c("category","주문수량")
# 댕댕이와 냥냥이만 선택하기
categoryd<-subset(category_big,category=="댕댕이")
categoryc<-subset(category_big,category=="냥냥이")
dogcat<-rbind(categoryc,categoryd)

# 총구매량 구하기
big<-dogcat%>%
  group_by(category)%>%
  summarise(총구매량=sum(주문수량))

# 그래프 그리기 
ggplot(big,aes(x=category,y=총구매량,fill=category))+geom_col()+xlab("대분류[댕댕이&냥냥이]")


# 대분류 그외에 것들 선택하기
besides<-subset(cbesides,category!="댕댕이")
besides<-subset(besides,category!="냥냥이")

# 총구매량 구하기
big_1<-besides%>%
  group_by(category)%>%
  summarise(총구매량=sum(주문수량))

# 그래프 그리기
ggplot(big_1,aes(x=category,y=총구매량,fill=category))+geom_col()+xlab("대분류[그외]")


# 주문일시에서 " " 기준으로 분리 
time<-strsplit(pet$주문일시," ")

# 주문일시에서 시간에 해당하는 부분만 가져오기
time<-unlist(lapply(time,function(t){
  return(t[2])}))

# 앞에 두글자만 가져오기(분을 제거)
time<-substr(time,1,2)
time_result<-data.frame(time,pet$주문수량)
colnames(time_result)<-c("time","주문수량")

# 시간에 따른 총구매량 구하기
time_count<-time_result%>%
  group_by(time)%>%
  summarise(총구매량=sum(주문수량))

# 그래프 그리기
ggplot(time_count,aes(x=time,y=총구매량,fill=time))+geom_col()+xlab("시간")

# 같은날짜&같은시간에 소비자가 구매한 상품을 알아보기 위해 주문일시와 임시고객번호를 합침
pastepet<-paste(pet$주문일시,pet$임시고객번호)

# pastepet별로 상품코드를 분류해서 고객이 어떠한 상품을 구매했는지를 확인
pet_list<-split(pet$상품코드,pastepet)

# arules함수 사용을 위한 transction형태로 변경
pet_trans<-as(pet_list,'transactions')
summary(pet_trans)  

# support(지지도)를 설정하지 않으면 규칙이 0개 생성되는 관계로 support값을 따로 지정 해줌.
pet_rules<-apriori(pet_trans,parameter =list(supp=0.0005))
summary(pet_rules)
inspect(pet_rules)


# 시각화 1
plot(pet_rules,method="paracoord")
# 시각화 2
plot(pet_rules,method='graph',control = list(type='items'),vertex.label.cex=0.7,edge.arrow.size=0.3,edge.arrow.width=2)


# rtest6.R
# Koweps_hpc10_2015_beta1.sav  SPSS,SAS,STATA

#

#- sav파일 읽는 법

#1) install.packages("foreign)"  // foreign 패키지 설치
#2) 라이브러리 실행
#3) 변수에 저장하는 코드 실행
#변수명 <- read.spss(file = "Koweps_hpc10_2015_beta1sav", to.data.frame = T)r
#＊to.data.frame = T는 spss 파일 데이터 프레임 형태로 변환, 이파라미터 설정하지 않으면 리스트 형태로 불러옴






install.packages("foreign")
library(foreign)

raw_wel <- read.spss(file = "Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

#복사본 만들어서 사용하기기
welfare <- raw_wel

View(welfare)
dim(welfare)
str(welfare)
summary(welfare)

# 열이름 변경
library(dplyr)
welfare <- rename(welfare, 
                  sex=h10_g3, 
                  birth=h10_g4,
                  marriage=h10_g10,
                  religion=h10_g11,
                  income=p1002_8aq1,
                  code_job=h10_eco9,
                  code_region=h10_reg7)
View(welfare)

# 성별 에 따른 월급 차이
# 성별 월급 변수 확인 전처리


# 변수 타입 확인
class(welfare$sex)

# 데이터 분포 확인
table(welfare$sex)

# NA 확인 
table(is.na(welfare$sex))
# -> 모두 False이므로 na값은 없다..

# 1 male  2 female 변경
welfare$sex <- ifelse(welfare$sex==1,"male","female")
table(welfare$sex)
# 빈도막대그래프
library(ggplot2)
qplot(welfare$sex)

# 월급 타입확인
class(welfare$income)

# 통계치
summary(welfare$income)

# 빈도막대그래프
qplot(welfare$income)+xlim(0,500)

# 0, 9999 => NA 변경
welfare$income <- ifelse(welfare$income %in% c(0,9999),NA,welfare$income)
# NA 개수 확인
table(is.na(welfare$income))

# 성별 월급 평균 만들기
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(sex) %>%
  summarise(mean_income=mean(income))

sex_income
# 그래프 sex_income 막대그래프 x 성별 y 월급평균
ggplot(data=sex_income,aes(x=sex,y=mean_income))+geom_col()

# 나이, 월급 관계
# 연령대 따른 월급 차이
# 연령대 따른 성별 월급 차이

# 태어난 연도 birth
class(welfare$birth)
summary(welfare$birth)
library(ggplot2)
qplot(welfare$birth)
table(is.na(welfare$birth))
# 열추가  현년도 - 태어난연도 +1
welfare$age <- 2015-welfare$birth+1
summary(welfare$age)

# 나이에 따른 월급 평균표 만들기
# age_income <- 
# income월급 na 제외
# age 그룹
# 요약 mean_income  월급 평균
library(dplyr)
age_income <-welfare %>%
  filter(!is.na(income)) %>%
  group_by(age) %>%
  summarise(mean_income=mean(income))

age_income

# 데이터 age_income x age y mean_income 선그래프
ggplot(data=age_income,aes(x=age,y=mean_income))+geom_line()

# 연령대에 따른 월급 차이
# young 30세미만
# middle 60세 미만
# old 60세 이상

welfare <- welfare %>%
  mutate(ageg=ifelse(age<30,"young",ifelse(age<60,"middle","old")))
table(welfare$ageg)

# ageg_income <- 
# income na 제외
# ageg 그룹
# mean_income income평균
ageg_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg) %>%
  summarise(mean_income=mean(income))
age_income
# 막대그래프 ageg_income  x=ageg  y mean_income
ggplot(data=ageg_income,aes(x=ageg,y=mean_income))+geom_col()

# 연령대 및 성별  그룹  월급 차이 
# sex_income
# income na 제외
# ageg, sex 그룹
# mean_income income평균
sex_income <- welfare %>%
  filter(!is.na(income)) %>%
  group_by(ageg,sex) %>%
  summarise(mean_income=mean(income))

# 막대그래프 sex_income  x=ageg,y=mean_income,fill=sex
ggplot(data=sex_income,aes(x=ageg,y=mean_income,fill=sex))+geom_col(position = "dodge")


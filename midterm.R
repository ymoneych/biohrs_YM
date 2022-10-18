

library(data.table);library(magrittr);library(ggpubr);library(rvg);library(officer) 
a <- fread("https://raw.githubusercontent.com/jinseob2kim/R-skku-biohrs/main/data/example_g1e.csv")


### Q1. “Q_” 로 시작하는 변수는 범주형(factor)으로, 나머지 변수는 숫자형(integer)으로 만드세요.
# "Q_"로 시작하는 변수 추출
var.factor <- grep("Q_", names(a), value = T)  
# 나머지 변수 추출
var.conti <- setdiff(names(a), var.factor)

# Q로 시작하는 변수 factor로 만들기 
a[, (var.factor) := lapply(.SD, as.factor), .SDcols = var.factor]
# 나머지 변수 numeric으로 만들기
a[, (var.conti) := lapply(.SD, as.numeric), .SDcols = var.conti]

sapply(a, class)


### Q2. 연속 변수 “WSTC”와 “BMI”의 연도별 평균 및 표준편차를 구하세요.
a_mean <- a[, .(WSTC.mean = mean(WSTC), BMI.mean = mean(BMI)), keyby = .(EXMD_BZ_YYYY)]
a_sd <- a[, .(WSTC_sd = sd(WSTC), BMI_sd = sd(BMI)), keyby = .(EXMD_BZ_YYYY)]
merge(a_mean, a_sd, by='EXMD_BZ_YYYY')



### Q3. 연도별 “FBS”를 나타내는 Boxplot을 그린 후 pptx로 저장하세요. (x축: “EXMD_BZ_YYYY”, y축: “FBS”)
p <- a[,EXMD_BZ_YYYY := factor(EXMD_BZ_YYYY)] %>% 
ggboxplot(x= "EXMD_BZ_YYYY", y= "FBS", fill="EXMD_BZ_YYYY", row.names=F)

# pptx로 저장하기
plot_file <- read_pptx() %>%
  add_slide() %>% ph_with(dml(ggobj = p), location = ph_location_type(type="body"))
print(plot_file, target = "plot_file.pptx")



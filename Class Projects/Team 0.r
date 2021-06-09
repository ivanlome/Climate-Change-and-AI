#creación de base de datos.
dir.data<-"//Users//ivanlomeli//Documents//R//Climate-Change-and-AI//Data//Model/"
data<-read.csv(paste0(dir.data,"ModelData.csv"))
bad.vars<-sapply(data, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
good.vars<-names(bad.vars[bad.vars<0.03])
data<-data[,good.vars]
data<-data[-c(4,8,22,58,60,92,93,101,108,113,132,139,145,157,160,175,179,190),]
dir.data<-"//Users//ivanlomeli//Documents//R//Climate-Change-and-AI//Data/"
data.2<-read_xlsx(paste0(dir.data,"CPI2020_GlobalTablesTS_210125.xlsx"))
bad.vars<-sapply(data.2, function(x) {mean(ifelse(is.na(x)==TRUE,1,0))})
good.vars<-names(bad.vars[bad.vars<0.3])
data.2<-data.2[,good.vars]
data.2<-data.2[-c(1,2,15,30,110,146),]
data.3<-data.2[order(match(data.2$...2,data$iso_code3)),]
data<-cbind(data.3,data)
data<-data[,-c(6,7,8,9,10,16,17)]
View(data)
colnames(data)[2:10]<- c("ISOCODE","Region","CPI.score","Ranking","EcoIUCR","GICRiskR","PRSRiskG",
                        "VarDemoP","WEcoForum")

#exploración de las variables
lm.prueba<-lm(EN.ATM.CO2E.PC~ CPI.score+EcoIUCR+GICRiskR+VarDemoP, data=data)
summary(lm.prueba)
#
#busqueda de variables con mayor significancia.
data3<-data2[,1:17]
lm.prueba<-lm(EN.ATM.CO2E.PC~ .-iso_code3-country, data=data3)
summary(lm.prueba)
data4<-data2[,18:25]
lm.prueba2<-lm(EN.ATM.CO2E.PC~ CPI.score.2020, data=data2)
summary(lm.prueba2)
summary(data2$EN.ATM.CO2E.PC)
#regresión logistica
CO2.bi<- rep(1,length(EN.ATM.CO2E.PC))
CO2.bi[EN.ATM.CO2E.PC<median(EN.ATM.CO2E.PC)]=0
data2.bi <- data.frame(data2,CO2.bi)
#datos de prueba y de ensayo.
Entre<-(1:9)

prueba<- (10:18)

data.prueba<-data2.bi[prueba,]

#reg logistica
glm.1<-glm(CO2.bi~CPI.score.2020+ a_water+a_education+a_energy+a_environment+
             a_water+m_economy.wide+IT.CEL.SETS.P2+AG.LND.FRST.K2, 
           data=data2.bi,family=binomial,subset=Entre)
summary(glm.1)

#Ahora se creara la matriz de confusión
glm.prob<-predict(glm.1, data.prueba ,type="response")
glm.pre<-rep(1,length(glm.prob))
glm.pre[glm.prob<0.5]=0
table(glm.pre, data.prueba$CO2)
mean(glm.pre!=data.prueba$CO2.bi)

#validación cruzada:
library (boot)
glm.fit<-glm(EN.ATM.CO2E.PC~ CPI.score.2020, data=data2)
cv.err<-cv.glm(data2 ,glm.fit)
cv.err$delta
cv.error<-rep(0,5)
for (i in 1:5){
  glm.fit<-glm(EN.ATM.CO2E.PC~poly(CPI.score.2020,i),data=data2)
  cv.error[i]<-cv.glm(data2,glm.fit)$delta[1]
}
cv.error
#
lm.prueba3<-lm(EN.ATM.CO2E.PC~ CPI.score.2020+ a_water+a_education+a_energy+a_environment+
                 a_water+m_economy.wide+IT.CEL.SETS.P2+AG.LND.FRST.K2, data = data2)
summary(lm.prueba3)


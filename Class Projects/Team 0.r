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
data<-data[-c(55,79),-c(6,7,8,9,10,16,17)]
View(data)
colnames(data)[2:10]<- c("ISOCODE","Region","CPI.score","Ranking","EcoIUCR","GICRiskR","PRSRiskG",
                        "VarDemoP","WEcoForum")
attach(data)
data<-transform(data, Region=as.factor(Region),CPI.score=as.numeric(CPI.score),
                Ranking=as.numeric(Ranking),EcoIUCR=as.numeric(EcoIUCR),GICRiskR=as.numeric(GICRiskR),
                PRSRiskG=as.numeric(PRSRiskG),VarDemoP=as.numeric(VarDemoP),
                WEcoForum=as.numeric(WEcoForum))
#exploración de las variables
lm.prueba<-lm(EN.ATM.CO2E.PC~ CPI.score+EcoIUCR+IT.CEL.SETS.P2+AG.LND.FRST.K2, data=data)
summary(lm.prueba)



#regresión logistica

CO2.bi<- rep(1,length(EN.ATM.CO2E.PC))
CO2.bi[EN.ATM.CO2E.PC<median(EN.ATM.CO2E.PC)]=0
data.bi <- data.frame(data,CO2.bi)
CO2.bi
#datos de prueba y de ensayo.
entre<-(1:(174/2))

prueba<- ((174/2)+1):(length(data))
prueba

data.prueba<-data.bi[prueba,]

#reg logistica
glm.1<-glm(CO2.bi~ CPI.score+EcoIUCR+IT.CEL.SETS.P2+AG.LND.FRST.K2, 
           data=data.bi,family=binomial,subset=entre)
summary(glm.1)

#Ahora se creara la matriz de confusión
glm.prob<-predict(glm.1, data.prueba ,type="response")
glm.pre<-rep(1,length(glm.prob))
glm.pre[glm.prob<0.5]=0
table(glm.pre, data.prueba$CO2.bi)
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


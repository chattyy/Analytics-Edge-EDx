WHO=read.csv("/Users/eshanchatty/Downloads/Visualization/WHO.csv")
str(WHO)
plot(WHO$GNI,WHO$FertilityRate)
library(ggplot2)
scatterplot=ggplot(WHO, aes(x=GNI,y=FertilityRate))# we can tell which objects to use, hence we can build different graphs using the same function.
scatterplot+geom_point()
scatterplot+geom_line()
fert_GNI=scatterplot+geom_point(colour="blue",size=3,shape=17)+ggtitle("FertilityV/SGrossNationalIncome")
pdf("Myplot.pdf")
print(fert_GNI)
dev.off()
#Regionwise Plotting
ggplot(WHO, aes(x=GNI,y=FertilityRate,color=Region))+geom_point()
ggplot(WHO, aes(x=GNI,y=FertilityRate,color=LifeExpectancy))+geom_point()
ggplot(WHO, aes(x=FertilityRate,y=Under15))+geom_point()
ggplot(WHO, aes(x=log(FertilityRate),y=Under15))+geom_point()#Here we can see a Linear Relationship.
model=lm(Under15~FertilityRate,data = WHO)
summary(model)#w/o log we get R2 of 87%
model1=lm(Under15~log(FertilityRate),data = WHO)
summary(model1)#Using the log we get an R2 of 93.91%
ggplot(WHO, aes(x=log(FertilityRate),y=Under15))+geom_point()+stat_smooth(method="lm",level=0.99)#by default stat_smooth gives 99.5% Confidence.
ggplot(WHO, aes(x=log(FertilityRate),y=Under15,color=Region))+geom_point()

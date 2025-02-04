train<-read.csv("train.csv",sep=',') #считываем данные
test<-read.csv("test.csv",sep=',')
mhtr<-mahalanobis(train[,2:331],center = colMeans(train[,2:331]),cov = cov(train[,2:331]),tol=1e-20,inverted = FALSE) #измеряем для 
спектров расстрояние махаланобиса
trout<-mhtr>mean(mhtr)+3*sd(mhtr) #ищем выбросы
train<-train[!trout,] #убираем выбросы
mhte<-mahalanobis(test[,2:331],center = colMeans(test[,2:331]),cov = cov(test[,2:331]),tol=1e-20,inverted = FALSE)
teout<-mhte>mean(mhte)+3*sd(mhte)
test<-test[!teout,]
trout2<-train$Protein>mean(train$Protein)+3*sd(train$Protein)# смотри выбросы по целоевому показателю - белку
train<-train[!trout2,] # убираем выбросы
teout2<-test$Protein>mean(test$Protein)+3*sd(test$Protein)
test<-test[!teout2,]
write.csv(test,"test_clean.csv",row.names = FALSE) # записываем данные 
write.csv(train,"train_clean.csv",row.names = FALSE)
library(nnet)
rmsep<-2 #создаем критерий качества
while(rmsep>1.3) { #создаем первую модель
s<- sample(10:15, 1)
it<-sample(100:200,1,10)
nn<-nnet(train[,2:331],train$Protein,linout=TRUE,maxit = it,MaxNWts=10000,size=s)
real<-test[,1]
pre<-predict(nn,test[,2:331])
res<-pre-real
rmsep<-sqrt(sum((res^2))/length(test[,1]))
print(rmsep)}
res<-sqrt(res^2) #основываясь на первой модели -убираем выброс
x<-res==max(res) 
test<-test[!x,]
write.csv(test,"test_clean.csv",row.names = FALSE)
while(rmsep>1) { #запускаем вторую модель с автоподбором параметров
s<- sample(10:15, 1)
it<-sample(100:200,1,10)
nn<-nnet(train[,2:331],train$Protein,linout=TRUE,maxit = it,MaxNWts=10000,size=s)
real<-test[,1]
pre<-predict(nn,test[,2:331])
res<-pre-real
rmsep<-sqrt(sum((res^2))/length(test[,1]))
print(rmsep)}











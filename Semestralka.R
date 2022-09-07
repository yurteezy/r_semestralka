##############
## Priklad1 ##
##############

## (3body):Mate zadany vektor X = {1, 1, 2, 2, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 5}.
## 1.Veskere sude prvky vektoru X umocnete na druhou a odectete od nich cislo 1. 
## 2.Nasledne veskere prvkyvektoru X celociselne delitelne cislem 3 vydelte tremi. 
## 3.Zupravenehovektoru(viz. bod 1 a 2)vypocitejte prumer a sumuprvku vektoru.


x = c(1, 1, 2, 2, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 1, 1, 2, 2, 2, 3, 3, 5)

##1##

x <- ifelse(x%%2==0,(x^2)-1,x)

##2##

x <- ifelse(x%%3==0,x/3,x)

##3##

mean(x)
sum(x)




##############
## Priklad2 ##
##############

library(car)
pr1 <- Blackmore

##1#
sapply(pr1,table)

 
##2##
 
num=nrow(pr1)

vektor <- c()
for (i in 1:num) {
  if (pr1$age[i] > 12 & pr1$group[i] == "patient") {
    vektor[i] <- pr1$exercise[i]
  }
  
}

prum <- mean(vektor,na.rm=TRUE)
cat(" prumerny pocet hodin venovanych cviceni behem jednoho tydne pouze pro
subjekty starsi 12 let a patrici do skupiny patient = ", prum)


##3##

vektor1 <- c()
vektor2 <- c()
for (i in 1:num) {
  if (pr1$group[i] == "patient") {
    vektor1[i] <- pr1$exercise[i]
  }
  else { vektor2[i] <- pr1$exercise[i]
    }
  
}

cat("minimum pro patient =",min(vektor1,na.rm=TRUE))
cat("maximum pro patient =",max(vektor1,na.rm=TRUE))
cat("minimum pro control =",min(vektor2,na.rm=TRUE))
cat("maximum pro control =",max(vektor2,na.rm=TRUE))

##4##

library("ggplot2")

ggplot(pr1, aes(x=group, y=exercise,fill=group)) + 
  geom_boxplot(
    
    
    color="blue",
    
    alpha=0.9,
    
    
    notch=TRUE,
    notchwidth = 0.8,
    
    
    outlier.colour="#FF6666",
    outlier.fill="red",
    outlier.size=3
    
  )

##############
## Priklad3 ##
##############

semlist <- list(Spolecnost_1=list(" a. Jmeno spolecnosti:"= c("Firma 1"),
                                  " b. Forma:" = "Akciovka",
                                  " c. Pocet zamestnancu:" = 666,
                                  " d. Tuzemska firma" = TRUE ),
                Spolecnost_2=list (" a. Jmeno spolecnosti:"= c("Firma 2"),
                                    " b. Forma:" = "Akciovka",
                                    " c. Pocet zamestnancu:" = 871,
                                    " d. Tuzemska firma" = FALSE ))
        
semlist$Spolecnost_1$" e. Prumerny vek zamestancu" = 38
semlist$Spolecnost_2$" e. Prumerny vek zamestancu" = 27
cat(str(semlist))


##############
## Priklad4 ##
##############

library(ggplot2)
x<- seq(from = 0.1, to = 10, by =0.001)
pp <- function(x){sin(1/x)}

plot(pp(x),x=x,
     main="Funkce sin(x/1) od 0.1",
     ylab="sin(1/x)",
     xlab="x",
     type="l",
     col="blue")


x1<-seq(from= -10, to = 10, by=0.01)  
eq<- function(x){(2*cos(x)+sin(2*x)*cos(60*(x^2)))}
plot(main="druha funkce",
     xlab="x",
     ylab="y",
     eq(x1),x=x1, 
     type='l',
     col="#FF6666")


##############
## Priklad5 ##
##############


myF <- function(x,y){
  (log(abs((x^3)-(7*y)),y))*(exp(1)^((x^2)-(y/2)))*(sqrt((x^4)+1))*(cos(y))
}
myF(1,12)

##############
## Priklad6 ##
##############

{
srt <- function(v) {
  for (i in 1:(length(v) - 1)) {
    if (v[i] > v[i+1]) {
      qw <- v[i]
      v[i] <- v[i+1]
      v[i+1] <- qw
    }}
  v 
}
mySort <- function(v) {
  while (is.unsorted(v)) {
    v <- srt(v)
  }
  v
}
}

mySort(c(1,4,5,6,7,123,4))


##############
## Priklad7 ##
##############

NSD <- function(x, y) {
  if(x > y) {ee = y }
  else      {ee = x }
  for(i in 1:ee) {
    if((x%%i==0)&(y%%i==0)) {
      NSD = i
    }
  }
  return(NSD)
}

NSD(1336,2032)


##############
## Priklad8 ##
##############

##vyuzil jsem tady uz nadefinovanou funkci z prikladu 6, aby pretridit vektor, doufam ze je to dovolene =)
mMin <- function(m,v){
  as <- mySort(v)
  as[m]
  
}
mMin(6,c(10,9,8,7,6,5,4,3,2,1))



##############
## Priklad9 ##
##############

solveQE <- function(a,b,c){
  d <- (b^2)-(4*a*c)
  if  (d<0) 
  { print("Neexistuji realne koreny pro tuto rovnici")}
  else if (d>0) { x11 <- ((-b)+sqrt(d)) / (2*a) 
          x22 <- ((-b)-sqrt(d)) / (2*a)
          print(x11)
          print(x22)
  }
  else { x33 <- (-b)/(2*a) 
  print(x33)
  }
}
solveQE(1,4,4)
solveQE(2,2,-4)
solveQE(2,2,4)



################################################
####################  Cast 2  ##################
################################################


#1

{df <- read.csv("https://query.data.world/s/nkx7phxc7rjd2z6mkubk2byguvkfe3", header=TRUE, stringsAsFactors=FALSE)
df$growth <- df$growth/1000
df$ifiid = NULL
df$ifmid = NULL
df$id=NULL
df$metro=NULL
df<- df[seq(1:40),]
}

print("tato tabulka udelana americkym casopisem INC. To je seznam 5000 nejrychleji rostoucich soukrome vlastnenych spolecnosti 
      ve Spojenych statech.Budu pracovat pouze s prvnimi 40,jinak to nedava smysl")
print("sloupec growth ukazuje rust trzby za poseldni rok v procentech, sloupec revenue ukazuje trzby za poseldni rok v $")
print("myslim ze vsechno ostatne musi byt jasne")

#2
df$rev_per_employee <- df$revenue/df$workers
print("novy sloupec ukazuje kolik penez kazdy zamestnanec generuje pro firmu")

#3
cat("dimenzionalita  ",dim(df))
summary(df)
boxplot(df$growth,ylab="Growth in %",main="growth boxplot of companies",col="darkgreen")
boxplot(df$workers,ylab="the number of workers",main="The number of workers boxplot",col="darkgreen")


##4

#4.1
library(tidyverse)
library(ggplot2)
p1 <- df %>% filter(between(workers,100,150)) %>% select(growth) %>% colSums(na.rm=TRUE)
p2 <- df %>% filter(between(workers,150,200)) %>% select(growth) %>% colSums(na.rm=TRUE)
p3 <- df %>% filter(workers>200) %>% select(growth) %>% colSums(na.rm=TRUE) 

cz <-c(p1,p2,p3) 
names(cz)<-c("100-150 workers","150-200 workers",">200 workers")

barplot(cz,names=names(cz),col = c("brown","#ddaa00","pink"),
        main="growth of companies by workers in summarise",
        )


  #4.2
ggplot(df[seq(1:20),], aes(x=rank, y=revenue)) +
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2) +
  geom_point(size=3, color="red") +
  ggtitle("Dependence between rank an revenue of the first 20 companies")

#4.3

install.packages(plotrix)  
library(plotrix)
pie3D(which(table(df$state_l)>1),labels=names(which(table(df$state_l)>1)),
      main="Pie Chart of the states which have more then 1 company",
      theta=0.8,
      explode=0.1,
      )
#4.4

ggplot(df, aes(y=state_l,x=workers)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  xlab("the number of workers") +
  ggtitle("The number of workers by states")+
  theme_bw()


#5
#nepodarilo se mi udelat mapu =(
     
#6

library(tidyverse)
##1
cat("companies that have revenue bigger then average")
df %>% filter(revenue > mean(revenue)) %>% 
  .$company 

##2
df %>% group_by(state_l) %>% summarise(city = n())

##3
df %>% filter(between(growth, 10, 20)) %>% select(company,growth,revenue,city)


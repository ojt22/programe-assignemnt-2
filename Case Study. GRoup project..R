transaction=read.csv("C:\\Users\\Siddharth\\Desktop\\R programming\\R case study (group)\\Transactions.csv")
customer=read.csv("C:\\Users\\Siddharth\\Desktop\\R programming\\R case study (group)\\Customer.csv")
product=read.csv("C:\\Users\\Siddharth\\Desktop\\R programming\\R case study (group)\\prod_cat_info.csv")
getwd()
library(dplyr)

# Ques1. Merge all the datasets as Consumer_Final
#named the data files according to our convenience 

transc_prod <- left_join(transactions,product, by=c("prod_cat_code","prod_subcat_code"="prod_sub_cat_code"))

consumer_final <- left_join(transc_prod,customer,by="cust_id")



# Ques2(a) Get the column names and their corresponding data types  
#Since we wanted a seperate column we used sapply which returns the vector or a matrix or a array as it is ,along with providing its class.
table_datatypes=sapply(consumer_final, class)
#sapply cause loop in hte background.
 #we can  ALSO use str giving us the class of the data frame(every column).
str(consumer_final)


# Ques2(b) We want to Get the top/bottom 20 observations ,as there are 18 Major subcatergories of products.
#ASK MAAM
#To analyse which   store type/ product maximum transactions.
#WE use the head/ tail function which provides us the first or the last parts of any vector ,data frame etc .
#head()(tails)),when all  n(i.e) rows >0, tails(head),n<0
#sales can never be -ve.
top20 <- head(consumer_final,20)

#1Since the data is from 2011-2014 stating hte recent entries if 2014 on top, this gives us entires only from 2014. We also see, that the retailer has -ve amt of quantities sold which indicates a LOSS.
#2provides us info that, mainly the transactions of BOOKS(prod_cat/productcategory)  has been done through e-shop or teleshop. 
bottom20 <- tail(consumer_final,20)
#They give us entires from 2011, MAJORITY qnt and amount are  +ve stating it had prfits back then and is eventually going into a loss as years increase.
#just to combine together
top_bottom20<-full_join(top20,bottom20)
## ASK MAAM DIFF IN top_n function and head/tails function

# Ques2(c) Five number summary for the dataset
five_no_sum <- summary(consumer_final)
#to view it
View(five_no_sum)
##Conclusion(why we need it) :mean,media,Max and min number, 1st qt /3rd qt mean.
#EG: To find transactions on a particular date like on (tran_date) 13-07-2011: -> 35  


# Ques2(d) Frequency table for all catagorical variables
 
Frequency2<-consumer_final%>%group_by(prod_subcat_code)%>%summarise(Total=n())
Frequency3<-consumer_final%>%group_by(Store_type)%>%summarise(Total=n())
Frequency1<-consumer_final%>%group_by(prod_cat,prod_cat_code)%>%summarise(Total=n())
Frequency4<-consumer_final%>%group_by(prod_subcat_code,prod_subcat)%>%summarise(Total=n())
Frequency5<-consumer_final%>%group_by(Gender)%>%summarise(Total=n())
Frequency6<-consumer_final%>%group_by(city_code)%>%summarise(Total=n())
#we used dplyr function group by which is used to add columns togethers .
#similarly summarise to group together.

#Ques3 Create frequency bars for all catagorical variables and histogram for all continuous variables
install.packages("ggpubr")
library(ggpubr)
library(ggplot2)
Plot1<-ggplot(Frequency1,aes(x=prod_cat_code,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot2<-ggplot(Frequency2,aes(x=prod_subcat_code,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot3<-ggplot(Frequency3,aes(x=Store_Type,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot4<-ggplot(Frequency4,aes(x=prod_cat,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot5<-ggplot(Frequency5,aes(x=prod_sub_cat_code,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot6<-ggplot(Frequency6,aes(x=prod_subcat,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot7<-ggplot(Frequency7,aes(x=Gender,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Plot8<-ggplot(Frequency8,aes(x=city_code,y=Total))+ geom_bar(fill="#0073C2FF",stat = "identity")+geom_text(aes(label=Total),vjust=-0.3)+theme_pubclean()
Hist1<-hist(Consumer_Final$Rate, breaks=20, main="Rate",xlab = "Rate")
Hist2<-hist(Consumer_Final$Tax, breaks=20, main="Tax",xlab = "Tax")    
Hist3<-hist(Consumer_Final$total_amt, breaks=20, main="Amount",xlab = "Amount")

# Ques4(b)
sum(consumer_final$total_amt<0)
# Ques4(a)
consumer_final$tran_date<-dmy(consumer_final$tran_date)
consumer_final$year<-year(consumer_final$tran_date)
consumer_final$month<-month(consumer_finall$tran_date)
consumer_final$day<-day(consumer_finall$tran_date)
span<-interval(ymd("2011-01-25"),ymd("2014-02-28"))
duration<-as.duration(span)
library(lubridate)


interval(ymd("2011-01-25"))
# Ques5

Male_popular<-Male%>%group_by(prod_cat)%>%summarise(Total=n())%>%arrange(desc(Total))
Female_popular<-Female%>%group_by(prod_cat)%>%summarise(Total=n())%>%arrange(desc(Total))
Consumer_Final%>%group_by(Gender)%>%summarise(n())
Male<-filter(Consumer_Final,Gender=="M")
Female<-filter(Consumer_Final,Gender=="F")

#Ques6
a<-Consumer_Final%>%group_by(cust_id,city_code)%>%summarise(Total=n())
filter(a,city_code=="1")
filter(a,city_code=="2")
filter(a,city_code=="3")
filter(a,city_code=="4")
filter(a,city_code=="5")
filter(a,city_code=="6")
filter(a,city_code=="7")
filter(a,city_code=="8")
filter(a,city_code=="9")
filter(a,city_code=="10")

525+536+566+559+560+506+553+541+522+536# Total number of customers

(566/5404)*100# percentage of customers in city code 3 

#Ques7
store<-Consumer_Final%>%group_by(Store_type)%>%summarise(Ttoal=sum(total_amt))
store1<-Consumer_Final%>%group_by(Store_type="e-Shop",prod_cat)%>%summarise(Ttoal=sum(total_amt))%>%arrange(desc(Ttoal))
store2<-Consumer_Final%>%group_by(Store_type="TeleShop",prod_cat)%>%summarise(Ttoal=sum(total_amt))%>%arrange(desc(Ttoal))
store3<-Consumer_Final%>%group_by(Store_type="Flagship store",prod_cat)%>%summarise(Ttoal=sum(total_amt))%>%arrange(desc(Ttoal))
store4<-Consumer_Final%>%group_by(Store_type="MBR",prod_cat)%>%summarise(Ttoal=sum(total_amt))%>%arrange(desc(Ttoal))

#Ques8
filter<-filter(Consumer_Final,prod_cat==c("Electronics","Clothing"))
filter%>%group_by(Store_type="Flagship store")%>%summarise(Total_Clothing_Electronics=sum(total_amt))

#Ques9

Male_electronics<-Male%>%group_by(prod_cat="Electronics")%>%summarise(Total=sum(total_amt))

#Ques10
#Ques11(a)
electronics<-filter(Consumer_Final,prod_cat=="Electronics")
electronics_amt<-electronics%>%group_by(prod_cat)%>%summarise(sum(total_amt))
books<-filter(Consumer_Final,prod_cat=="Books")
books_amt<-books%>%group_by(prod_cat)%>%summarise(sum(total_amt))
total_spent<-rbind(electronics_amt,books_amt)
#Ques11(b)
jan_feb<-filter(Consumer_Final,month==c("1","2"),year=="2014")

jan_feb_spent<-jan_feb%>%group_by()%>%summarise(Total=sum(total_amt))


































-------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------
names(Consumer_Final$year)

names(Frequency1)
names(Consumer_Final)
View(consumer_final1)
library(dplyr)
names(dplyr)
ls(dplyr)
length(dplyr)
names(dplyr)
count(consumer_final1$Gender)
group_by(consumer_final1)
summarise(consumer_final1)
count(consumer_final1$Gender)
count.fields(consumer_final1$Gender)
count_(consumer_final1$Gender)
count(consumer_final1$Gender)
tally(consumer_final1$Gender)
names(consumer_final1$Gender)
Gender
names(consumer_final1$Gender)
names(consumer_final1$cust_id)
names(consumer_final1$DOB)
names(consumer_final1)
names(consumer_final1,"Gender" )
names("Gender"  )
count(consumer_final1,name ="M","f")
count(consumer_final1,name ="Gender")
library(dplyr)
count(Consumer_Final,name ="prod_subcat")
View(product)
View(customer)
names(customer)
count(customer,name ="Gender")
count(customer,name ="M")
count(customer)
group_by(customer)
summarise(customer)
summarise(group_by(customer))
summary(customer)
customer[3][1]
customer[3]
customer[3][2]
summary(customer)
summary(customer$Gender)
Gender[2]
summary(customer$Gender)
frequency(customer$Gender)
frequency(customer)
View(summary(customer$Gender))
gender=View(summary(customer$Gender))
gender=summary(customer$Gender)
View(gender)
View(product)
summary(product)
summary(product$prod_cat)
prod_cat=summary(product$prod_cat)
View(prod_cat)
summary(product$prod_subcat)
View(prod_subcat)
prod_subcat=summary(product$prod_subcat)
View(prod_subcat)
summary(product)
summary(product$prod_cat_code)
summary(product$prod_sub_cat_code)
summary(customer)
prod_cat_code=summary(product$prod_cat_code)
prod_sub_cat_code=summary(product$prod_sub_cat_code)
cust_id=summary(customer$cust_id)
city_code=summary(customer$city_code)
View(prod_cat_code)
View(prod_sub_cat_code)
View(cust_id)
View(city_code)
View(prod_cat_code)
dim(city_code)
prod_cat_code=summary(product$prod_cat_code)
summary(product$prod_cat_code)
View(summary(product$prod_cat_code))
table(city_code)
city_code=summary(customer$city_code)
summary(product$prod_cat_code)
summary(product$prod_cat_code)
summary(customer$cust_id)
summary(customer$city_code)
summary(customer$city_code,is.na=T
        summary(customer$city_code,is.na=T)
        consumer_final1[consumer_final1$Gender=='M',]
        View(df)
        Vnames(df)
        names(df)
        df=consumer_final1[consumer_final1$Gender=='M',1:50]
        df=consumer_final1[consumer_final1$Gender=='M',]
        length(df)
        names(consumer_final1)
        df=consumer_final1[consumer_final1$Gender=='M','cust_id']
        View(df)
        library(dplyr)
        sum(consumer_final1$total_amt<0)
        neg=sum(consumer_final1$total_amt<0)
        View(neg)
        neg=sum(consumer_final1$total_amt<0)
        neg=sum(consumer_final1$total_amt>0)
        neg=sum(consumer_final1$total_amt>0)
        neg=sum(consumer_final1$total_amt<0)
        pos=sum(consumer_final1$total_amt>0)
        9294+89999
        consumer_final[consumer_final$Gender=='M',"prod_cat"]
        female.pop=consumer_final[consumer_final$Gender=='M',"prod_cat"]
        View(female.pop)
        rm(female.pop)
        summary(female.pop)
        2008=18696+4554+12850+4407+8536
        2008+18696+4554+12850+4407+8536
        summary(customer$Gender)
        summary(consumer_final1$Gender)
        48202+51051
        sum(is.na(consumer_final1$Gender))
        summary(consumer_final1$Gender,)
        sum(consumer_final1$Gender=='F')
        sum(consumer_final1$Gender=='M')
        View(consumer_final)
        View(consumer_final1)
        48202+51051
        summary(consumer_final1$Gender,)
        consumer_final1[consumer_final1$Gender=='F',"prod_cat"]
        male.pop=consumer_final1[consumer_final1$Gender=='F',"prod_cat"]
        summary(male.pop)
        summary(consumer_final1$city_code)
        summary(consumer_final1$city_code,is.na(T))
        summary(consumer_final1$city_code,is.na(F))
        summary(consumer_final1$city_code,na.rm(T))
        summary(consumer_final1$city_code)
        View(consumer_final1$city_code,consumer_final1$cust_id)
        a=(consumer_final1$city_code,consumer_final1$cust_id)
        df1=consumer_final1[consumer_final1$city_code=='4','cust_id']
        View(df1)
        length(df1)
        df2=consumer_final1[consumer_final1$city_code=='10','cust_id']
        length(df2)
        df3=consumer_final1[consumer_final1$city_code=='3','cust_id']
        length(df3)
        df4=consumer_final1[consumer_final1$city_code=='9','cust_id']
        length(df4)
        df5=consumer_final1[consumer_final1$city_code=='5','cust_id']
        length(df5)
        df6=consumer_final1[consumer_final1$city_code=='2','cust_id']
        length(df6)
        df7=consumer_final1[consumer_final1$city_code=='8','cust_id']
        length(df7)
        length(consumer_final1$cust_id)
        length(df1)/length(consumer_final1$cust_id)*100
        #percentage of customers in this city code
        table(consumer_final1$Store_type,consumer_final1$prod_cat)
        #percentage of customers in this city code
        View(table(consumer_final1$Store_type,consumer_final1$prod_cat))
        View(table(consumer_final1$Store_type,consumer_final1$prod_cat=='Books'))
        View(table(consumer_final1$Store_type,consumer_final1$prod_cat[consumer_final1$prod_cat]=='Books'))
        View(table(consumer_final1$Store_type,consumer_final1[consumer_final1$prod_cat]=='Books'))
        View(table(consumer_final1$Store_type,consumer_final1[consumer_final1$prod_cat]=='Books'))
        length(consumer_final1$prod_cat='Books')
        length(consumer_final1$prod_cat=='Books')
        14682+7338+7344+7050
        View(length(consumer_final1$prod_cat=='Books'))
        sum(consumer_final1$prod_cat=='Books')
        #df1 is the highest
        length(df1)
        #df1 is the highest
        sum(df1)
        sum(consumer_final1$prod_cat=='Books')
        View(table(consumer_final1$prod_cat,consumer_final1$total_amt,consumer_final1$Store_type))
        names(consumer_final1$Store_type)
        Consumer_Final[Consumer_Final$Store_type=='Flagship store',"prod_cat"]
        consumer_final1[consumer_final1$Store_type=='Flagship store',"prod_cat"==c(Electronics,Clothing)]
        consumer_final1[consumer_final1$Store_type=='Flagship store',"prod_cat"==c('Electronics','Clothing')]
        View(consumer_final1[consumer_final1$Store_type=='Flagship store',"prod_cat"==c('Electronics','Clothing')])
        table(consumer_final1[consumer_final1$Store_type=='Flagship store'])
        table(consumer_final1[consumer_final1$Store_type=='Flagship store','prod_cat'])
        table(consumer_final1[consumer_final1$Store_type=='Flagship store','prod_cat'=='Electronics'])
        View(table(consumer_final1[consumer_final1$Store_type=='Flagship store','prod_cat'))
        View(table(consumer_final1[consumer_final1$Store_type=='Flagship store','prod_cat']))
        table(consumer_final1$Store_type=='Flagship store')
        table(consumer_final1$Store_type=='Flagship store',consumer_final1$prod_cat=='Electronics')
        View(consumer_final1$Store_type=='Flagship store',consumer_final1$prod_cat=='Electronics')
        View(table(consumer_final1$Store_type=='Flagship store',consumer_final1$prod_cat=='Electronics')_
             View(table(consumer_final1$Store_type=='Flagship store',consumer_final1$prod_cat=='Electronics'))
             View(table(consumer_final1[consumer_final1$Store_type=='Flagship store','prod_cat','total_amt']))
             View(table(consumer_final1[consumer_final1$Store_type=='Flagship store','total_amt']))
             library(dplyr)
             names(consumer_final1)
             filter(consumer_final1,Store_type=Flagship store)
             filter(consumer_final1,Store_type="Flagship store")
             filter(consumer_final1,Store_type='Flagship store')
             filter(consumer_final1,Store_type=='Flagship store')
             head(consumer_final1)
             filter(consumer_final1,Store_type=='Flagship store',prod_cat=='Electronics,Clothing')
             filter(consumer_final1,Store_type=='Flagship store',prod_cat=='Electronics,Clothing',total_amt)
             head(consumer_final1)
             View(filter(consumer_final1,Store_type=='Flagship store',prod_cat=='Electronics,Clothing',total_amt))
             View(filter(consumer_final1,Store_type=='Flagship store'))
             View(filter(consumer_final1,Store_type=='Flagship store',prod_cat=='Electronics'))
             View(filter(consumer_final1,Store_type=='Flagship store',prod_cat=='Electronics,Clothing'))
             View(filter(consumer_final1,Store_type=='Flagship store',prod_cat=='c(Electronics,Clothing'))
             View(filter(consumer_final1,Store_type=='Flagship store',prod_cat=='Electronics',prod_cat=='Clothing'))
             View(filter(consumer_final1,Store_type=='Flagship store',prod_cat==c('Electronics','Clothing')))
             a=filter(consumer_final1,Store_type=='Flagship store',prod_cat==c('Electronics','Clothing'))
             sum(a$total_amt)
             b=filter(consumer_final1,gender=='M',prod_cat=='Electronics')
             View(b)
             b=filter(consumer_final1,prod_cat=='Electronics')
             View(b)
             b=filter(consumer_final1,prod_cat=='Electronics',Gender=='M')
             View(b)
             sum(b$total_amt)
             sapply(consumer_final1, class)
             fivenum(consumer_final1,na.rm=TRUE)
             data_top10=consumer_final1[1:10,]
             consumer_final1[1:10,]
             consumer_final1[nrow(consumer_final1)-5:nrow(consumer_final1),]
             tail(consumer_final1,5)
             summary(customer$Gender)
             summary(product$prod_cat)
             sum(consumer_final1$total_amt<0)
             consumer_final1[consumer_final1$Gender=='M',"prod_cat"]
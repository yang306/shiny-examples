request<-read.csv("request_update.csv")
zip<-read.csv("zipcode.csv")

unique(request$RequestType)

library(ggplot2)
library(dplyr)
#device
device=request%>%
  filter(RequestSource=="Mobile App")%>%
  group_by(RequestType)%>%
  summarise(count=n())

#request types
requests =request%>%
  group_by(RequestType)%>%
  summarise(sum=n())

str(requests$sum)

ggplot(requests,aes(x=reorder(RequestType,sum),y=sum))+
  scale_y_continuous(breaks=seq(0,550000,30000),
                     labels=seq(0,550000,30000),
                     limits=c(0,580000))+
  geom_bar(stat="identity",fill="pink")+
  geom_text(aes(label=sum),hjust=-0.1) +
  xlab("Request Type")+
  ylab("Sum")+
  coord_flip()+
  theme_classic()+
  ggtitle("Summary of Request Type")
  


ggplot(request,aes(x=Longitude,y=Latitude))+
  geom_point(alpha=0.005)+
  facet_wrap(~RequestType)
#summarise by time
  request$CreatedDate=mdy_hms(request$CreatedDate)
  
  
day=wday(request$CreatedDate,abbr=T,label = T)
hour=hour(request$CreatedDate) 
request=cbind(request,day,hour)
month=month(request$CreatedDate,abbr=T,label=T)
request=cbind(request,month)
# day and hour
dayhour=request%>%
  group_by(day,hour)%>%
  summarise(count=n())
ggplot(dayhour,aes(x=day,y=hour,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")
write.csv(dayhour,"dayhour.csv")

write.csv(apphour,"apphour.csv")
write.csv(hourst,"hourst.csv")
# request source and hour
apphour=request%>%
  group_by(hour,RequestSource)%>%
  summarise(count=n())
ggplot(apphour,aes(x=hour,y=RequestSource,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  theme_classic()+
  ggtitle("Request Source By Hour of the Day")


#month and hour
monthhr=request%>%
  group_by(month,hour)%>%
  summarise(count=n())
ggplot(monthhr,aes(x=factor(month),y=hour,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")


#month and request type
monthst=request%>%
  group_by(month,RequestType)%>%
  summarise(count=n())

ggplot(monthst,aes(x=factor(month),y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  ggtitle("Request Type By Month")+
  theme_classic()+
  xlab("Month")

levels(monthst$month)=c("Jan","Feb","Mar","Apr","May","Jun",
                                     "Jul","Aug","Sep","Oct","Nov","Dec")

levels(monthst$month)
#request and hour
hourst=request%>%
  group_by(hour,RequestType)%>%
  summarise(count=n())
ggplot(hourst,aes(x=RequestType,y=hour,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")

#request and day
wdayst=request%>%
  group_by(day,RequestType)%>%
  summarise(count=n())

ggplot(wdayst,aes(x=day,y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="white",high="black")+
  theme_classic()+
  ggtitle("Request Type By Day of the Week")

write.csv(wdayst,"weekdayst.csv")
levels(wdayst$day)=c("Mon","Tues","Wed","Thurs","Fri","Sat","Sun")
####################### NEW ERA!!!!!!!!!!!!!

request$ZipCode<-as.numeric(as.character(request$ZipCode))

zip$age.group =NULL
for(i in 1:nrow(zip)){
  if(zip$age[i]<25){
    zip$age.group[i]<-"0-25"
  }else if(zip$age[i]>=25&zip$age[i]<35){
    zip$age.group[i]<-"25-35"
  }else if(zip$age[i]>=35&zip$age[i]<45){
    zip$age.group[i]<-"35-45"
  }else if(zip$age[i]>=45&zip$age[i]<55){
    zip$age.group[i]<-"45-55"
  }
  else {
    zip$age.group[i]=">55"
  }
}

zip.request<-merge(request,zip,
                   by.x="ZipCode",
                   by.y="Los.Angeles..CA",
                   all.x=T)

zip.request<-zip.request%>%
  filter(ZipCode!=90095)

zip.request<-zip.request[-2]

table(zip.request$ZipCode)

tableau<-zip.request%>%
  select(ZipCode,CreatedDate,Owner,RequestType,RequestSource,MobileOS,AssignTo,CD,NC,PolicePrecinct,
         Latitude,Longitude,Location,
         day,hour,month,total.income,age,population,bachelor,race,perc,age.group)%>%
  filter(!is.na(MobileOS))%>%
  filter(!is.na(Location))

tableau<-tableau%>%
  filter(ZipCode!=0)%>%
  filter(!is.na(AssignTo))




write.csv(tableau,"finalproject.csv")

zip.income<-zip.request%>%
  group_by(RequestType,total.income)%>%
  summarise(count=n())

ggplot(zip.income,aes(x=total.income,y=count,group=RequestType))+
  geom_point(aes(color=RequestType))






str(zip.request$age)
table(zip.request$age)


zip.req.income<-zip.request%>%
  group_by(RequestType,total.income)%>%
  summarise(count=n())

ggplot(zip.req.income,aes(x=as.factor(total.income),y=count,group=RequestType))+
  geom_line(aes(color=RequestType))


ggplot(zip.req.income,aes(x=factor(total.income),y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="pink",high="black")
  

zip.edu<-zip.request%>%
  group_by(RequestType,bachelor)%>%
  summarise(count=n())

ggplot(zip.edu,aes(x=factor(bachelor),y=RequestType,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="pink",high="black")

# request vs. app

reqst<-request%>%
  filter(!is.na(RequestSource))%>%
  group_by(RequestType,RequestSource)%>%
  summarise(count=n())
  
ggplot(reqst,aes(RequestType,RequestSource,fill=count))+
  geom_tile()+
  scale_fill_gradient(low="pink",high="black")
zip<-read.csv("zipdata.csv")


ggplot(zip.request,aes(x=ZipCode,y=total.income,group=RequestType))+
  geom_point()


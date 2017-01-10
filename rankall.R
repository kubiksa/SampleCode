rankall <- function(outcome, num) {
  data<- read.csv("outcome-of-care-measures.csv",colClasses="character")
  
  #state<-"TX"
  outcome<-"pneumonia"
  num<-"worst"
  
  sts <- unique(data$State) #gives states included in file

  if(! outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")}
  
  if(outcome == "heart attack"){
    column <- 11
  }else if (outcome == "heart failure"){
    column <- 17
  }else{
    column <- 23
  }
  
  datab<- data[c(2,7,column)]  #use name and number and outcome column
  datab_comp <- datab[which(datab[,3] != "Not Available"), ] #still take out Not avaiable(?)
  name<-as.character(datab_comp[,1])
  state<-as.character(datab_comp[,2])
  number<-as.numeric(datab_comp[,3]) 
  
  df<- data.frame(name,state,number)  #data frame name,state,number
  
 
sorted.data<-df[order(df$state,df$number,df$name),] #sorted by state then number then name
sorted.sts<-sts[order(sts)]
#Now need to pick out frames
#for (j in 1:length(name)){
#current.state<-sorted.data[j,2]
#i=1
 # while (sorted.data[i,2]== current.state){
#  rank[j]=i
#  i=i+1
#  }
#}

name.final<-data.frame(hospital=character(), state=character()) #initialize data frame

if (class(num)== "numeric"){
  
  for (i in 1:length(sts)){
    ranking.data<-sorted.data[sorted.data$state==sorted.sts[i],]
    
    name.out<-data.frame(hospital=ranking.data[num,1],state=sorted.sts[i])  
    
    name.final<-rbind(name.final,name.out)
    i=i+1
  }
  
  }else if (num=="best"){
    for (i in 1:length(sts)){
      ranking.data<-sorted.data[sorted.data$state==sorted.sts[i],]
      num=1
      name.out<-data.frame(hospital=ranking.data[num,1],state=sorted.sts[i])  
      
      name.final<-rbind(name.final,name.out)
      i=i+1
    }
    
 }else if (num=="worst"){
   for (i in 1:length(sts)){
     ranking.data<-sorted.data[sorted.data$state==sorted.sts[i],]
     num=nrow(ranking.data)
     name.out<-data.frame(hospital=ranking.data[num,1],state=sorted.sts[i])  
     
     name.final<-rbind(name.final,name.out)
     i=i+1
   }
}else {
    stop("invalid outcome")
  }


return (name.final)
}
#sorted.data<-cbind(sorted.data,rank)  #now have name, state, number, rank
#need to write code that checks for state and then picks out rows with specified rank
#and gives na if it doesn't exist for that state.
#still need to write code which ammends to add NA if state does not exist.
#subset data frame to get output
#http://www.ats.ucla.edu/stat/r/faq/subset_R.htm

#df[,2][1:1] # this returns for the column 2 (B) the value in the row 1
  #ran
  #ranked.data<- data.frame(sorted.data[,1],sorted.data[,2],rank)
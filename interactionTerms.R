interactionTerms<-function(data, power,start){
## Define how many columns we work with (start allows us to start later than 1)   
## Data is ONLY predictors, result variable should be left out.
    
    k<-ncol(data)-(start-1)
    
    for (i in start:k) {
            for (j in i:k) {
                ## Create the new variable
                temp<-data[i]*data[j]
                
                ## Rename it so we know what it is
                names(temp)<-paste(names(data[i]),"*",names(data[j]))
                
                ## append the new column
                data<-cbind(data,temp)
            }
    }
    
return(data)
}
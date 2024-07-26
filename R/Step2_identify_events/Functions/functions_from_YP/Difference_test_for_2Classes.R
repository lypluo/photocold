############################################
#Aims:function used to do the difference test for two datasets
############################################
#now this function has not been used in the analysis->in the future might be use(2019-01-16)
difftest_function<-function(Data1,Data2){
  #Data1 and Data2 are two vectors, which can have different data length
  # Data1<-df.event_sel$comp_var
  # Data2<-df.nonevent_sel$comp_var
  #remove the rows which have NA
  Data1<-Data1[!is.na(Data1)]
  Data2<-Data2[!is.na(Data2)]
  N_Data1<-length(Data1)
  N_Data2<-length(Data2)
  
  ##new datasets used to compare
  New_Data<-list(Data1=Data1,Data2=Data2)
  ##KS.test to test normality->big datasets
  ks_result<-c()
  for(i in 1:2){
    temp<-ks.test(New_Data[[i]],'pnorm',mean=mean(New_Data[[i]]),sd=sd(New_Data[[i]]))  # if p-value>0.05, then the data are normality distribution
    ks_result[i]<-temp$p.value
  }
  # if p-value=NA for ks_test, then set ks_test=0
  if(length(is.na(ks_result))==2){ ## if two p-values are NA, set ks_result<-c(0,0)
    ks_result<-c(0,0)
  }
  
  norm_result<-c()
  n=0
  for(i in 1:1){
    for(j in (i+1):2){
      n=n+1
      if((ks_result[i]>0.05)&(ks_result[j]>0.05)){
        norm_result[n]<-1} ##two variables are both normality distribution
      else
      {norm_result[n]<-0} ##at least one variable is not normality distribution
    }     
  }
  
  ##correlation test
  cor.test_1<-function(x,y){
    result<-cor.test(x,y,use='complete.obs')
    return(result)
  }
  if(N_Data1==N_Data2){
    corr_result<-c()
    n=0
    for(i in 1:1){
      for(j in (i+1):2){
        n=n+1
        temp<-cor.test_1(New_Data[[i]],New_Data[[j]])
        corr_result[n]<-temp$p.value}     ##if p-vale>0.05, then correlation between two varibales  are zero
    }
  }else{
    corr_result<-0
  }
  
  ##variance homogeneity test
  var_result<-c()
  n=0
  for(i in 1:1){
    for(j in (i+1):2){
      n=n+1
      temp<-var.test(New_Data[[i]],New_Data[[j]])
      var_result[n]<-temp$p.value }    ##if p-vale>0.05, then variation of two variables is the same
  }
  
  evaluate_paras<-rbind(norm_result,var_result,corr_result)
  ##variables comparison test
  evaluate_paras<-as.data.frame(evaluate_paras);names(evaluate_paras)<-c('12')
  sign_result<-c()
  n=0
  for(i in  1:1){
    for(j in (i+1):2){
      n=n+1
      if(N_Data1==N_Data2){
        if((evaluate_paras[1,n]==1)&(evaluate_paras[3,n]>0.05)){
          temp<-t.test(New_Data[[i]],New_Data[[j]],paired = FALSE)   ##non-grouped t.test as there is no strong correlation 
          ##/R automatically detect if the variation are homogenous or not, no matter variation homogenous or not, both using t.test
          sign_result[n]<-round(temp$p.value,3)
        }
        if((evaluate_paras[1,n]==1)&(evaluate_paras[3,n]<0.05)){
          temp<-t.test(New_Data[[i]],New_Data[[j]],paired = TRUE)   ##grouped t.test as there is no strong correlation 
          sign_result[n]<-round(temp$p.value,3)
        }
        if((evaluate_paras[1,n]==0)&(evaluate_paras[3,n]>0.05)){
          temp<-wilcox.test(New_Data[[i]],New_Data[[j]],paired = FALSE)   #non-grouped t.test as there is no strong correlation
          sign_result[n]<-round(temp$p.value,3)
        }
        if((evaluate_paras[1,n]==0)&(evaluate_paras[3,n]<0.05)){
          temp<-wilcox.test(New_Data[[i]],New_Data[[j]],paired = TRUE)   #grouped t.test as there is no strong correlation
          sign_result[n]<-round(temp$p.value,3)
        }
      }
      if(N_Data1!=N_Data2){
        temp<-wilcox.test(New_Data[[i]],New_Data[[j]]) #Mann-Whitney test
        sign_result[n]<-round(temp$p.value,3)
      }
    }
  }
  sign_symbol<-c()
  for(i in 1:length(sign_result)){
    if(sign_result[i]<=0.001){sign_symbol[i]<-'***'}
    if((sign_result[i]>0.001)&(sign_result[i]<=0.01)){sign_symbol[i]<-'**'}
    if((sign_result[i]>0.01)&(sign_result[i]<=0.05)){sign_symbol[i]<-'*'}
    if((sign_result[i]>0.05)){sign_symbol[i]<-'ns'}
  }
  sign_sum<-rbind(sign_result,sign_symbol);sign_sum<-as.data.frame(sign_sum)
  names(sign_sum)<-c('y1 vs y2')
  return(sign_sum)
}

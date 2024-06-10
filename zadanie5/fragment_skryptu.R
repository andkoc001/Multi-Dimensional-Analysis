# weryfikacja założeń 
zalozenia_regresja<-function(model){
  if(length(model$coefficients)==1){
    print("Funkcja to stała")
    return(NULL)
  }
  
  zalozenia <- data.frame()
  zalozenia[1,"Rozkład normalny reszt\n(Shapiro-Wilk p-value)"]<-c(round(shapiro.test(model$residuals)$p.value,4))
  zalozenia[1,"Brak heteroskadastyczności\n(Breusch-Pagan p-value)"]<-c(round(lmtest::bptest(model)$p.value,4)) #H0: homoskedastycznosc, H1: heteroskedastycznosc
  
  if(length(model$coefficients) > 2){
    vif_tab<-as.data.frame(t(car::vif(model)))
    colnames(vif_tab)<-paste("VIF:",colnames(vif_tab))
    zalozenia<-bind_cols(zalozenia, vif_tab)
  }
  
  return(zalozenia)
}

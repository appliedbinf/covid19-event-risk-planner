## ---------------------------
##
## COVID19 Event Risk Assessment Planning tool
##
## Aroon Chande <mail@aroonchande.com> <achande@ihrc.com>
## Lavanya Rishsishwar <lrishishwar@ihrc.com>
## Model from Joshua Weitz
## See: https://github.com/jsweitz/covid-19-ga-summer-2020
## ---------------------------

library(shiny)
library(ggplot2)
library(matlab)

pcrit = function(x){0.01/x}
USpop=330*10^6;
n=logspace(1,5,100);
pcrit_val=pcrit(n);
numcrit=pcrit_val*USpop;
sizevec= c(10, 100, 1000, 10000, 100000)
risk_vals = c(0.02,  0.1, 0.5, 0.9)
pcrit_risk_list = list()
for (i in 1:length(risk_vals)){
    pcrit_risk = 1 - (1-risk_vals[i]**(1/n))
    pcrit_risk = pcrit_risk *USpop
    pcrit_risk_list[[i]] = data.frame("risk" = risk_vals[i], "pcrit_risk" = pcrit_risk)
}


ytarget = 100000;
pcrit_label=ytarget/USpop
pcrit_lab_list = list()
for (i in 1:length(risk_vals)){
    nlabel = log(1-risk_vals[i])/log(1-pcrit_label)
    pcrit_lab_list[[i]] = data.frame("risk" = risk_vals[i], "pcrit_lab" = nlabel)
}

# Making the lower left triangle
# % Bad patch
# pcrit_bad = @(x) 0.9./x;
# xblock = [10^5 2*10^5 2*10^5 10 10^5];
# yblock = [10 10 pcrit(10^5)*USpop pcrit(10^1)*USpop 10];
# 
# ylim([10^4 3*10^7]);
# tmpt=text(12,30000,{'Less than';'1\% chance of';'COVID-19';'positive attendee';'at the event'});
# set(tmpt,'interpreter','latex','fontsize',18);


nvec = c(8000000, 400000, 2000000) 
risk_vals_list = list()
for (i in 1:length(nvec)){
    p_equiv = nvec[i]/USpop;
    risk_vals_I = round(100*(1 - (1-p_equiv)**sizevec), 2)
    risk_vals_list[[i]] = data.frame("nvec" = nvec[i], "risk" = risk_vals_I)

}

pcrit.df = do.call(rbind.data.frame, pcrit_risk_list)
pcrit_lab.df = do.call(rbind.data.frame, pcrit_lab_list)
risk.df = do.call(rbind.data.frame, risk_vals_list)

calc_risk = function(I,n){
    p_I = I/USpop
    1-(1-p_I)**n
}

calc_risk(400000, 700)


ylimits = c(10**4, 3*10**7)
xblock = c(10, 100, 1000, 10**4, 10**5)
yblock = c(10**5, 4*10**5, 10**6, 2*10**6, 8*10**6)

shinyServer(function(input, output, session) {

    ggplot() + 
        geom_line(aes())
   

})


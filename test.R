for(i in 1:nrow(data))
        if(data$cropdmg.exp[i]=="H"){
                data$cropdmg.exp.n[i] <- 100
        } else {
                if(data$cropdmg.exp[i]=="K"){
                        data$cropdmg.exp.n[i] <- 1000
                } else {
                        if(data$cropdmg.exp[i]=="M"){
                                data$cropdmg.exp.n[i] <- 1000000
                        } else {
                                if(data$cropdmg.exp[i]=="B"){
                                        data$cropdmg.exp.n[i] <- 1000000000
                                }
}}}
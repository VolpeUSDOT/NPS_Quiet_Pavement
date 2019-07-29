library(dplyr)


predictors <- read.csv('C:/Quiet_Pavement_Local/MANOVA_Predictors.csv')
LZFeq <- read.csv('C:/Quiet_Pavement_Local/MANOVA_LZFeq.csv')
LZFMax <- read.csv('C:/Quiet_Pavement_Local/MANOVA_LZFMax.csv')

autos_only_predictors = filter(predictors,vehtype == 1)


LZFeqResponse = merge(predictors,LZFeq) %>% filter(vehtype == 1)

Feq.man.chip <- manova(cbind(X63.0,X80.0,X100.0,X125.0,X160.0,X200.0,X250.0,X315.0,X400.0,X500.0,
                        X630.0,X800.0,X1000.0,X1250.0,X1600.0,X2000.0,X2500.0,X3150.0,X4000.0,X5000.0,
                        X6300.0,X8000.0,X10000.0,X12500.0,X16000.0,X20000.0)~
                    Chip.Seal+X3.8in.Chip.Seal+X1.4in.Chip.Seal+pavetemp+speed, data = LZFeqResponse)
summary.aov(Feq.man.chip)

Feq.man.micro <- manova(cbind(X63.0,X80.0,X100.0,X125.0,X160.0,X200.0,X250.0,X315.0,X400.0,X500.0,
                        X630.0,X800.0,X1000.0,X1250.0,X1600.0,X2000.0,X2500.0,X3150.0,X4000.0,X5000.0,
                        X6300.0,X8000.0,X10000.0,X12500.0,X16000.0,X20000.0)~
                    Microsurfacing + Type.II.Microsurfacing + Type.III.Microsurfacing +pavetemp+speed, data = LZFeqResponse)
summary.aov(Feq.man.micro)


for (freq in names(LZFeqResponse[22:47])){
  print("---------------------------------------------------------------------------------------------------")
  print(freq)
  print(anova(lm(LZFeqResponse[[freq]]~pavetemp+speed+X3.8in.Chip.Seal+X1.4in.Chip.Seal+
             Type.II.Microsurfacing+Type.III.Microsurfacing+Chip.Seal+Microsurfacing, data = LZFeqResponse)))
  }
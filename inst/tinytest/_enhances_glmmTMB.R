library(glmmTMB)
Owls <- transform(
  Owls,
  Nest=reorder(Nest,NegPerChick),
  NCalls=SiblingNegotiation,
  FT=FoodTreatment)
fit <- glmmTMB(
  NCalls ~ FT + ArrivalTime + offset(log(BroodSize)) + (1|Nest),
  data=Owls, ziformula=~SexParent, family=poisson)
visreg(fit, 'FT')
visreg(fit, 'FT', type='contrast')
visreg(fit, 'ArrivalTime')
visreg(fit, 'ArrivalTime', type='contrast')

fit <- glmmTMB(
  NCalls ~ FT + ArrivalTime + offset(log(BroodSize)) + (1|Nest),
  data=Owls, family=poisson)
visreg(fit, 'FT')
visreg(fit, 'FT', type='contrast')
visreg(fit, 'ArrivalTime')
visreg(fit, 'ArrivalTime', type='contrast')

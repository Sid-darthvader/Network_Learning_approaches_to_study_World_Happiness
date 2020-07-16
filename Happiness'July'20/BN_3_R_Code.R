library(gRain)
library(bnlearn)
library(lattice)
library(gridExtra)
#junction <- compile(as.grain(bn.fit(learned,df)))
junction <- compile(as.grain(bn.fit(avg.raw.full,df)))
querygrain(junction, nodes = "Life.Ladder")$Life.Ladder

learned_consensus = bn.fit(avg.raw.full,df)

jsex <- setEvidence(junction, nodes = "Log.GDP.per.capita", states = "high")
querygrain(jsex, nodes = "Democratic.Quality")$Democratic.Quality




jsex1 <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "low")
querygrain(jsex1, nodes = "Healthy.life.expectancy.at.birth")$Healthy.life.expectancy.at.birth

jsex2 <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "medium")
querygrain(jsex2, nodes = "Healthy.life.expectancy.at.birth")$Healthy.life.expectancy.at.birth

jsex3 <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "high")
querygrain(jsex3, nodes = "Healthy.life.expectancy.at.birth")$Healthy.life.expectancy.at.birth





p1 <- barchart(querygrain(jsex1, nodes = "Freedom.to.make.life.choices")$Freedom.to.make.life.choices, main = "Low Confidence\n in Government", xlim = c(0,1),xlab="Pr(Life Freedom|Govt.Conf.='Low')")
p2 <- barchart(querygrain(jsex2, nodes = "Freedom.to.make.life.choices")$Freedom.to.make.life.choices, main = "Medium Confidence\n in Government", xlim = c(0,1),xlab="Pr(Life Freedom|Govt.Conf.='Low')")
p3 <- barchart(querygrain(jsex3, nodes = "Freedom.to.make.life.choices")$Freedom.to.make.life.choices, main = "High Confidence\n in Government", xlim = c(0,1),xlab="Pr(Life Freedom|Govt.Conf.='Low')")
grid.arrange(p1, p2, p3, ncol = 3)




jsex1 <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "low")
jsex2 <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "medium")
jsex3 <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "high")
p1 <- barchart(querygrain(jsex1, nodes = "Freedom.to.make.life.choices")$Freedom.to.make.life.choices, main = "Low Confidence\nin Govt.", xlim = c(0,1),xlab="Pr(Life Freedom|Conf.in.Govt='Low')")
p2 <- barchart(querygrain(jsex2, nodes = "Freedom.to.make.life.choices")$Freedom.to.make.life.choices, main = "Medium Confidence\nin Govt.", xlim = c(0,1),xlab="Pr(Life Freedom|Conf.in.Govt='Medium')")
p3 <- barchart(querygrain(jsex3, nodes = "Freedom.to.make.life.choices")$Freedom.to.make.life.choices, main = "High Confidence\nin Govt.", xlim = c(0,1),xlab="Pr(Life Freedom|Conf.in.Govt='High')")
grid.arrange(p1, p2, p3, ncol = 3)

querygrain(jsex1, nodes = "Freedom.to.make.life.choices")
querygrain(jsex2, nodes = "Freedom.to.make.life.choices")
querygrain(jsex3, nodes = "Freedom.to.make.life.choices")



bn.fit.barchart(learned_consensus$Life.Ladder, main = "Life Ladder",xlab = "Pr(Life Ladder | Social Support,Healthy Life Expectancy)", ylab = "")


jres <- setEvidence(junction, nodes = "Confidence.in.national.government", states = "low")
querygrain(jres, nodes = "Life.Ladder")$Life.Ladder



jsex <- setEvidence(junction, nodes = "Log.GDP.per.capita", states = "low")
querygrain(jsex, nodes = "Healthy.life.expectancy.at.birth")$Healthy.life.expectancy.at.birth
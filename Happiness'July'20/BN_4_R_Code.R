library(gRain)
library(Rgraphviz)
library(bnlearn)
str.raw = boot.strength(df, R = 10000, algorithm = "hc")
attr(str.raw, "threshold")
avg.raw.full = averaged.network(str.raw)
gR = strength.plot(avg.raw.full, str.raw, shape = "rectangle",main = 'World Happiness Bayesian Network averaged from 10000 networks')
nodeRenderInfo(gR)$fill = "lightblue"
edgeRenderInfo(gR)$col = "grey"
nodeRenderInfo(gR)$fill = "lightblue"
renderGraph(gR)

############ Probablistic Analysis peforming Inference   #############




#,sub='Consensus Model by learning 10000 Bayesian networks and keeping the arcs that appear at least ≈ 50% of the time')
nrow(str.raw[with(str.raw, strength > 0.50 & direction > 0.50), ])
nrow(str.raw[with(str.raw, strength > 0.90 & direction > 0.50), ])
min(str.raw[with(str.raw, strength > 0.50 & direction > 0.50), "direction"])
avg.raw.simpler = averaged.network(str.raw, threshold = 0.85)
strength.plot(avg.raw.simpler, str.raw, shape = "rectangle",main = 'Averaged Bayesian Network on World Happiness (Arcs having >85% strength)')
####

fitted.raw.full = bn.fit(avg.raw.full, df)
sim = cpdist(fitted.raw.full, nodes = c("Social.support", "Life.Ladder"), n = 10^4)#,evidence = (Log.GDP.per.capita='high'))
plot(sim, col = "grey")
abline(v = 0, col = 2, lty = 2, lwd = 2)
abline(h = 0, col = 2, lty = 2, lwd = 2)
abline(coef(lm(dPPPM ~ dCoGo, data = sim)), lwd = 2)
library(bnlearn)
library(Rgraphviz)
library(gRain)
df = Discrete_BN
names(df)[names(df) == "gini of household income reported in Gallup, by wp5-year"] <- "GINI(Household Income)"
names(df)
df = data.frame(df)
write.csv(df,'BN_df.csv')
learned <- hc(df)
modelstring(learned)
score(learned, data = df, type = "bic")
learned2 <- hc(df, score = "bde")
score(learned2,data=df)
library(Rgraphviz)
graphviz.plot(highlight = c(),shape = 'ellipse',learned,main = 'Bayesian Network learned on World Happiness Data')
arc_str_df = data.frame(arc.strength(learned, data = df, criterion = "mi"))
df_bic = data.frame(arc.strength(learned, data = df, criterion = "bic"))

fitted = bn.fit(learned,df)

graphviz.chart(fitted, type = "barprob", layout = "dot", draw.levels = TRUE,
               grid = FALSE, scale = c(0.75, 1.1), col = "darkblue",
               text.col = "black", bar.col = "darkblue", main = "Bayesian Network(with Marginal Probabilities) learned on World Happiness Data",
               sub = NULL, bg = "azure" ,strip.bg = "lightskyblue")
# GeNIe style.
graphviz.chart(learned, col = "darkblue", bg = "azure", bar.col = "darkblue")

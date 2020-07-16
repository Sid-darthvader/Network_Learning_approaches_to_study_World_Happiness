learned_mi_1 = aracne(df, whitelist = NULL, blacklist = NULL, mi = 'mi', debug = FALSE)
learned_mi_2 = chow.liu(df, whitelist = NULL, blacklist = NULL, mi = 'mi', debug = FALSE)

fitted_mi_1 = bn.fit(learned_mi_1,df)
fitted_mi_2 = bn.fit(learned_mi_1,df)

learned_iamb=iamb(df, cluster = NULL, whitelist = NULL, blacklist = NULL, test = NULL,
     alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE, undirected = FALSE)

learned_mmpc = mmpc(df, cluster = NULL, whitelist = NULL, blacklist = NULL, test = NULL,
     alpha = 0.05, B = NULL, max.sx = NULL, debug = FALSE, undirected = TRUE)
learned_tabu = tabu(df, start = NULL, whitelist = NULL, blacklist = NULL, score = NULL,
                    debug = FALSE, tabu = 10, max.tabu = 9, max.iter = Inf, maxp = Inf, optimized = TRUE)
modelstring(learned_tabu)
fitted_tabu = bn.fit(learned_tabu,df)

graphviz.chart(learned_mmpc, type = "barprob", layout = "dot", draw.levels = TRUE,
               grid = FALSE, scale = c(0.75, 1.1), col = "darkblue",
               text.col = "black", bar.col = "darkblue", main = "Bayesian Network(with Marginal Probabilities) learned on World Happiness Data",
               sub = NULL, bg = "azure" ,strip.bg = "lightskyblue")

graphviz.chart(fitted_tabu, type = "barprob", layout = "dot", draw.levels = TRUE,
               grid = FALSE, scale = c(0.75, 1.1), col = "darkblue",
               text.col = "black", bar.col = "darkblue", main = "Bayesian Network(with Marginal Probabilities) learned on World Happiness Data",
               sub = NULL, bg = "azure" ,strip.bg = "lightskyblue")

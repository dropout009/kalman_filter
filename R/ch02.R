library(tidyverse)
library(KFAS)

weight = scan("data/Weight.dat")

df = tibble(weight = weight) %>% 
    mutate(time  = row_number())
weight = weight %>% ts()

mod = SSModel(weight ~ SSMtrend(1, Q = NA), H = NA)
mod

fit = fitSSM(mod, numeric(2), method = "BFGS")
fit

kfs = KFS(fit$model)
kfs

N = length(weight)

a_filter =  kfs$a[N]
P_filter = kfs$P[,, N] - fit$model$Q

a_filter_conf = cbind(a_filter + sqrt(P_filter) * 1.96, a_filter - sqrt(P_filter) * 1.96)
alpha_hat_conf = predict(fit$model, interval = "confidence", level = 0.95)


df = alpha_hat_conf %>% 
    as_data_frame() %>% 
    bind_cols(df)



df %>% 
    ggplot(aes(time, weight)) +
    geom_line(aes(y = lwr), linetype= 2) +
    geom_line(aes(y = upr), linetype= 2) +
    geom_line(aes(y = fit), size = 1) + 
    geom_point(color = "royalblue", size = 2)


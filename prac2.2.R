library("lmtest")
library("GGally") 
library("car")

data = mtcars

model = lm(mpg ~ wt + qsec + hp + drat, data)
summary(model)

# Число степеней свободы в моделе df =  27 - 4 = 23
t_critical = qt(0.975, df = 23)

#ошибки коэффициентов
Error_Intercept = 10.3154
Error_wt = 0.8822
Error_qsec = 0.4328
Error_hp = 0.0147
Error_drat = 1.2169

#доверительные интервалы:
print(paste("Доверительный интервал Intercept: [", model$coefficients[1] - t_critical * Error_Intercept, 
            ",", model$coefficients[1] + t_critical * Error_Intercept, "]"))

print(paste("Доверительный интервал wt: [", model$coefficients[2] - t_critical * Error_wt, 
            ",", model$coefficients[2] + t_critical * Error_wt, "]"))

print(paste("Доверительный интервал qsec: [", model$coefficients[3] - t_critical * Error_qsec, 
            ",", model$coefficients[3] + t_critical * Error_qsec, "]"))

print(paste("Доверительный интервал hp: [", model$coefficients[4] - t_critical * Error_hp, 
            ",", model$coefficients[4] + t_critical * Error_hp, "]"))

print(paste("Доверительный интервал drat: [", model$coefficients[5] - t_critical * Error_drat, 
            ",", model$coefficients[5] + t_critical * Error_drat, "]"))



"Доверительный интервал Intercept: [ -2.07933469260419 , 40.5987267364291 ]"
# 0 попадает можно отвергнуть статистическую гипотезу о том, что коэффициент равен 0
"Доверительный интервал wt: [ -5.53270228727505 , -1.88276279945168 ]"
# 0 не попадает невозможно отвергнуть статистическую гипотезу о том, что коэффициент равен 0
"Доверительный интервал qsec: [ -0.367771734119545 , 1.42285829345918 ]"
# 0 попадает невозможно отвергнуть статистическую гипотезу о том, что коэффициент равен 0
"Доверительный интервал hp: [ -0.0482445728849236 , 0.0125739608613964 ]"
#0 попадает невозможно отвергнуть статистическую гипотезу о том, что коэффициент равен 0
"Доверительный интервал drat: [ -0.860250524352061 , 4.17444836788582 ]"
#0 попадает невозможно отвергнуть статистическую гипотезу о том, что коэффициент равен 0

# 3. Доверительный интервал для одного прогноза (p = 95%, Fertility = 20, Catholic = 10, Agriculture = 10).

new.data = data.frame(wt = 20, qsec = 10, hp = 10, drat = 10)
predict(model, new.data, interval = "confidence")
# Доверительный интервал (-79.74025 13.28647)

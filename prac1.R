library("lmtest")
library("GGally")

data = swiss

data

"Пункт 1. Оцените среднее значение, дисперсию и СКО переменных, указанных во втором и 
третьем столбце (Catholic и Fertility)"

#среднее значение:
print(paste("среднее значение Catholic =",  mean(data$Catholic)))
print(paste("среднее значение Fertility =",  mean(data$Fertility)))
#дисперсия
print(paste("дисперсия Catholic =",  var(data$Catholic)))
print(paste("дисперсия Fertility =",  var(data$Fertility)))
#СКО
print(paste("СКО Catholic =",  sd(data$Catholic)))
print(paste("СКО Fertility =",  sd(data$Fertility)))

"Пункт 2. Постройте зависимости вида y = a + bx, где y – объясняемая переменная, x –
регрессор."
model1 = lm(Catholic~Fertility, data)
model2 = lm(Catholic~Examination, data)

model1 # -67.441 + 1.548
model2 # 90.514 - 2.994

"Пункт 3. Оцените, насколько «хороша» модель по коэффициенту детерминации R^2"
summary(model1) # 0.215 R^2 относительно низкий, модель плохая
summary(model2) # 0.328 R^2 = 32% модель относительно плохая 

"Пункт 4. Оцените, есть ли взаимосвязь между объясняемой переменной и объясняющей
переменной"
# В model1 регрессор плохо описывает объясняемую переменную
# В model2 регрессор хорошо описывает объясняемую переменную



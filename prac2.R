library("lmtest")
library("GGally")
library("car")

data = mtcars

# ������� ������
data


# mpg ~ wt, qsec, hp, drat
pairwise_comparison_1 = lm(wt~qsec, data)
summary(pairwise_comparison_1) # ����������� ���

pairwise_comparison_1 = lm(wt~hp, data)
summary(pairwise_comparison_1) # R^2 = 43%

pairwise_comparison_1 = lm(wt~drat, data)
summary(pairwise_comparison_1) # R^2 = 50%

pairwise_comparison_1 = lm(qsec~hp, data)
summary(pairwise_comparison_1) # R^2 = 50%

pairwise_comparison_1 = lm(qsec~hp, data)
summary(pairwise_comparison_1) # R^2 = 50%

pairwise_comparison_1 = lm(qsec~drat, data)
summary(pairwise_comparison_1) # R^2 = 25% ��������

# 2. �������� �������� ������ � ������ �
model = lm(mpg ~ wt + qsec + hp + drat, data)
summary(model) # R=0.8454

model = lm(mpg ~ wt + qsec + hp, data)
summary(model) # R=0.8348

model = lm(mpg ~ wt + qsec, data)
summary(model) # R=0.8264 - ������ �� ����������


# 3. ��������� ������ � ������ ��������� �����������, �������������� ��������, ��� ��� �������� �����������

model1 = lm(I(log(mpg)) ~ I(log(wt)) + I(log(qsec)), data)
model
vif(model1) #
summary(model1) # R ~ 87%

model2 = lm(mpg ~ I(log(wt)) + I(log(qsec)), data)     
vif(model2) # 
summary(model2) # R ~ 88%  ������ ����� ������

model3 = lm(mpg ~ I(log(wt)) + qsec, data)     
vif(model3) # 
summary(model3) # R ~ 88%

model3 = lm(mpg ~ wt + I(log(qsec)), data)     
vif(model3) #
summary(model) # R ~ 83%

# 4. ��������� ������ � ������ ������������ ������������ ��� �����������, �������������� ��������, ��� ��� �������� �����������

model11 = lm(mpg ~qsec + I(wt^2) + I(qsec^2) + I(wt*qsec), data) 
model11
vif(model11) # ������� ���������� � ������������ �-�� vif

model22 = lm(mpg ~qsec + I(wt^2) + I(wt*qsec), data)
vif(model22) # ������� ���������� � ������������ �-�� vif
 
model33 = lm(mpg ~qsec + I(wt*qsec), data)
vif(model33) # ������� ���������� � ������������ �-�� vif

model44 = lm(mpg ~qsec + I(wt*qsec), data)
vif(model44)
#������� ����������� �-� vif<2
#model44 �������� ����� ������
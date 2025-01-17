library("lmtest")
library("GGally")

data = swiss

data

"����� 1. ������� ������� ��������, ��������� � ��� ����������, ��������� �� ������ � 
������� ������� (Catholic � Fertility)"

#������� ��������:
print(paste("������� �������� Catholic =",  mean(data$Catholic)))
print(paste("������� �������� Fertility =",  mean(data$Fertility)))
#���������
print(paste("��������� Catholic =",  var(data$Catholic)))
print(paste("��������� Fertility =",  var(data$Fertility)))
#���
print(paste("��� Catholic =",  sd(data$Catholic)))
print(paste("��� Fertility =",  sd(data$Fertility)))

"����� 2. ��������� ����������� ���� y = a + bx, ��� y � ����������� ����������, x �
���������."
model1 = lm(Catholic~Fertility, data)
model2 = lm(Catholic~Examination, data)

model1 # -67.441 + 1.548
model2 # 90.514 - 2.994

"����� 3. �������, ��������� ������� ������ �� ������������ ������������ R^2"
summary(model1) # 0.215 R^2 ������������ ������, ������ ������
summary(model2) # 0.328 R^2 = 32% ������ ������������ ������ 

"����� 4. �������, ���� �� ����������� ����� ����������� ���������� � �����������
����������"
# � model1 ��������� ����� ��������� ����������� ����������
# � model2 ��������� ������ ��������� ����������� ����������



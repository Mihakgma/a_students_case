# ��������� ��������� ��������� ����� �� ������ 2000 - ��� ������������ ����
set.seed(2000)

# ������� ������� �������� �������� �� ������� (��� += 1 (� ������� ������������), ��� = ��� + 1 (� ������� ������������) � �.�.)

jan <- rep(c(0,0,0,0,0,0,0,0,0,0,0,1), 25)
feb <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0), 25)
mar <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0), 25)
apr <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0), 25)
may <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0), 25)
jun <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0), 25)
jul <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0), 25)
aug <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0), 25)
sep <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0), 25)
oct <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0), 25)
nov <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0), 25)
dec <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0), 25)

# ������� ��� �������� � ������� ��������� (N = 300) �� ������ ������������ ������������� (runif), ������ ����������� � ������������ �������� ���� ��������
birth <- round(runif(n = 300, min = 1975, max = 1982),digits = 0)

# ������������� � ��� �������� ����� � 2000 ����. ����������� ������� ������� �� 2000 ��� ��������
years <- 2000 - birth

# ������������� ��� �������� (birth) � �������� (years) �������� ������������ ����� ������-����� ���������� �� �����������
shapiro.test(birth)
shapiro.test(years)

jan_years <- years
feb_years <- jan_years + feb
mar_years <- feb_years + mar
apr_years <- mar_years + apr
may_years <- apr_years + may
jun_years <- may_years + jun
jul_years <- jun_years + jul
aug_years <- jul_years + aug
sep_years <- aug_years + sep
oct_years <- sep_years + oct
nov_years <- oct_years + nov
dec_years <- nov_years + dec

# ������� ��������� � ������ ���������� ��������� �� ������� �������������
df_years <- cbind(birth, jan_years,feb_years,mar_years,apr_years,may_years,jun_years,jul_years,aug_years,sep_years,oct_years,nov_years,dec_years)
View(df_years)
# ����������� ������ ������������ ���������� ������� (����������.)
coef_r <- abs(apply(df_years,2,function(x) cor.test(x,birth)$estimate))
# ����������� ������ ������������ ���������� �������� (������������.)
coef_r_1 <- abs(apply(df_years,2,function(x) cor.test(x,birth, method = 'spearman')$estimate))

# ������ ������ ����������� ������ ������������ ���������� ������� �� ������ �������������
plot(y = coef_r, x = seq(1:12), xlab = '����� �������������', ylab = '������ ������������ ���������� �������')

# ������ ������ ����������� ������ ������������ ���������� �������� �� ������ �������������
plot(y = coef_r_1, x = seq(1:12), xlab = '����� �������������', ylab = '������ ������������ ���������� ��������')

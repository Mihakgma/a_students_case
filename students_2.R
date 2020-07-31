# Фиксируем генератор случайных чисел на уровне 2000 - для повторимости кода
set.seed(2000)

# создаем вектора прибавки возраста по месяцам (янв += 1 (у каждого двенадцатого), фев = янв + 1 (у каждого двенадцатого) и т.д.)

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

# создаем год рождения и возраст студентов (N = 300) по закону равномерного распределения (runif), задаем минимальное и максимальное значение года рождения
birth <- round(runif(n = 300, min = 1975, max = 1982),digits = 0)

# анкетирование у нас проходит якобы в 2000 году. Высчитываем возраст отнимая от 2000 год рождения
years <- 2000 - birth

# распределение дат рождения (birth) и возраста (years) согласно проведенному тесту Шапиро-Уилка отличается от нормального
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

# Создаем датафрейм с новыми возрастами студентов по месяцам анкетирования
df_years <- cbind(birth, jan_years,feb_years,mar_years,apr_years,may_years,jun_years,jul_years,aug_years,sep_years,oct_years,nov_years,dec_years)
View(df_years)
# Вытаскиваем модуль коэффициента корреляции Пирсона (параметрич.)
coef_r <- abs(apply(df_years,2,function(x) cor.test(x,birth)$estimate))
# Вытаскиваем модуль коэффициента корреляции Спирмена (непараметрич.)
coef_r_1 <- abs(apply(df_years,2,function(x) cor.test(x,birth, method = 'spearman')$estimate))

# Строим график зависимости модуля коэффициента корреляции Пирсона от месяца анкетирования
plot(y = coef_r, x = seq(1:12), xlab = 'Месяц анкетирования', ylab = 'Модуль коэффициента корреляции Пирсона')

# Строим график зависимости модуля коэффициента корреляции Спирмена от месяца анкетирования
plot(y = coef_r_1, x = seq(1:12), xlab = 'Месяц анкетирования', ylab = 'Модуль коэффициента корреляции Спирмена')

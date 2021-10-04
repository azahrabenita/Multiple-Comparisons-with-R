#This final project was done by:
#Azahra Benita, Intan Lucky Rahayu and Delia Rizki


### Kode R untuk model Rancangan Acak Kelompok Lengkap (RACL) atau Randomized Complete Block Design (RCBD)
### Kode ini meliputi 3 tahapan analisis, yaitu:
### 1. memodelkan RACL dengan ANOVA
### 2. komparasi setiap pasangan mean treatment
### 3. diagnosis model

# misalkan ada 4 kelompok/blok dan 4 treatment
# variabel respons untuk masing-masing blok
blok1 <- c(9.3,9.4,9.2,9.7)
blok2 <- c(9.4,9.3,9.4,9.6)
blok3 <- c(9.6,9.8,9.5,10.0)
blok4 <- c(10.0,9.9,9.7,10.2)
# menggabungkan semua pengamatan di setiap blok ke dalam satu vektor
y <- c(blok1, blok2, blok3, blok4)

# membuat variabel independent untuk identifikasi blok dan treatment
blok <- rep(1:4, each = 4)
treat <- rep(1:4, times = 4)

# menggabungkan variabel respon dan variabel independent ke dalam satu data frame
df <- data.frame(y, blok, treat)

# re-format variabel independent menjadi faktor / variabel kategorik
df$blok <- as.factor(df$blok)
df$treat <- as.factor(df$treat)


## ANOVA untuk model RACL
racl <- aov(y ~ blok + treat, data = df)
summary(racl)
# kesimpulan: karena p-value (0.00871) lebih kecil dari 0.05 maka tolak H0 artinya ada perbedaan treatment

# cara menghitung p-value dengan perintah df()
1-pf(14.44,3,9)
# cara menghitung titik kritis
qf(0.95,3,9)
# karena F0=14.44 lebih besar dari Fk=3.86 maka tolak H0 artinya paling tidak 1 treatment mean-nya berbeda dari yang lain
# karena H0 ditolak maka bisa dilakukan uji contrast dengan cara yang sama dengan model 1 faktor


## Karena uji ANOVA signifikan maka bisa dilakukan komparasi semua pasangan treatment, yaitu
## H0: miu i = miu j vs H1: miu i != miu j untuk semua i tidak sama dengan j
## bisa menggunakan metode contrast Scheffe dalam bentuk C= miu i - miu j untuk semua i tidak sama dengan j
## namun metode Scheffe dipandang kurang efisien untuk menguji semua pasangan treatment
## ada metode lain yang memang didesain secara khusus untuk membandingkan semua pasangan treatment
## salah satunya adalah metode Tukey
## sebagai contoh jika ada 3 treatment maka ada 3 pasangan treatment yang diuji
## atau 4 treatment maka ada 6 pasangan treatment yang diuji
TukeyHSD(racl)
## berdasarkan hasil metode Tukey treatment yang berbeda secara signifikan adalah
## treatment 4 dan 2 (p-value = 0.0113) serta treatment 4 dan 3 (p-value = 0.0006)



# fitted value / estimasi atau penduga pengamatan ke j pada treatment ke i
fv <- racl$fitted.values 

# Diagnosis model
resid <- racl$residuals # residual model

##### cek normalitas
# histogram
hist(resid)
# QQplot
qqnorm(resid)
# shapiro test
qqline(resid)
shapiro.test(resid) 
# --> indikasi normal terpenuhi

#### cek variansi konstan
# plot residual vs. fitted value
plot(resid,fv) # tersebar acak

#### cek independent menggunakan durbin-watson, tidak ada indikasi autokorelasi karena p-value > alpha
library(lmtest)
dwtest(fit)


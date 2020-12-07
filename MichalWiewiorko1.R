
#Zadanie 1

podzielnoscLiczb <- function(a,b)
{
  if(a %% b == 0) return(TRUE)  else return(FALSE)
}

print(podzielnoscLiczb(4,2))

print(podzielnoscLiczb(5,2))

#Zadanie 2

print("Średnia predkosc z calej trasy: ")
print((120 + 90)/2)

# Zadanie 3

x<- c(1,2,1,3,1)
y<- c(1,1,1,1,2)

pearsonCalc <-function(x,y){
  if(length(x) == length(y))
  {
    return( cor(x, y,  method = "pearson", use = "complete.obs"))
  } 
    else return("Lenght of x is not equal to y")
}

pearsonCalc(x,y)

daneTab <- read.table("dane.csv", header = TRUE, sep = ";")

pearsonCalc(daneTab$waga, daneTab$wzrost)

install.packages("ggpubr")
library("ggpubr")

ggscatter(daneTab, x = "waga", y = "wzrost", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Waga [kg]", ylab = "Wzrost[cm]")

# Korelacja 0.979 oznacza ze zmienne waga i wzrost sa ze soba bardzo powiazane. Oznacza to, ze wraz ze wzrotem wagi bedzie rosl 
# wzrost lub wraz ze wzrostem wagi rosnie nam wzrost. 

# Zadanie 4

stworzDataFrame <- function(ile=1){
  columns <- c(strsplit(readline("Podaj kolumny po przecinku "), ",")[[1]])
  df <- data.frame(matrix(0, ncol = length(columns), nrow = ile))
  colnames(df) <- columns
  for(item in names(df)){
    df[item] <- c(strsplit(readline(message("Podaj wektor danych kolumny ", item )), ",")[[1]])
  }
  return(df)
}

stworzDataFrame(3)

# Zadanie 5

sciezka <- "D:/Studia_PJATK/II_Semestr/ADNR/ProjrktyR/R_Training/smogKrakow/"



liczZplikow <- function(sciezka,nazwaKolumny,jakaFunkcja="mean",DlaIluPlikow=1){ 
  
  fileList <- list.files(sciezka)
  
  for (idx in DlaIluPlikow)
  {
    daneTab <- read.table(paste(sciezka,fileList[idx], sep=""), header = TRUE, sep = ",")
  }
  
  result <- switch (jakaFunkcja,
    "mean" = mean(na.omit(daneTab[[nazwaKolumny]])),
    "median" = median(na.omit(daneTab[[nazwaKolumny]])),
    "max" = max(na.omit(daneTab[[nazwaKolumny]])),
    "min" = min(na.omit(daneTab[[nazwaKolumny]])),
    
  )
  return(result)
}

liczZplikow(sciezka=sciezka,nazwaKolumny='X142_humidity',jakaFunkcja='mean',DlaIluPlikow = 2)

liczZplikow(sciezka=sciezka,nazwaKolumny='X142_pm10',jakaFunkcja='max',DlaIluPlikow = 3)

liczZplikow(sciezka=sciezka,nazwaKolumny='X142_pm10',jakaFunkcja='min',DlaIluPlikow = 3)

liczZplikow(sciezka=sciezka,nazwaKolumny='X142_pm10',jakaFunkcja='median',DlaIluPlikow = 1)

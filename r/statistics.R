#====================
# Início
#====================

install.packages("DBI")
install.packages("odbc")
#The Libraries required
install.packages("datarium")
#Packages need to be installed only once
install.packages("caTools")
install.packages("ggplot2") 
install.packages("GGally")
install.packages("corrplot")
install.packages("RColorBrewer")


options(scipen=999)

library(odbc)
sort(unique(odbcListDrivers()[[1]]))

con <- dbConnect(odbc(), Driver = "ODBC Driver 17 for SQL Server", 
                 Server = "eva-tcc.database.windows.net", Database = "DB_EVA", 
                 UID = "mcaron", PWD = "M4saiani", Port = 1433)


eva <- dbGetQuery(con,'
select 
  LL,
  EVA
from final
')

#summary(eva)

#Pairwise plotting technique 1
#plot(eva, col="red", main="Plotting Pairs Against Each Other", lwd = 1000)

#====================
# Regressão Linear
#====================

show(eva)

#creating the model (regressão linear)
Model <- lm(EVA ~ LL, data = eva)
Model
summary(Model)

#====================
# Correlação entre EVA e LL
#====================

res <- cor.test(eva$EVA, eva$LL, 
                method = "pearson")
res


#====================
# Matriz de correlação
#====================

source("http://www.sthda.com/upload/rquery_cormat.r")

eva_matriz <- dbGetQuery(con,'
select 
  WACC,
  KD,
  KE,
  XD,
  XE,
  ROIC,
  INVESTED_CAPITAL AS IC,
  SPREAD
from final
')

eva_matriz <- cor(eva_matriz)

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(eva_matriz)
head(p.mat[, 1:5])

#summary(eva_matriz)


col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(eva_matriz, method = "color", col = col(200),
         type = "upper", order = "original", number.cex = 1.5,
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 90, # Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 100, insig = "blank",
         # hide correlation coefficient on the principal dagonal
         diag = FALSE)



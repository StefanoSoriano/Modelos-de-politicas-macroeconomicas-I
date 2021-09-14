# Modelos de políticas macroeconómicas I

```r
#||||||||||||||||||||||||||||||||||||||||
#||||||||||| @Markdown en OneDrive ||||||
#||||||||||||||||||||||||||||||||||||||||
rutaRmd_OneDrive <- paste("F:/OneDrive/Proyecto_", proyecto,"/",inf[7,5], sep = "")
setwd(rutaRmd_OneDrive)
rmarkdown::render("Markdown.Rmd", "html_document", encoding = "UTF-8")
#||||||||||||||||||||||||||||||||||||||||
#||||||||||| @Markdown en USB |||||||||||
#||||||||||||||||||||||||||||||||||||||||
rutaRmd_disk <- paste(as.character(toupper(inf[7,2])),":/",inf[7,5], sep = "")
setwd(rutaRmd_disk)
rmarkdown::render("Markdown.Rmd", "html_document", encoding = "UTF-8")
#|||||||||||||||||||||||||||||||||||||||||||||||||
#||||||||||| @Markdown Fin del proceso |||||||||||
#|||||||||||||||||||||||||||||||||||||||||||||||||
```

```r
                      #||||||||||||||||||||||||||||||||||||||||
                   #||||||||   PROYECTO MODELOS I     ||||||
                 #||||||||||||||||||||||||||||||||||||||||


proyecto  <- 1

#||| Excel/Java (484) |||
#||||||||||||||||||||||||
#if (!"rJava" %in% rownames(installed.packages())) {
  #install.packages("rJava")
#}
# "C:/Program Files/Java/jdk1.8.0_201" 64 bits
# "C:/Program Files (x86)/Java/jdk1.8.0_201" 32 bits
#java <- "C:\\Program Files\\Java\\jdk1.8.0_201\\"

if (!"xlsx" %in% rownames(installed.packages())) {
  install.packages("xlsx")
}

java <- Sys.getenv("JAVA_HOME")
Sys.setenv(JAVA_HOME = java)

library(xlsx)
#|||||||||||||||||||||||
#|||||||||||||||||||||||


carpeta_proy <- paste("F:/OneDrive/Proyecto_",proyecto,sep = "")
setwd(carpeta_proy)
inf <- read.csv("informe.csv", header = T, stringsAsFactors = F)

archivo_x <- inf[1,6]
archivo_x <- paste(archivo_x, ".csv", sep = "")
archivo_y <- inf[2,6]
archivo_y <- paste(archivo_y, ".csv", sep = "")
archivo_z <- inf[3,6]
archivo_z <- paste(archivo_z, ".csv", sep = "")
archivo_w <- inf[4,6]
archivo_w <- paste(archivo_w, ".csv", sep = "")
archivo_h <- inf[5,6]
archivo_h <- paste(archivo_h, ".csv", sep = "")

var_name_x <- inf[1,3]
var_name_y <- inf[2,3]
var_name_z <- inf[3,3]
var_name_w <- inf[4,3]
var_name_h <- inf[5,3]

imagen <- inf[7,6]

author <- paste0('"',inf[7,1],'"', sep="")


email <- " "

    edo_x <- inf[1,2]
    edo_y <- inf[2,2]
    edo_z <- inf[3,2]
    edo_w <- inf[4,2]
    edo_h <- inf[5,2]
    
iteraciones_x <- inf[1,4]
iteraciones_y <- inf[2,4]
iteraciones_z <- inf[3,4]
iteraciones_w <- inf[4,4]
iteraciones_h <- inf[5,4]

ruta <- paste(as.character(toupper(inf[7,2])),":/",inf[7,5], sep = "")


link <- inf[7,3]
leyenda_link <- inf[7,4]

#||||||||||||||| PowerShell |||||||||||||
#||||||||||||||||||||||||||||||||||||||||
wwwPs <- inf[1,5]
namePs <- inf[7,5]
unidadPs <- paste(as.character(toupper(inf[7,2])),":", sep = "")
rutaPs <- getwd()
#||||||||||||||||||||||||||||||||||||||||

ruta_imagen <- paste(ruta,"/",wwwPs,"/",imagen, sep = "")
arch.var <- 'variables_ls.xlsx'

#||||||||||||||||||||||||||||||||||||||||

date <- Sys.Date()
date <- format(date, format = "%d %B %Y")
date <- paste('"',date,'"', sep = "")

carpeta_proyNf <- paste("F:/OneDrive/Proyecto_",proyecto,"/New folder",sep = "")
setwd(carpeta_proyNf)
rutaDirPs <- getwd()
var_x <- read.csv(archivo_x, header = T, dec = '.', stringsAsFactors = F)
var_x <- na.omit(var_x)
var_y <- read.csv(archivo_y, header = T, dec = '.', stringsAsFactors = F)
var_y <- na.omit(var_y)
var_z <- read.csv(archivo_z, header = T, dec = '.', stringsAsFactors = F)
var_z <- na.omit(var_z)
var_w <- read.csv(archivo_w, header = T, dec = '.', stringsAsFactors = F)
var_w <- na.omit(var_w)
var_h <- read.csv(archivo_h, header = T, dec = '.', stringsAsFactors = F)
var_h <- na.omit(var_h)

if (!"rlang" %in% rownames(installed.packages())) {
    install.packages("rlang")
}
if (!"astsa" %in% rownames(installed.packages())) {
    install.packages("astsa")
}
if (!"tseries" %in% rownames(installed.packages())) {
    install.packages("tseries")
}
if (!"timeSeries" %in% rownames(installed.packages())) {
    install.packages("timesSeries")
}
if (!"fGarch" %in% rownames(installed.packages())) {
    install.packages("fGarch")
}
if (!"forecast" %in% rownames(installed.packages())) {
    install.packages("forecast")
}
if (!"stats" %in% rownames(installed.packages())) {
    install.packages("stats")
}
if (!"zoo" %in% rownames(installed.packages())) {
  install.packages("zoo")
}


library(astsa)
library(tseries)
library(timeSeries)
library(fGarch)
library(stats)
library(zoo)



anhos <- as.numeric(substr(var_x[, 1], 1, 4))
anho.min_x <- min(anhos)
anho <- anho.min_x
anho.max_x <- max(anhos)
mes_x <- cbind(as.numeric(gsub("(.*)/", "", var_x[, 1])))
mes_x <- mes_x[1,]
frec_x <- as.data.frame(anhos)$anhos == anho.min_x + 1
frec_x <- sum(frec_x == TRUE)
(tiempo_x <- cbind(anho.min_x, anho.max_x, mes_x, frec_x))

#### COMPROBANDO PERIODICIDAD ANUAL
    list <- as.list(var_x)
    list <- as.data.frame(list[1])
    list_long_x <- lengths(strsplit(as.character(list[1,1]), ''))

    var_x <- if (list_long_x == 4) {
        var_x <- ts(var_x, start = c(anho.min_x), end = c(anho.max_x))
        var_x <- var_x[, -1]
    } else {
        var_x <- ts(var_x, start = c(anho.min_x, mes_x), frequency = frec_x)
        var_x <- var_x[, -1]
    }

    anual_x <- if (list_long_x == 4) {
        anual_x <- ""
    } else {
        anual_x <- "#"
    }

    difer_anual_x <- if (list_long_x != 4) {
        difer_anual_x <- ""
    } else {
        difer_anual_x <- "#"
    }

var_x <- na.approx(var_x)

n_diff_x <- forecast::ndiffs(var_x, test = c("adf"))

###  En Script adf.test

estaci2Script_x <- if (n_diff_x == 0) {
         print("#")
} else {
         print("")
}

estaciACFScript_x <- if (n_diff_x != 0) {
         print("#")
} else {
         print("")
}

###  En Markdown adf.test

estaciMdw_x <- if (n_diff_x == 0) {
         print("FALSE")
} else {
         print("TRUE")
}

pron <- forecast::auto.arima(var_x)
p_s_x <- pron$arma[1]
d_s_x <- pron$arma[6]
q_s_x <- pron$arma[2]
P_s_x <- pron$arma[3]
D_s_x <- pron$arma[7]
Q_s_x <- pron$arma[4]
S_s_x <- pron$arma[5]


   
####################################################################################################
#################################        y         #################################################
####################################################################################################

anhos <- as.numeric(substr(var_y[, 1], 1, 4))
anho.min_y <- min(anhos)
anho <- anho.min_y
anho.max_y <- max(anhos)
mes_y <- cbind(as.numeric(gsub("(.*)/", "", var_y[, 1])))
mes_y <- mes_y[1,]
frec_y <- as.data.frame(anhos)$anhos == anho.min_y + 1
frec_y <- sum(frec_y == TRUE)
(tiempo_y <- cbind(anho.min_y, anho.max_y, mes_y, frec_y))

#### COMPROBANDO PERIODICIDAD ANUAL
    list <- as.list(var_y)
    list <- as.data.frame(list[1])
    list_long_y <- lengths(strsplit(as.character(list[1, 1]), ''))

    var_y <- if (list_long_y == 4) {
        var_y <- ts(var_y, start = c(anho.min_y), end = c(anho.max_y))
        var_y <- var_y[, -1]
    } else {
        var_y <- ts(var_y, start = c(anho.min_y, mes_y), frequency = frec_y)
        var_y <- var_y[, -1]
    }

    anual_y <- if (list_long_y == 4) {
        anual_y <- ""
    } else {
        anual_y <- "#"
    }

    difer_anual_y <- if (list_long_y != 4) {
        difer_anual_y <- ""
    } else {
        difer_anual_y <- "#"
    }


var_y <- na.approx(var_y)
n_diff_y <- forecast::ndiffs(var_y, test = c("adf"))

###  En Script adf.test

estaci2Script_y <- if (n_diff_y == 0) {
         print("#")
} else {
         print("")
}                           

estaciACFScript_y <- if (n_diff_y != 0) {
         print("#")
} else {
         print("")
}

###  En Markdown adf.test
estaciMdw_y <- if (n_diff_y == 0) {
         print("FALSE")
} else {
         print("TRUE")
}

pron <- forecast::auto.arima(var_y)
p_s_y <- pron$arma[1]
d_s_y <- pron$arma[6]
q_s_y <- pron$arma[2]
P_s_y <- pron$arma[3]
D_s_y <- pron$arma[7]
Q_s_y <- pron$arma[4]
S_s_y <- pron$arma[5]

####################################################################################################
#################################        z         #################################################
####################################################################################################

anhos <- as.numeric(substr(var_z[, 1], 1, 4))
anho.min_z <- min(anhos)
anho <- anho.min_z
anho.max_z <- max(anhos)
mes_z <- cbind(as.numeric(gsub("(.*)/", "", var_z[, 1])))
mes_z <- mes_z[1,]
frec_z <- as.data.frame(anhos)$anhos == anho.min_z + 1
frec_z <- sum(frec_z == TRUE)
(tiempo_z <- cbind(anho.min_z, anho.max_z, mes_z, frec_z))

#### COMPROBANDO PERIODICIDAD ANUAL
    list <- as.list(var_z)
    list <- as.data.frame(list[1])
    list_long_z <- lengths(strsplit(as.character(list[1, 1]), ''))

    var_z <- if (list_long_z == 4) {
        var_z <- ts(var_z, start = c(anho.min_z), end = c(anho.max_z))
        var_z <- var_z[, -1]
    } else {
        var_z <- ts(var_z, start = c(anho.min_z, mes_z), frequency = frec_z)
        var_z <- var_z[, -1]
    }

    anual_z <- if (list_long_z == 4) {
        anual_z <- ""
    } else {
        anual_z <- "#"
    }

    difer_anual_z <- if (list_long_z != 4) {
        difer_anual_z <- ""
    } else {
        difer_anual_z <- "#"
    }


var_z <- na.approx(var_z)
n_diff_z <- forecast::ndiffs(var_z, test = c("adf"))

###  En Script adf.test

estaci2Script_z <- if (n_diff_z == 0) {
         print("#")
} else {
         print("")
}

estaciACFScript_z <- if (n_diff_z != 0) {
         print("#")
} else {
         print("")
}

###  En Markdown adf.test

estaciMdw_z <- if (n_diff_z == 0) {
         print("FALSE")
} else {
         print("TRUE")
}


pron <- forecast::auto.arima(var_z)
p_s_z <- pron$arma[1]
d_s_z <- pron$arma[6]
q_s_z <- pron$arma[2]
P_s_z <- pron$arma[3]
D_s_z <- pron$arma[7]
Q_s_z <- pron$arma[4]
S_s_z <- pron$arma[5]

####################################################################################################
#################################        w         #################################################
####################################################################################################

anhos <- as.numeric(substr(var_w[, 1], 1, 4))
anho.min_w <- min(anhos)
anho <- anho.min_w
anho.max_w <- max(anhos)
mes_w <- cbind(as.numeric(gsub("(.*)/", "", var_w[, 1])))
mes_w <- mes_w[1,]
frec_w <- as.data.frame(anhos)$anhos == anho.min_w + 1
frec_w <- sum(frec_w == TRUE)
(tiempo_w <- cbind(anho.min_w, anho.max_w, mes_w, frec_w))

#### COMPROBANDO PERIODICIDAD ANUAL
    list <- as.list(var_w)
    list <- as.data.frame(list[1])
    list_long_w <- lengths(strsplit(as.character(list[1,1]), ''))

    var_w <- if (list_long_w == 4) {
        var_w <- ts(var_w, start = c(anho.min_w), end = c(anho.max_w))
        var_w <- var_w[, -1]
    } else {
        var_w <- ts(var_w, start = c(anho.min_w, mes_w), frequency = frec_w)
        var_w <- var_w[, -1]
    }

    anual_w <- if (list_long_w == 4) {
        anual_w <- ""
    } else {
        anual_w <- "#"
    }

    difer_anual_w <- if (list_long_w != 4) {
        difer_anual_w <- ""
    } else {
        difer_anual_w <- "#"
    }

var_w <- na.approx(var_w)

n_diff_w <- forecast::ndiffs(var_w, test = c("adf"))

###  En Script adf.test

estaci2Script_w <- if (n_diff_w == 0) {
         print("#")
} else {
         print("")
}

estaciACFScript_w <- if (n_diff_w != 0) {
         print("#")
} else {
         print("")
}

###  En Markdown adf.test

estaciMdw_w <- if (n_diff_w == 0) {
         print("FALSE")
} else {
         print("TRUE")
}

pron <- forecast::auto.arima(var_w)
p_s_w <- pron$arma[1]
d_s_w <- pron$arma[6]
q_s_w <- pron$arma[2]
P_s_w <- pron$arma[3]
D_s_w <- pron$arma[7]
Q_s_w <- pron$arma[4]
S_s_w <- pron$arma[5]


####################################################################################################
#################################        h         #################################################
####################################################################################################


anhos <- as.numeric(substr(var_h[, 1], 1, 4))
anho.min_h <- min(anhos)
anho <- anho.min_h
anho.max_h <- max(anhos)
mes_h <- cbind(as.numeric(gsub("(.*)/", "", var_h[, 1])))
mes_h <- mes_h[1,]
frec_h <- as.data.frame(anhos)$anhos == anho.min_h + 1
frec_h <- sum(frec_h == TRUE)
(tiempo_h <- cbind(anho.min_h, anho.max_h, mes_h, frec_h))

#### COMPROBANDO PERIODICIDAD ANUAL
    list <- as.list(var_h)
    list <- as.data.frame(list[1])
    list_long_h <- lengths(strsplit(as.character(list[1,1]), ''))

    var_h <- if (list_long_h == 4) {
        var_h <- ts(var_h, start = c(anho.min_h), end = c(anho.max_h))
        var_h <- var_h[, -1]
    } else {
        var_h <- ts(var_h, start = c(anho.min_h, mes_h), frequency = frec_h)
        var_h <- var_h[, -1]
    }

    anual_h <- if (list_long_h == 4) {
        anual_h <- ""
    } else {
        anual_h <- "#"
    }

    difer_anual_h <- if (list_long_h != 4) {
        difer_anual_h <- ""
    } else {
        difer_anual_h <- "#"
    }

var_h <- na.approx(var_h)

n_diff_h <- forecast::ndiffs(var_h, test = c("adf"))

###  En Script adf.test

estaci2Script_h <- if (n_diff_h == 0) {
         print("#")
} else {
         print("")
}

estaciACFScript_h <- if (n_diff_h != 0) {
         print("#")
} else {
         print("")
}

###  En Markdown adf.test

estaciMdw_h <- if (n_diff_h == 0) {
         print("FALSE")
} else {
         print("TRUE")
}

pron <- forecast::auto.arima(var_h)
p_s_h <- pron$arma[1]
d_s_h <- pron$arma[6]
q_s_h <- pron$arma[2]
P_s_h <- pron$arma[3]
D_s_h <- pron$arma[7]
Q_s_h <- pron$arma[4]
S_s_h <- pron$arma[5]


############ VARIABLES ###########
#### MARKDOWN
### Title


t1 <- if (edo_x == edo_y) {
    print("m1")  
} else { 
    print("di")
}

t2 <- if (edo_x == edo_z) {
    print("m1")  
} else {
    print("fe")
}

# Título del Markdown personalizado
title <- inf[7,7]
title <- paste('"',title,'"',sep = "") 

# Título del Markdown generado
#title <- if (t1 == t2) {
 #   title <- paste('"',"Actividad ", var_name_x,", ","actividad ", var_name_y, " y ","actividad ", var_name_z, ", de ", edo_x,".",'"',sep = "") 
#} else {
 #  title <- paste('"',"Actividad ", var_name_x, " de ", edo_x, ", ","actividad ", var_name_y, " de ", edo_y, " y ", "actividad ",var_name_z, " de ", edo_z,".",'"',sep = "")
#}





#### algunas variables

 archivo_x <- paste("'", archivo_x, "'", sep = "")
 archivo_y <- paste("'", archivo_y, "'", sep = "")
 archivo_z <- paste("'", archivo_z, "'", sep = "")
 archivo_w <- paste("'", archivo_w, "'", sep = "")
 archivo_h <- paste("'", archivo_h, "'", sep = "")
 
 
var_name_x_p <- gsub("\\s","_",  var_name_x)
var_name_y_p <- gsub("\\s","_",  var_name_y)
var_name_z_p <- gsub("\\s","_",  var_name_z)
var_name_w_p <- gsub("\\s","_",  var_name_w)
var_name_h_p <- gsub("\\s","_",  var_name_h)

periodo_x <-  ifelse(frec_x == 1, "anualidades",frec_x);
periodo_x <-  ifelse(frec_x == 12, "meses",periodo_x);
periodo_x <-  ifelse(frec_x == 6, "bimestres",periodo_x);
periodo_x <-  ifelse(frec_x == 4, "trimestres",periodo_x);
periodo_x <-  ifelse(frec_x == 3, "cuatrimestres",periodo_x)


periodo_y <-  ifelse(frec_y == 1, "anualidades",frec_y);
periodo_y <-  ifelse(frec_y == 12, "meses",periodo_y);
periodo_y <-  ifelse(frec_y == 6, "bimestres",periodo_y);
periodo_y <-  ifelse(frec_y == 4, "trimestres",periodo_y);
periodo_y <-  ifelse(frec_y == 3, "cuatrimestres",periodo_y)


periodo_z <-  ifelse(frec_z == 1, "anualidades",frec_z);
periodo_z <-  ifelse(frec_z == 12, "meses",periodo_z);
periodo_z <-  ifelse(frec_z == 6, "bimestres",periodo_z);
periodo_z <-  ifelse(frec_z == 4, "trimestres",periodo_z);
periodo_z <-  ifelse(frec_z == 3, "cuatrimestres",periodo_z)


periodo_w <-  ifelse(frec_w == 1, "anualidades",frec_w);
periodo_w <-  ifelse(frec_w == 12, "meses",periodo_w);
periodo_w <-  ifelse(frec_w == 6, "bimestres",periodo_w);
periodo_w <-  ifelse(frec_w == 4, "trimestres",periodo_w);
periodo_w <-  ifelse(frec_w == 3, "cuatrimestres",periodo_w)


periodo_h <-  ifelse(frec_h == 1, "anualidades",frec_h);
periodo_h <-  ifelse(frec_h == 12, "meses",periodo_h);
periodo_h <-  ifelse(frec_h == 6, "bimestres",periodo_h);
periodo_h <-  ifelse(frec_h == 4, "trimestres",periodo_h);
periodo_h <-  ifelse(frec_h == 3, "cuatrimestres",periodo_h)

x <- gsub(" ","_",var_name_x)
y <- gsub(" ","_",var_name_y)
z <- gsub(" ","_",var_name_z)
w <- gsub(" ","_",var_name_w)
h <- gsub(" ","_",var_name_h)


#  Eligiendo algoritmo de ajuste ML (Likelihood Model) o CSS (Conditional Square Sum)

methAj_x <- try(arima(var_x, order=c(p_s_x, d_s_x , q_s_x), seasonal = list(order = c(P_s_x,D_s_x,Q_s_x), period = S_s_x),method="ML", include.mean = TRUE), silent =  TRUE)
if (class(methAj_x) == "try-error") {
      methAj_x <- "CSS"
} else {
    methAj_x <- "ML"
}   

methAj_y <- try(arima(var_y, order=c(p_s_y, d_s_y , q_s_y), seasonal = list(order = c(P_s_y,D_s_y,Q_s_y), period = S_s_y),method="ML", include.mean = TRUE), silent =  TRUE)
if (class(methAj_y) == "try-error") {
      methAj_y <- "CSS"
} else {
    methAj_y <- "ML"
}  

methAj_z <- try(arima(var_z, order=c(p_s_z,d_s_z,q_s_z), seasonal = list(order = c(P_s_z,D_s_z,Q_s_z), period = S_s_z),method="ML", include.mean = TRUE), silent =  TRUE)
if (class(methAj_z) == "try-error") {
      methAj_z <- "CSS"
} else {
    methAj_z <- "ML"
}  

methAj_w <- try(arima(var_w, order=c(p_s_w,d_s_w,q_s_w), seasonal = list(order = c(P_s_w,D_s_w,Q_s_w), period = S_s_w),method="ML", include.mean = TRUE), silent =  TRUE)
if (class(methAj_w) == "try-error") {
      methAj_w <- "CSS"
} else {
    methAj_w <- "ML"
}  

methAj_h <- try(arima(var_h, order=c(p_s_h,d_s_h,q_s_h), seasonal = list(order = c(P_s_h,D_s_h,Q_s_h), period = S_s_h),method="ML", include.mean = TRUE), silent =  TRUE)
if (class(methAj_h) == "try-error") {
      methAj_h <- "CSS"
} else {
    methAj_h <- "ML"
}  



###### PATRONES ARIMA
if (p_s_x == 0) {
    ar_x <- ""
} else {
    ar_x <- paste("AR(",p_s_x,")", sep="")
}
if (d_s_x == 0) {
    i_x <- ""
} else {
    i_x <- paste("I(",d_s_x,")", sep="")
}
if (q_s_x == 0) {
     ma_x <- ""
} else { 
    ma_x <- paste("MA(",q_s_x,")", sep="")
}


if (p_s_y == 0) {
     ar_y <- ""
} else {
     ar_y <- paste("AR(",p_s_y,")", sep="")
}
if (d_s_y == 0) {
    i_y <- ""
} else {
    i_y <- paste("I(",d_s_y,")", sep="")
}
if (q_s_y == 0) {
    ma_y <- ""
} else {
    ma_y <- paste("MA(",q_s_y,")", sep="")
}


if (p_s_z == 0) {
      ar_z <- ""
} else {
    ar_z <- paste("AR(",p_s_z,")", sep="")
}
if (d_s_z == 0) {
    i_z <- ""
} else {
    i_z <- paste("I(",d_s_z,")", sep="")
}
if (q_s_z == 0) {
    ma_z <- ""
} else {
    ma_z <- paste("MA(",q_s_z,")", sep="")
}

if (p_s_z == 0) {
      ar_z <- ""
} else {
    ar_z <- paste("AR(",p_s_z,")", sep="")
}
if (d_s_z == 0) {
    i_z <- ""
} else {
    i_z <- paste("I(",d_s_z,")", sep="")
}
if (q_s_z == 0) {
    ma_z <- ""
} else {
    ma_z <- paste("MA(",q_s_z,")", sep="")
}

if (p_s_w == 0) {
      ar_w <- ""
} else {
    ar_w <- paste("AR(",p_s_w,")", sep="")
}
if (d_s_w == 0) {
    i_w <- ""
} else {
    i_w <- paste("I(",d_s_w,")", sep="")
}
if (q_s_w == 0) {
    ma_w <- ""
} else {
    ma_w <- paste("MA(",q_s_w,")", sep="")
}

if (p_s_h == 0) {
      ar_h <- ""
} else {
    ar_h <- paste("AR(",p_s_h,")", sep="")
}
if (d_s_h == 0) {
    i_h <- ""
} else {
    i_h <- paste("I(",d_s_h,")", sep="")
}
if (q_s_h == 0) {
    ma_h <- ""
} else {
    ma_h <- paste("MA(",q_s_h,")", sep="")
}
###### PATRONES SARIMA
sar_x <- paste("AR(",P_s_x,")", sep="")
si_x <- paste("I(",D_s_x,")", sep="")
sma_x <- paste("MA(",Q_s_x,")", sep="")

sar_y <- paste("AR(",P_s_y,")", sep="")
si_y <- paste("I(",D_s_y,")", sep="")
sma_y <- paste("MA(",Q_s_y,")", sep="")

sar_z <- paste("AR(",P_s_z,")", sep="")
si_z <- paste("I(",D_s_z,")", sep="")
sma_z <- paste("MA(",Q_s_z,")", sep="")

sar_w <- paste("AR(",P_s_w,")", sep="")
si_w <- paste("I(",D_s_w,")", sep="")
sma_w <- paste("MA(",Q_s_w,")", sep="")

sar_h <- paste("AR(",P_s_h,")", sep="")
si_h <- paste("I(",D_s_h,")", sep="")
sma_h <- paste("MA(",Q_s_h,")", sep="")


#  Estacionalidad ### UNICODE
patron_ARIMA <- "000"

patron_puro_ar_x <- paste(d_s_x,q_s_x, sep="")
patron_puro_ar_y <- paste(d_s_y,q_s_y, sep="")
patron_puro_ar_z <- paste(d_s_z,q_s_z, sep="")
patron_puro_ar_w <- paste(d_s_w,q_s_w, sep="")
patron_puro_ar_h <- paste(d_s_h,q_s_h, sep="")

patron_puro_ma_x <- paste(p_s_x,d_s_x, sep="")
patron_puro_ma_y <- paste(p_s_y,d_s_y, sep="")
patron_puro_ma_z <- paste(p_s_z,d_s_z, sep="")
patron_puro_ma_w <- paste(p_s_w,d_s_w, sep="")
patron_puro_ma_h <- paste(p_s_h,d_s_h, sep="")

patron_x <- paste(P_s_x,D_s_x,Q_s_x, sep="")
patron_y <- paste(P_s_y,D_s_y,Q_s_y, sep="")
patron_z <- paste(P_s_z,D_s_z,Q_s_z, sep="")
patron_w <- paste(P_s_w,D_s_w,Q_s_w, sep="")
patron_h <- paste(P_s_h,D_s_h,Q_s_h, sep="")

setwd("F:/OneDrive/Proyecto/variables")
UNICODE <- read.csv("unicode.csv", stringsAsFactors = FALSE)

calculo <- UNICODE[1,1]
autocorrelacion <- UNICODE[2,1]
patron <- UNICODE[3,1]
moviles <- UNICODE[4,1]

#### AR()
if (d_s_x == 0 & q_s_x == 0) {
    ar_puro_x <- paste(" por lo que la serie de tiempo sigue un proceso estacionario autorregresivo puro de orden ", p_s_x, sep = "")
} else {
    ar_puro_x <- ""
}

if (d_s_y == 0 & q_s_y == 0) {
    ar_puro_y <- paste(" por lo que la serie de tiempo sigue un proceso estacionario autorregresivo puro de orden ", p_s_y, sep = "")
} else {
    ar_puro_y <- ""
}

if (d_s_z == 0 & q_s_z == 0) {
    ar_puro_z <- paste(" por lo que la serie de tiempo sigue un proceso estacionario autorregresivo puro de orden ", p_s_z, sep = "")
} else {
    ar_puro_z <- ""
}

if (d_s_w == 0 & q_s_w == 0) {
    ar_puro_w <- paste(" por lo que la serie de tiempo sigue un proceso estacionario autorregresivo puro de orden ", p_s_w, sep = "")
} else {
    ar_puro_w <- ""
}

if (d_s_h == 0 & q_s_h == 0) {
    ar_puro_h <- paste(" por lo que la serie de tiempo sigue un proceso estacionario autorregresivo puro de orden ", p_s_h, sep = "")
} else {
    ar_puro_h <- ""
}

### MA()
if (p_s_x == 0 & d_s_x == 0) {
    ma_puro_x <- paste("por lo que la serie de tiempo sigue un proceso estacionario de promedios ", moviles," puro de orden ", q_s_x, sep = "")
} else {
    ma_puro_x <- ""
}

if (p_s_y == 0 & d_s_y == 0) {
    ma_puro_y <- paste("por lo que la serie de tiempo sigue un proceso estacionario de promedios ", moviles," puro de orden ", q_s_y, sep = "")
} else {
    ma_puro_y <- ""
}

if (p_s_z == 0 & d_s_z == 0) {
    ma_puro_z <- paste("por lo que la serie de tiempo sigue un proceso estacionario de promedios ", moviles," puro de orden ", q_s_z, sep = "")
} else {
    ma_puro_z <- ""
}

if (p_s_w == 0 & d_s_w == 0) {
    ma_puro_w <- paste("por lo que la serie de tiempo sigue un proceso estacionario de promedios ", moviles," puro de orden ", q_s_w, sep = "")
} else {
    ma_puro_w <- ""
}

if (p_s_h == 0 & d_s_h == 0) {
    ma_puro_h <- paste("por lo que la serie de tiempo sigue un proceso estacionario de promedios ", moviles," puro de orden ", q_s_h, sep = "")
} else {
    ma_puro_h <- ""
}


adf_d_x <- paste('

### Se vuelve a aplicar la prueba; Augmented Dickey-Fuller (adf.test) para comprobar que la serie es estacionaria

```{r, echo=F, message=F, warning=FALSE}
test.raiz <- adf.test(var_diff_x)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_x,', el valor p es el siguiente: ")
    diff_orden <- diff(var_x, lag = ',S_s_x,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}   
```', sep = "")

adf_d_y <- paste('

### Se vuelve a aplicar la prueba; Augmented Dickey-Fuller (adf.test) para comprobar que la serie es estacionaria

```{r, echo=F, message=F, warning=FALSE}
test.raiz <- adf.test(var_diff_y)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_y,', el valor p es el siguiente: ")
    diff_orden <- diff(var_y, lag = ',S_s_y,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}   
```', sep = "")

adf_d_z <- paste('

### Se vuelve a aplicar la prueba; Augmented Dickey-Fuller (adf.test) para comprobar que la serie es estacionaria

```{r, echo=F, message=F, warning=FALSE}
test.raiz <- adf.test(var_diff_z)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_z,', el valor p es el siguiente: ")
    diff_orden <- diff(var_z, lag = ',S_s_z,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}   
```', sep = "")

adf_d_w <- paste('

### Se vuelve a aplicar la prueba; Augmented Dickey-Fuller (adf.test) para comprobar que la serie es estacionaria

```{r, echo=F, message=F, warning=FALSE}
test.raiz <- adf.test(var_diff_w)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_w,', el valor p es el siguiente: ")
    diff_orden <- diff(var_w, lag = ',S_s_w,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}   
```', sep = "")

adf_d_h <- paste('

### Se vuelve a aplicar la prueba; Augmented Dickey-Fuller (adf.test) para comprobar que la serie es estacionaria

```{r, echo=F, message=F, warning=FALSE}
test.raiz <- adf.test(var_diff_h)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_h,', el valor p es el siguiente: ")
    diff_orden <- diff(var_h, lag = ',S_s_h,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}   
```', sep = "")



adf_d_x_script <- paste('test.raiz <- adf.test(var_diff_x)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_x,', el valor p es el siguiente: ")
    diff_orden <- diff(var_x, lag = ',S_s_x,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}', sep = "")

adf_d_y_script <- paste('test.raiz <- adf.test(var_diff_y)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_y,', el valor p es el siguiente: ")
    diff_orden <- diff(var_y, lag = ',S_s_y,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}', sep = "")

adf_d_z_script <- paste('test.raiz <- adf.test(var_diff_z)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_z,', el valor p es el siguiente: ")
    diff_orden <- diff(var_z, lag = ',S_s_z,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}', sep = "") 



adf_d_w_script <- paste('test.raiz <- adf.test(var_diff_w)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_w,', el valor p es el siguiente: ")
    diff_orden <- diff(var_w, lag = ',S_s_w,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}', sep = "") 

adf_d_h_script <- paste('test.raiz <- adf.test(var_diff_h)
options(digits=4) 
if (test.raiz$p.value < 0.05)
{
valor <- test.raiz$p.value  
     cat("Serie de tiempo estacionaria el valor p es: ", valor, 
    "\npor lo tanto, se rechaza la H0 de no estacionariedad.\nSe puede entonces calcular el modelo de ajuste")
} else {
 valor <- test.raiz$p.value
    cat("Serie de tiempo no estacionaria.\n",
       "\nDebido a que la serie sigue presentando no estacionariedad",
       "se calculan las primeras diferencias de la serie",
        "\njunto con el rezago de orden ',S_s_h,', el valor p es el siguiente: ")
    diff_orden <- diff(var_h, lag = ',S_s_h,')
    test.raiz <- adf.test(diff_orden)
       options(digits=4) 
       if (test.raiz$p.value < 0.05)
       {
          valor <- test.raiz$p.value  
          cat(valor, 
           "\npor lo tanto, la serie es estacionaria. \nSe puede entonces calcular el modelo de ajuste")
       } else {
           cat("Serie de tiempo no estacionaria.")
         }
}', sep = "") 


test.raiz_x <- adf.test(var_x)
options(digits=4) 
if (test.raiz_x$p.value > 0.05){
    var_tend_x <- "#### Eliminando la tendencia y estabilizando la varianza de la  serie de tiempo mediante el logaritmo diferenciado, para transformar la serie de tiempo a estacionaria."
    var_x <- "var_diff_x"  # Variable para correlograma
    adf_d_x <- adf_d_x
} else {
    var_tend_x <- ""
    var_x <- "var_x"   # Variable para correlograma
    adf_d_x <- ""
    adf_d_x_script <- ""
} 

test.raiz_y <- adf.test(var_y)
options(digits=4) 
if (test.raiz_y$p.value > 0.05){
    var_tend_y <- "#### Eliminando la tendencia y estabilizando la varianza de la  serie de tiempo mediante el logaritmo diferenciado, para transformar la serie de tiempo a estacionaria."
    var_y <- "var_diff_y"  # Variable para correlograma
    adf_d_y <- adf_d_y
} else {
    var_tend_y <- ""
    var_y <- "var_y"  # Variable para correlograma
    adf_d_y <- ""
    adf_d_y_script <- ""
}

test.raiz_z <- adf.test(var_z)
options(digits=4) 
if (test.raiz_z$p.value > 0.05){
    var_tend_z <- "#### Eliminando la tendencia y estabilizando la varianza de la  serie de tiempo mediante el logaritmo diferenciado, para transformar la serie de tiempo a estacionaria."
    var_z <-  "var_diff_z"  # Variable para correlograma
    adf_d_z <- adf_d_z
} else {
    var_tend_z <- ""
    var_z <- "var_z"  # Variable para correlograma
    adf_d_z <- ""
    adf_d_z_script <- ""
} 

test.raiz_w <- adf.test(var_w)
options(digits=4) 
if (test.raiz_w$p.value > 0.05){
    var_tend_w <- "#### Eliminando la tendencia y estabilizando la varianza de la  serie de tiempo mediante el logaritmo diferenciado, para transformar la serie de tiempo a estacionaria."
    var_w <-  "var_diff_w"  # Variable para correlograma
    adf_d_w <- adf_d_w
} else {
    var_tend_w <- ""
    var_w <- "var_w"  # Variable para correlograma
    adf_d_w <- ""
    adf_d_w_script <- ""
} 

test.raiz_h <- adf.test(var_h)
options(digits=4) 
if (test.raiz_h$p.value > 0.05){
    var_tend_h <- "#### Eliminando la tendencia y estabilizando la varianza de la  serie de tiempo mediante el logaritmo diferenciado, para transformar la serie de tiempo a estacionaria."
    var_h <-  "var_diff_h"  # Variable para correlograma
    adf_d_h <- adf_d_h
} else {
    var_tend_h <- ""
    var_h <- "var_h"  # Variable para correlograma
    adf_d_h <- ""
    adf_d_h_script <- ""
} 




if (patron_x == patron_ARIMA) {
    estacional_x <- "FALSE"
    correlo_x <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_x, i_x, ma_x,"**",sep="")
    var_est_x <- ""
} else {
    estacional_x <- "TRUE"
    correlo_x <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_x, i_x, ma_x,"**", " junto con el ", patron," de residuos (P,D,Q): ", "**",sar_x,si_x,sma_x,"**",sep="")
    var_est_x <- paste("#### Eliminando el componente estacional de la serie de tiempo mediante el ", calculo," de las diferencias de orden ", S_s_x, sep = "")
    #var_x <- "var_diff_log_orden"  
}
if (patron_y == patron_ARIMA) {
    estacional_y <- "FALSE"
    correlo_y <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_y, i_y, ma_y,"**",sep="")
    var_est_y <- ""
} else {
    estacional_y <- "TRUE"
    correlo_y <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_y, i_y, ma_y,"**", " junto con el ", patron," de residuos (P,D,Q): ", "**",sar_y,si_y,sma_y,"**",sep="")
    var_est_y <-  paste("#### Eliminando el componente estacional de la serie de tiempo mediante el ", calculo," de las diferencias de orden ",  S_s_y, sep = "")
    #var_y <- "var_diff_log_orden" 
}   
if (patron_z == patron_ARIMA) {
    estacional_z <- "FALSE"
    correlo_z <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_z, i_z, ma_z,"**",sep="")
    var_est_z <- ""
} else {
    estacional_z <- "TRUE"
    correlo_z <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_z, i_z, ma_z,"**", " junto con el ", patron," de residuos (P,D,Q): ", "**",sar_z,si_z,sma_z,"**",sep="")
    var_est_z <- paste("#### Eliminando el componente estacional de la serie de tiempo mediante el ", calculo," de las diferencias de orden ",  S_s_z, sep = "")  
    #var_z <- "var_diff_log_orden" 
}


if (patron_w == patron_ARIMA) {
    estacional_w <- "FALSE"
    correlo_w <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_w, i_w, ma_w,"**",sep="")
    var_est_w <- ""
} else {
    estacional_w <- "TRUE"
    correlo_w <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_w, i_w, ma_w,"**", " junto con el ", patron," de residuos (P,D,Q): ", "**",sar_w,si_w,sma_w,"**",sep="")
    var_est_w <- paste("#### Eliminando el componente estacional de la serie de tiempo mediante el ", calculo," de las diferencias de orden ",  S_s_w, sep = "")  
    #var_w <- "var_diff_log_orden" 
}

if (patron_h == patron_ARIMA) {
    estacional_h <- "FALSE"
    correlo_h <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_h, i_h, ma_h,"**",sep="")
    var_est_h <- ""
} else {
    estacional_h <- "TRUE"
    correlo_h <- paste("De las funciones de ", autocorrelacion, " obtengo el siguiente ", patron , "**",ar_h, i_h, ma_h,"**", " junto con el ", patron," de residuos (P,D,Q): ", "**",sar_h,si_h,sma_h,"**",sep="")
    var_est_h <- paste("#### Eliminando el componente estacional de la serie de tiempo mediante el ", calculo," de las diferencias de orden ",  S_s_h, sep = "")  
    #var_h <- "var_diff_log_orden" 
}


ajuste_x <- paste("ajusteARIMA <- arima(",x,", order=c(",p_s_x,",",d_s_x,",",q_s_x,"), seasonal = list(order = c(",P_s_x,",",D_s_x,",",Q_s_x,"), period = ",S_s_x,'), method="ML", include.mean = TRUE)', sep = "")
ajuste_y <- paste("ajusteARIMA <- arima(",y,", order=c(",p_s_y,",",d_s_y,",",q_s_y,"), seasonal = list(order = c(",P_s_y,",",D_s_y,",",Q_s_y,"), period = ",S_s_y,'), method="ML", include.mean = TRUE)', sep = "")
ajuste_z <- paste("ajusteARIMA <- arima(",z,", order=c(",p_s_z,",",d_s_z,",",q_s_z,"), seasonal = list(order = c(",P_s_z,",",D_s_z,",",Q_s_z,"), period = ",S_s_z,'), method="ML", include.mean = TRUE)', sep = "")
ajuste_w <- paste("ajusteARIMA <- arima(",y,", order=c(",p_s_w,",",d_s_w,",",q_s_w,"), seasonal = list(order = c(",P_s_w,",",D_s_w,",",Q_s_w,"), period = ",S_s_w,'), method="ML", include.mean = TRUE)', sep = "")
ajuste_h <- paste("ajusteARIMA <- arima(",z,", order=c(",p_s_h,",",d_s_h,",",q_s_h,"), seasonal = list(order = c(",P_s_h,",",D_s_h,",",Q_s_h,"), period = ",S_s_h,'), method="ML", include.mean = TRUE)', sep = "")



resumen <- data.frame(VARIABLE = c("X      ", "Y     ", "Z    ", "W   ", "H  "),
            AR = c(ar_x, ar_y, ar_z, ar_w, ar_h),
            I = c(i_x,i_y,i_z,i_w,i_h),
            MA = c(ma_x,ma_y,ma_z,ma_w,ma_h),
            S_AR = c(sar_x, sar_y, sar_z, sar_w, sar_h),
            S_I = c(si_x,si_y,si_z,si_w,si_h),
            S_MA = c(sma_x,sma_y,sma_z,sma_w,sma_h))

variables <- cbind(author, 
                   email, 
                   ruta, 
                   ruta_imagen, 
                   link, 
                   leyenda_link, 
                   anual_x, 
                   anual_y, 
                   anual_z, 
                   difer_anual_x, 
                   difer_anual_y, 
                   difer_anual_z, 
                   estaci2Script_x,
                   estaci2Script_y,
                   estaci2Script_z,
                   estaciMdw_x,
                   estaciMdw_y,
                   estaciMdw_z,
                   estaciACFScript_x,
                   estaciACFScript_y,
                   estaciACFScript_z, 
                   anho.max_x, 
                   anho.max_y, 
                   anho.max_z,
                   anho.min_x, 
                   anho.min_y, 
                   anho.min_z, 
                   archivo_x, 
                   archivo_y, 
                   archivo_z, 
                   d_s_x, 
                   D_s_x, 
                   d_s_y, 
                   D_s_y, 
                   d_s_z,
                   D_s_z, 
                   date, 
                   edo_x, 
                   edo_y, 
                   edo_z, 
                   frec_x, 
                   frec_y, 
                   frec_z, 
                   iteraciones_x, 
                   iteraciones_y ,
                   iteraciones_z , 
                   mes_x, 
                   mes_y, 
                   mes_z, 
                   n_diff_x, 
                   n_diff_y, 
                   n_diff_z, 
                   p_s_x,
                   P_s_x, 
                   p_s_y, 
                   P_s_y, 
                   p_s_z, 
                   P_s_z, 
                   q_s_x, 
                   Q_s_x, 
                   q_s_y, 
                   Q_s_y, 
                   q_s_z, 
                   Q_s_z, 
                   S_s_x, 
                   S_s_y, 
                   S_s_z, 
                   tiempo_x,
                   tiempo_y,
                   tiempo_z,
                   title, 
                   var_name_x, 
                   var_name_y,
                   var_name_z,
                   var_name_x_p,
                   var_name_y_p,
                   var_name_z_p,
                   periodo_x,
                   periodo_y,
                   periodo_z,
                   x,
                   y,
                   z,
                   w,
                   h,
                   methAj_x,
                   methAj_y,
                   methAj_z,
                   estacional_x,
                   estacional_y,
                   estacional_z,
                   var_tend_x,
                   var_tend_y,
                   var_tend_z,
                   var_est_x,
                   var_est_y,
                   var_est_z,
                   ar_x,
                   ar_y,
                   ar_z,
                   sar_x,
                   sar_y,
                   sar_z,
                   i_x,
                   i_y,
                   i_z,
                   si_x,
                   si_y,
                   si_z,
                   ma_x,
                   ma_y,
                   ma_z,
                   sma_x,
                   sma_y,
                   sma_z,
                   correlo_x,
                   correlo_y,
                   correlo_z,
                   ar_puro_x,
                   ar_puro_y,
                   ar_puro_z,
                   ma_puro_x,
                   ma_puro_y,
                   ma_puro_z,
                   var_x,
                   var_y,
                   var_z,
                   ajuste_x,
                   ajuste_y,
                   ajuste_z,
                   adf_d_x,
                   adf_d_y,
                   adf_d_z,
                   adf_d_x_script,
                   adf_d_y_script,
                   adf_d_z_script,
anual_w, 
anual_h, 
difer_anual_w, 
difer_anual_h, 
estaci2Script_w,
estaci2Script_h,
estaciMdw_w,
estaciMdw_h,
estaciACFScript_w,
estaciACFScript_h, 
anho.max_w, 
anho.max_h,
anho.min_w, 
anho.min_h, 
archivo_w, 
archivo_h, 
d_s_w, 
D_s_w, 
d_s_h,
D_s_h, 
edo_w, 
edo_h, 
frec_w, 
frec_h, 
iteraciones_w,
iteraciones_h, 
mes_w, 
mes_h, 
n_diff_w, 
n_diff_h, 
p_s_w, 
P_s_w, 
p_s_h, 
P_s_h, 
q_s_w, 
Q_s_w, 
q_s_h, 
Q_s_h, 
S_s_w, 
S_s_h, 
tiempo_w,
tiempo_h,
var_name_w,
var_name_h,
var_name_w_p,
var_name_h_p,
periodo_w,
periodo_h,
methAj_w,
methAj_h,
estacional_w,
estacional_h,
var_tend_w,
var_tend_h,
var_est_w,
var_est_h,
ar_w,
ar_h,
sar_w,
sar_h,
i_w,
i_h,
si_w,
si_h,
ma_w,
ma_h,
sma_w,
sma_h,
correlo_w,
correlo_h,
ar_puro_w,
ar_puro_h,
ma_puro_w,
ma_puro_h,
var_w,
var_h,
ajuste_w,
ajuste_h,
adf_d_w,
adf_d_h,
adf_d_w_script,
adf_d_h_script)

                   
    
####  Guardando las variables
write.xlsx(variables, arch.var, sheetName="variables_ls", row.names=FALSE)
#write.csv(variables, arch.var, row.names=FALSE)

###### write para PowerShell ######
write.csv(namePs, "namePs.csv", row.names = FALSE)
unidadPsLag <- read.csv("unidadPs.csv", header = T, stringsAsFactors = FALSE)
unidadPsLag <- unidadPsLag[1,1]
write.csv(unidadPsLag, "unidadPsLag.csv", row.names = FALSE)
write.csv(unidadPs, "unidadPs.csv", row.names = FALSE)
write.csv(rutaPs, "rutaPs.csv", row.names = FALSE)
write.csv(rutaDirPs, "rutaDirPs.csv", row.names = FALSE)
write.csv(wwwPs, "wwwPs.csv", row.names = FALSE)
###################################


###  Respaldo de variables en el log
setwd("F:/OneDrive/Proyecto/log")
log <- paste(inf[7,1],".csv", sep="")
time <- Sys.Date()
log <- paste(time, log , sep=" ")
write.csv(variables, log)


resumen
```

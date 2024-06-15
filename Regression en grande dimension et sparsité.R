###############################################################
###                                                         ###
###                                                         ###
###                                                         ###
###               Exercice 1 – Régularisation               ###
###                                                         ###
###                                                         ###
###                                                         ###
###############################################################

########################################Question1######################################## ----

## PARTIE 1 : DEFINITION DU MODELE

    ## INSTALLATION DES PACKAGES NECESSAIRES
    install.packages("glmnet")
    library(glmnet)

    # PARAMETRE 
    n = 1000
    p = 5000
    s = 15 # PARAMETRE DE SPARCITE
    eta = rnorm(n, 0, 1) #bruit
    beta = rep(c(1, 0), c(15, p - 15))
    
    
    X = matrix(data = 0, nrow = n, ncol = p)
    
    for (i in 1:p){
      X[, i] = rnorm(n, 0, 1)
    }
    
    #  MODELE 
    Y = X%*%beta + eta

## a/  UTILISATION DES FONCTIONS glmnet ET cv.glmnet

    Lasso = glmnet(X, Y, family = 'gaussian', alpha = 1)
    Ridge = glmnet(X, Y, family = 'gaussian', alpha = 0)
    Elastic_net_1 = glmnet(X, Y, family = 'gaussian', alpha = 0.1)
    Elastic_net_2 = glmnet(X, Y, family = 'gaussian', alpha = 0.2)
    Elastic_net_3 = glmnet(X, Y, family = 'gaussian', alpha = 0.3)
    Elastic_net_4 = glmnet(X, Y, family = 'gaussian', alpha = 0.4)
    Elastic_net_5 = glmnet(X, Y, family = 'gaussian', alpha = 0.5)
    Elastic_net_6 = glmnet(X, Y, family = 'gaussian', alpha = 0.6)
    Elastic_net_7 = glmnet(X, Y, family = 'gaussian', alpha = 0.7)
    Elastic_net_8 = glmnet(X, Y, family = 'gaussian', alpha = 0.8)
    Elastic_net_9 = glmnet(X, Y, family = 'gaussian', alpha = 0.9)
    
    cvLasso = cv.glmnet(X, Y, type.measure = 'mse', alpha = 1, family = 'gaussian')
    cvRidge = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0, family = 'gaussian')
    cvElastic1 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.1, family = 'gaussian')
    cvElastic2 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.2, family = 'gaussian')
    cvElastic3 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.3, family = 'gaussian')
    cvElastic4 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.4, family = 'gaussian')
    cvElastic5 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.5, family = 'gaussian')
    cvElastic6 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.6, family = 'gaussian')
    cvElastic7 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.7, family = 'gaussian')
    cvElastic8 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.8, family = 'gaussian')
    cvElastic9 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.9, family = 'gaussian')

## b/  REPRESENTATION GRAPHIQUE DES ESTIMATEURS
 
## ESTIMATEUR LASSO :     
    
    par(mfrow = c(1, 2), mar = c(5.1, 4.1, 6.1, 2.1))  
  
    # Premier graphique
    plot(Lasso, xvar = 'lambda', main = '')
    title(main = 'Graphique de l\'estimateur Lasso', line = 3)
    
    # Deuxième graphique
    plot(cvLasso, main = '')
    title(main = 'Validation croisée Lasso', line = 3)
    
## ESTIMATEUR RIDGE :     
    
    par(mfrow = c(1, 2), mar = c(5.1, 4.1, 6.1, 2.1))  
    
    # Premier graphique
    plot(Ridge, xvar = 'lambda', main = '')
    title(main = 'Graphique de l\'estimateur Ridge', line = 3)
    
    # Deuxième graphique
    plot(cvRidge, main = '')
    title(main = 'Validation croisée Ridge', line = 3)

## ESTIMATEUR Elastic_net :    
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.1')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.1')
    
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.2')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.2')
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.3')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.3')
    
    plot(Elastic_net_4, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.4')
    plot(cvElastic4, main = 'Validation croisee Elastic Net pour alpha = 0.4')
    
    plot(Elastic_net_9, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.9')
    plot(cvElastic9, main = 'Validation croisee Elastic Net pour alpha = 0.9')
    
    par(mfrow = c(1, 1))

## c/  ERREUR DE PREDICTION
    
    X_pred = X [1:340, ] # Base test
    erreur_pred = function(X_pred, beta, cvest){
      n = length(X_pred [, 1])
      p = length(beta)
      beta_chap = coef.glmnet(cvest, s = 'lambda.min', exact = FALSE)[1:p, 1]
      s = sum(abs(X_pred *beta - X_pred *beta_chap)^2)
      return(1/n*s)
    }

    err_Lasso = erreur_pred(X, beta, cvLasso)
    err_Ridge = erreur_pred(X, beta, cvRidge)
    err_E1 = erreur_pred(X, beta, cvElastic1)
    err_E2 = erreur_pred(X, beta, cvElastic2)
    err_E3 = erreur_pred(X, beta, cvElastic3)
    err_E4 = erreur_pred(X, beta, cvElastic4)
    err_E5 = erreur_pred(X, beta, cvElastic5)
    err_E6 = erreur_pred(X, beta, cvElastic6)
    err_E7 = erreur_pred(X, beta, cvElastic7)
    err_E8 = erreur_pred(X, beta, cvElastic8)
    err_E9 = erreur_pred(X, beta, cvElastic9)

########################################Question2######################################## ----
    
    ## PARTIE 1 : DEFINITION DU MODELE AVEC LE NOUVEAU PARAMETRE

    # PARAMETRE 
    n = 1000
    p = 5000
    s = 15 # PARAMETRE DE SPARCITE
    eta = rnorm(n, 0, 1) #bruit
    beta = rep(c(1, 0), c(1500, p - 1500))
    
    
    X = matrix(data = 0, nrow = n, ncol = p)
    
    for (i in 1:p){
      X[, i] = rnorm(n, 0, 1)
    }
    
    #  MODELE 
    Y = X%*%beta + eta

## a/  UTILISATION DES FONCTIONS glmnet ET cv.glmnet
    
    Lasso = glmnet(X, Y, family = 'gaussian', alpha = 1)
    Ridge = glmnet(X, Y, family = 'gaussian', alpha = 0)
    Elastic_net_1 = glmnet(X, Y, family = 'gaussian', alpha = 0.1)
    Elastic_net_2 = glmnet(X, Y, family = 'gaussian', alpha = 0.2)
    Elastic_net_3 = glmnet(X, Y, family = 'gaussian', alpha = 0.3)
    Elastic_net_4 = glmnet(X, Y, family = 'gaussian', alpha = 0.4)
    Elastic_net_5 = glmnet(X, Y, family = 'gaussian', alpha = 0.5)
    Elastic_net_6 = glmnet(X, Y, family = 'gaussian', alpha = 0.6)
    Elastic_net_7 = glmnet(X, Y, family = 'gaussian', alpha = 0.7)
    Elastic_net_8 = glmnet(X, Y, family = 'gaussian', alpha = 0.8)
    Elastic_net_9 = glmnet(X, Y, family = 'gaussian', alpha = 0.9)
    
    cvLasso = cv.glmnet(X, Y, type.measure = 'mse', alpha = 1, family = 'gaussian')
    cvRidge = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0, family = 'gaussian')
    cvElastic1 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.1, family = 'gaussian')
    cvElastic2 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.2, family = 'gaussian')
    cvElastic3 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.3, family = 'gaussian')
    cvElastic4 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.4, family = 'gaussian')
    cvElastic5 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.5, family = 'gaussian')
    cvElastic6 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.6, family = 'gaussian')
    cvElastic7 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.7, family = 'gaussian')
    cvElastic8 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.8, family = 'gaussian')
    cvElastic9 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.9, family = 'gaussian')
    
## b/  REPRESENTATION GRAPHIQUE DES ESTIMATEURS
    
    ## ESTIMATEUR LASSO :     
    
    par(mfrow = c(1, 2), mar = c(5.1, 4.1, 6.1, 2.1))  
    
    # Premier graphique
    plot(Lasso, xvar = 'lambda', main = '')
    title(main = 'Graphique de l\'estimateur Lasso', line = 3)
    
    # Deuxième graphique
    plot(cvLasso, main = '')
    title(main = 'Validation croisée Lasso', line = 3)
    
    ## ESTIMATEUR RIDGE :     
    
    par(mfrow = c(1, 2), mar = c(5.1, 4.1, 6.1, 2.1))  
    
    # Premier graphique
    plot(Ridge, xvar = 'lambda', main = '')
    title(main = 'Graphique de l\'estimateur Ridge', line = 3)
    
    # Deuxième graphique
    plot(cvRidge, main = '')
    title(main = 'Validation croisée Ridge', line = 3)
    
    ## ESTIMATEUR Elastic_net :    
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.1')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.1')
    
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.2')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.2')
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.3')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.3')
    
    plot(Elastic_net_4, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.4')
    plot(cvElastic4, main = 'Validation croisee Elastic Net pour alpha = 0.4')
    
    plot(Elastic_net_9, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.9')
    plot(cvElastic9, main = 'Validation croisee Elastic Net pour alpha = 0.9')
    
    par(mfrow = c(1, 1))
    
## c/  ERREUR DE PREDICTION
    
    X_pred = X [1:340, ] # Base test
    erreur_pred = function(X_pred, beta, cvest){
      n = length(X_pred [, 1])
      p = length(beta)
      beta_chap = coef.glmnet(cvest, s = 'lambda.min', exact = FALSE)[1:p, 1]
      s = sum(abs(X_pred *beta - X_pred *beta_chap)^2)
      return(1/n*s)
    }
    
    err_Lasso = erreur_pred(X, beta, cvLasso)
    err_Ridge = erreur_pred(X, beta, cvRidge)
    err_E1 = erreur_pred(X, beta, cvElastic1)
    err_E2 = erreur_pred(X, beta, cvElastic2)
    err_E3 = erreur_pred(X, beta, cvElastic3)
    err_E4 = erreur_pred(X, beta, cvElastic4)
    err_E5 = erreur_pred(X, beta, cvElastic5)
    err_E6 = erreur_pred(X, beta, cvElastic6)
    err_E7 = erreur_pred(X, beta, cvElastic7)
    err_E8 = erreur_pred(X, beta, cvElastic8)
    err_E9 = erreur_pred(X, beta, cvElastic9)
    
########################################Question3######################################## ----

## PARTIE 1 : DEFINITION DU MODELE AVEC LES NOUVEAUX PARAMETRES
    
    install.packages("MASS")
    library("MASS")
    
    # PARAMETRE 
    n = 100
    p = 50
    s = 14 # PARAMETRE DE SPARCITE
    eta = rnorm(n, 0, 1) #bruit
    mu = rep(0, p)
    beta = rep(c(10, 5, 1, 0), c(2, 2, 10, p - 14))
    Sigma = matrix(data = 0, nrow = p, ncol = p)
    
    for (i in 1:p){
      for(j in 1:p){
        Sigma[i, j] = 0.7^abs(i - j)
      }
    }
    
    X = mvrnorm(n, mu, Sigma)
    
    #  MODELE 
    Y = X%*%beta + eta
    
## a/  UTILISATION DES FONCTIONS glmnet ET cv.glmnet
    
    Lasso = glmnet(X, Y, family = 'gaussian', alpha = 1)
    Ridge = glmnet(X, Y, family = 'gaussian', alpha = 0)
    Elastic_net_1 = glmnet(X, Y, family = 'gaussian', alpha = 0.1)
    Elastic_net_2 = glmnet(X, Y, family = 'gaussian', alpha = 0.2)
    Elastic_net_3 = glmnet(X, Y, family = 'gaussian', alpha = 0.3)
    Elastic_net_4 = glmnet(X, Y, family = 'gaussian', alpha = 0.4)
    Elastic_net_5 = glmnet(X, Y, family = 'gaussian', alpha = 0.5)
    Elastic_net_6 = glmnet(X, Y, family = 'gaussian', alpha = 0.6)
    Elastic_net_7 = glmnet(X, Y, family = 'gaussian', alpha = 0.7)
    Elastic_net_8 = glmnet(X, Y, family = 'gaussian', alpha = 0.8)
    Elastic_net_9 = glmnet(X, Y, family = 'gaussian', alpha = 0.9)
    
    cvLasso = cv.glmnet(X, Y, type.measure = 'mse', alpha = 1, family = 'gaussian')
    cvRidge = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0, family = 'gaussian')
    cvElastic1 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.1, family = 'gaussian')
    cvElastic2 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.2, family = 'gaussian')
    cvElastic3 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.3, family = 'gaussian')
    cvElastic4 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.4, family = 'gaussian')
    cvElastic5 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.5, family = 'gaussian')
    cvElastic6 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.6, family = 'gaussian')
    cvElastic7 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.7, family = 'gaussian')
    cvElastic8 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.8, family = 'gaussian')
    cvElastic9 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.9, family = 'gaussian')
    
## b/  REPRESENTATION GRAPHIQUE DES ESTIMATEURS
    
    ## ESTIMATEUR LASSO :     
    
    par(mfrow = c(1, 2), mar = c(5.1, 4.1, 6.1, 2.1))  
    
    # Premier graphique
    plot(Lasso, xvar = 'lambda', main = '')
    title(main = 'Graphique de l\'estimateur Lasso', line = 3)
    
    # Deuxième graphique
    plot(cvLasso, main = '')
    title(main = 'Validation croisée Lasso', line = 3)
    
    ## ESTIMATEUR RIDGE :     
    
    par(mfrow = c(1, 2), mar = c(5.1, 4.1, 6.1, 2.1))  
    
    # Premier graphique
    plot(Ridge, xvar = 'lambda', main = '')
    title(main = 'Graphique de l\'estimateur Ridge', line = 3)
    
    # Deuxième graphique
    plot(cvRidge, main = '')
    title(main = 'Validation croisée Ridge', line = 3)
    
    ## ESTIMATEUR Elastic_net :    
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.1')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.1')
    
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.2')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.2')
    
    plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.3')
    plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.3')
    
    plot(Elastic_net_4, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.4')
    plot(cvElastic4, main = 'Validation croisee Elastic Net pour alpha = 0.4')
    
    plot(Elastic_net_9, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.9')
    plot(cvElastic9, main = 'Validation croisee Elastic Net pour alpha = 0.9')
    
    par(mfrow = c(1, 1))
    
## c/  ERREUR DE PREDICTION
    
    erreur_pred = function(X, beta, cvest){
      n = length(X[, 1])
      p = length(beta)
      beta_chap = coef.glmnet(cvest, s = 'lambda.min', exact = FALSE)[1:p, 1]
      s = sum(abs(X *beta - X*beta_chap)^2)
      return(1/n*s)
    }
    
    err_Lasso = erreur_pred(X, beta, cvLasso)
    err_Ridge = erreur_pred(X, beta, cvRidge)
    err_E1 = erreur_pred(X, beta, cvElastic1)
    err_E2 = erreur_pred(X, beta, cvElastic2)
    err_E3 = erreur_pred(X, beta, cvElastic3)
    err_E4 = erreur_pred(X, beta, cvElastic4)
    err_E5 = erreur_pred(X, beta, cvElastic5)
    err_E6 = erreur_pred(X, beta, cvElastic6)
    err_E7 = erreur_pred(X, beta, cvElastic7)
    err_E8 = erreur_pred(X, beta, cvElastic8)
    err_E9 = erreur_pred(X, beta, cvElastic9)


###############################################################
###                                                         ###
###                                                         ###
###                                                         ###
###                     Exercice 2                          ###
###                                                         ###
###                                                         ###
###                                                         ###
###############################################################
    
data(mtcars)

p = length(mtcars)
n = length(mtcars[, 1])

X = mtcars[, 2:11]
Y = mtcars[, 1]

X = as.matrix(X)
Y = as.matrix(Y)

## a/  UTILISATION DES FONCTIONS glmnet ET cv.glmnet

Lasso = glmnet(X, Y, family = 'gaussian', alpha = 1)
Ridge = glmnet(X, Y, family = 'gaussian', alpha = 0)
Elastic_net_1 = glmnet(X, Y, family = 'gaussian', alpha = 0.1)
Elastic_net_2 = glmnet(X, Y, family = 'gaussian', alpha = 0.2)
Elastic_net_3 = glmnet(X, Y, family = 'gaussian', alpha = 0.3)
Elastic_net_4 = glmnet(X, Y, family = 'gaussian', alpha = 0.4)
Elastic_net_5 = glmnet(X, Y, family = 'gaussian', alpha = 0.5)
Elastic_net_6 = glmnet(X, Y, family = 'gaussian', alpha = 0.6)
Elastic_net_7 = glmnet(X, Y, family = 'gaussian', alpha = 0.7)
Elastic_net_8 = glmnet(X, Y, family = 'gaussian', alpha = 0.8)
Elastic_net_9 = glmnet(X, Y, family = 'gaussian', alpha = 0.9)

cvLasso = cv.glmnet(X, Y, type.measure = 'mse', alpha = 1, family = 'gaussian')
cvRidge = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0, family = 'gaussian')
cvElastic1 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.1, family = 'gaussian')
cvElastic2 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.2, family = 'gaussian')
cvElastic3 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.3, family = 'gaussian')
cvElastic4 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.4, family = 'gaussian')
cvElastic5 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.5, family = 'gaussian')
cvElastic6 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.6, family = 'gaussian')
cvElastic7 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.7, family = 'gaussian')
cvElastic8 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.8, family = 'gaussian')
cvElastic9 = cv.glmnet(X, Y, type.measure = 'mse', alpha = 0.9, family = 'gaussian')


## b/  REPRESENTATION GRAPHIQUE DES ESTIMATEURS

par(mfrow = c(1, 2))

plot(Lasso, xvar = 'lambda', main = 'Graphique de l\'estimateur Lasso')
plot(cvLasso, main = 'Validation croisee Lasso')

plot(Ridge, xvar = 'lambda', main = 'Graphique de l\'estimateur Ridge')
plot(cvRidge, main = 'Validation croisee Ridge')

plot(Elastic_net_1, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.1')
plot(cvElastic1, main = 'Validation croisee Elastic Net pour alpha = 0.1')

plot(Elastic_net_4, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.4')
plot(cvElastic4, main = 'Validation croisee Elastic Net pour alpha = 0.4')

plot(Elastic_net_9, xvar = 'lambda', main = 'Graphique de l\'estimateur Elastic Net pour alpha = 0.9')
plot(cvElastic9, main = 'Validation croisee Elastic Net pour alpha = 0.9')

par(mfrow = c(1, 1))











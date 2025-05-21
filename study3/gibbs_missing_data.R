## A ideia é inputar os ys faltantes como se fossem variáveis latentes q serão geradas pela
## condicional completa em cada iteração

#durante a execução do modelo, os valores faltantes serão tratados como variáveis latentes
#e preenchidos iterativamente com base na distribuição condicional completa em cada iteração 
#do processo de amostragem, permitindo uma estimativa mais robusta dos parâmetros do modelo
#mesmo na presença de dados faltantes.


#Passo-a-passo do Processo
#Inicialização: Comece com um conjunto inicial de valores para os parâmetros do modelo e, 
#se necessário, imputações iniciais para os dados faltantes.

#Iteração: Para cada iteração do algoritmo:
#  Geração dos Dados Faltantes: Gere novos valores para os dados faltantes a partir da 
#distribuição condicional completa.
#Atualização dos Parâmetros: Atualize os parâmetros do modelo com base nos dados observados
#e nos dados faltantes imputados.
#Repetição: Repita o processo até que o algoritmo converja, ou seja, até que as 
#distribuições dos parâmetros e das imputações dos dados faltantes se estabilizem.


# gibbs_missing_data <- function(m0, C0, y, Ft0, Gt, delta, V, v0, s0, nit, bn, thin, y_missing = NULL,
#                                ...){
  gibbs_missing_data <- function(m0, C0, y, Ft0, Gt, delta, V, v0, s0, nit, lower = NULL, upper = NULL, bn,
                                 bn.sample = NULL, thin, y_missing = NULL,
                                 ...){
  n <- nrow(y)
  q <- ncol(y)
  p = length(m0)
  ##### aqui mostra em qual linha (age) e coluna (sex) temos missing
  ind_missing = which(is.na(y), arr.ind = T) 
  
  Ft <- Ft0
  V.post <- array(NA, dim = c(nit, q, q))
  mu.post <- array(NA, dim = c(nit, n, q))
  # theta.post <- matrix(NA, nit, ncol = n)
  theta.post <- array(dim = c(nit, n, ncol(Ft)))
  Wt = array(NA, dim = c(nit, n, p, p))
  
  ### parte do missing data
  input.post = matrix(NA, nrow = nit, ncol = nrow(ind_missing))
  
  ## chutes iniciais
  if(length(ind_missing) > 0 & is.null(y_missing)){
    y_missing = rowMeans(y[ind_missing[,1],], na.rm = T)
  }
  y[ind_missing] = y_missing
  t_missing = unique(ind_missing[,1])
  
  pb  = progress::progress_bar$new(format = "Simulating [:bar] :percent in :elapsed",
                                   total = nit, clear = FALSE, width = 60)
  
  ## first parameter from posterior of V
  alpha.star <- (v0 + 1 + n)/2
  V0 <- (v0-2)*s0
  
  for (k in 1:nit) {
    
    pb$tick()
    
    # FFBS for thetas (each age x)
    mld <- ffbs(m0, C0, y, V = V, Ft0, Gt, delta, ...)
    dt <- mld$d
    Wt[k,,,] = mld$W

    mu.post[k,,] <- t(Ft%*%t(dt)) ;   theta.post[k,,] <- dt ;
    muk <- mu.post[k,,]
    
    ####### gibbs for V (pagina 174 do petris distribuicao a posteriori)
    SSy = t(y-muk)%*%(y-muk)
    beta.star <- 0.5*(SSy + V0)  ### S0= 0.5*V0     
    V.post[k,,] <- solve(rWishart(1, alpha.star, solve(beta.star))[,,1])
    V <- V.post[k,,]
    
    ## condicional completa y
    input = c()
    ## indice com 1 se refere ao dado faltante e indice 2 ao dado observado
    for(i in 1:length(t_missing)){
      ## valores dos y observados da i-esima idade com dados faltantes
      aux2 = y[t_missing[i], -ind_missing[ind_missing[,1] == t_missing[i], 2]]
      ## Medias
      mu1 = muk[t_missing[i], ind_missing[ind_missing[,1] == t_missing[i], 2]]
      mu2 = muk[t_missing[i], -ind_missing[ind_missing[,1] == t_missing[i], 2]]
      ## Variancias
      V11 = V[ind_missing[ind_missing[,1] == t_missing[i], 2], ind_missing[ind_missing[,1] == t_missing[i], 2]]
      V12 = V[ind_missing[ind_missing[,1] == t_missing[i], 2], -ind_missing[ind_missing[,1] == t_missing[i], 2]]
      V22 = V[-ind_missing[ind_missing[,1] == t_missing[i], 2], -ind_missing[ind_missing[,1] == t_missing[i], 2]]
      inv_V22 = solve(V22)
      V21 = as.vector(t(V12))
      
      ## Parametros da condicional
      mu_condicional = mu1 + V12%*%inv_V22%*%(aux2-mu2) ## aux 2 eh yobs e mu2 é Fxthetaxobs
      sigma_condicional = V11 - V12%*%inv_V22%*%V21
    
      # ## Parametros da condicional
      # mu_condicional = mu1 + V12%*%inv_V22%*%(aux2-mu2) ## aux 2 eh yobs e mu2 é Fxthetaxobs
      # sigma_condicional = V11 - V12%*%inv_V22%*%t(V12)

       y[t_missing[i], ind_missing[ind_missing[,1] == t_missing[i], 2]] = MASS::mvrnorm(1, mu = mu_condicional, Sigma = sigma_condicional)
      input = append(input, y[t_missing[i], ind_missing[ind_missing[,1] == t_missing[i], 2]])
    }
    input.post[k,] = input
    
  }
  return(list(mu = mu.post[seq(bn+1, nit, by = thin),,],
              theta = theta.post[seq(bn+1, nit, by = thin),,],
              V = V.post[seq(bn+1, nit, by = thin),,], 
              Wt = Wt[seq(bn+1, nit, by = thin),,,], 
              input = input.post[seq(bn+1, nit, by = thin),]))
}



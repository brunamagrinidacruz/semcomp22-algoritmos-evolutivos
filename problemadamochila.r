# Uma mochila suporta 15kg
# Temos a possibilidade de fazer a combinacao entre os pesos e o ganho:
# Valor: 2 Peso: 1
# Valor: 4 Peso: 12
# Valor: 2 Peso: 2
# Valor: 1 Peso: 1
# Valor: 10 Peso: 4

capacidade = 15
valor = c(2, 4, 2 , 1, 10)
peso = c(1, 12, 2, 1, 4)

mochila <- function(cromossomo) {
  valor_mochila = 0
  peso_mochila = 0
  
  # Para i de 1 ate o tamanho do cromossomo
  for(i in 1:length(cromossomo)) {
    
    # Se o valor da posicao do cromossomo for 1, pega o valor e o peso daquele item
    # Exemplo: 1 1 0 1 0
    # Pegaria o valor: 2 (1º), 4 (2º), 1 (4º) = 7
    # Pegaria o peso: 1 (1º), 12 (2º), 1 (4º) = 14
    if(cromossomo[i] == 1) {
        valor_mochila = valor_mochila + valor[i]
        peso_mochila = peso_mochila + peso[i]
    }
    
  }
  
  # Estamos verificando se o peso estourou a capacidade da mochila
  if(peso_mochila > capacidade) {
        return(0)
  } else {
        return (valor_mochila)
  }
  
}

# Temos uma aplicacao totalmente diferente do cromossomo em relacao ao AlgortimosEvolutivos-v3, mas o restante e identico:

# Entrada:
# - tamanho da populacao
# - tamanho do cromosssomo
# - numero de geracoes
# Saida:
# - media e desvio do fitness da populacao final

ae_mochila <- function(tamanho_populacao, tamanho_cromossomo, numero_de_geracoes) {
  
  # A funcao round(numero, casa) arredonda um numero para uma quantia de casa decimais. Por definicao, casa = 0
  # Exemplo: round(123.546, 2) = 123.54 ou round(123.546) = 123
  # A funcao runinf(n) cria n valores aleatorios entre 0 e 1
  # Quanto ruinf(n, a, b) ela cria n valores aleatorios entre a e b
  
  # Cria a populacao de invidivuos. Cada cromossomo e uma linha da matriz
  populacao <- matrix(round(runif(tamanho_populacao * tamanho_cromossomo)),
                      nrow = tamanho_populacao) 
  # Cria uma matriz com n valores de 0 e 1 (gerando valores entre 0 e 1 com o ruinf e arredondando com round)
  # O n tem tamanho tamanho_populacao * tamanho_cromossomo (se tiver 3 individuos, cada qual com 5 de tamanho, será necessario 3*5 = 15 valores)
  # Alem disso, a matriz tem tamanho_população de linhas
  
  # Array que armazenara as aptidoes
  fitness = c()
  
  # Avalia cada individuo
  # Neste exemplo, a aptidao e exatamente o valor da soma dos bits
  for(i in 1:nrow(populacao)) { # Interar na quantidade de individuos. Poderia ser tamanho_populacao
    
    # 1º Modificacao: fitness sera o valor dentro da mochila
    fitness = c(fitness, mochila(populacao[i,]))
    
  }
  
  # A funcao max(vetor), mean(vetor) e min(vetor) retorna respectivamente o maior, do meio e minino elemento do vetor
  
  # Imprime a aptidao do melhor individuo, do pior e a media
  cat("Indice \t Maximo \t Media \t Minimo \n")
  cat(0, "\t", max(fitness), "\t", mean(fitness), "\t", min(fitness), "\n")
  
  
  # Inicio do processo evolutivo
  
  for(i in 1:numero_de_geracoes) {
    
    # A funcao sample(numero, quantidade) gera quantidade de valores entre 1 ate numero de forma aleatoria
    # Exemplo sample(5) = 5 2 3 4 1 ou sample(5, 2) = 2 4
    
    # Seleciona dois reprodutores
    reprodutores = sample(tamanho_populacao, 2, replace = FALSE)
    # Aqui, a funcao sample vai escrever 2 indices entre o tamanho_populacao
    # Os individuos que forem tiverem esse indice, sao os selecionados para serem reprodutores
    
    # Seleciona um ponto para ocorrer o crossover (nesse ponto o vetor pai1 e pai2 serao cortados)
    ponto_de_crossover = sample(tamanho_cromossomo-2, 2) + 1
    # Exclui os pontos das extremidades (2: comeco e fim) e soma 1, para nao ficar no inicio
    # Isso ocorre porque se nao o ponto de crossover poderia ser o 1 ou tamanho_cromossomo, nao gerando modificacoes no cromossomo filho
    
    # Criando os dois filhos
    filho1 = c(populacao[reprodutores[1], 1:ponto_de_crossover], # Pega os valores do cromossomo reprodutor1 do inicio ate o ponto de crossover
               populacao[reprodutores[2], (ponto_de_crossover+1):tamanho_cromossomo]) # Pega os valores do cromossomo reprodutor2 do ponto de crossover ate o fim do cromossomo
    # Assim, gera-se um novo cromossomo com parte do corpo do reprodutor 1 e a outra parte do reprodutor 2
    filho2 = c(populacao[reprodutores[2], 1:ponto_de_crossover],
               populacao[reprodutores[1], (ponto_de_crossover+1):tamanho_cromossomo])
    
    # Seleciona os pontos para occorer a mutacao
    ponto_de_mutacao1 = sample(tamanho_cromossomo, 1, replace = TRUE)
    ponto_de_mutacao2 = sample(tamanho_cromossomo, 1, replace = TRUE)
    
    # Aplica mutacao nos dois filhos gerados
    filho1[ponto_de_mutacao1] = !filho1[ponto_de_mutacao1]
    filho2[ponto_de_mutacao2] = !filho2[ponto_de_mutacao2]
    
    # 2º Modificacao: Calcula o fitness de cada filho
    fitness_filho1 = mochila(filho1)
    fitness_filho2 = mochila(filho2)
    
    # A funcao order(vetor) retorna um vetor com as posicoes para que vetor fique em ordem crescente
    # Exemplo: order(7, 5, 4) = 3, 2, 1 pois o item 3, item 2 e item 1 nessa sequencia tornaria o vetor ordenado
    # Escrevendo order(vetor)[a:b] retorna um vetor de a ate b
    
    # Devolve o indice dos individuos com piores soma (fitness) da populacao
    inferiores_populacao <- order(fitness)[1:2]
    
    # Se o filhos tem melhor fitness que os dois piores individuos, realiza a substituicao
    if(fitness_filho1 > fitness[inferiores_populacao[1]]) {
      populacao[inferiores_populacao[1], ] <- filho1 # Agora, o pior individuo e substituido pelo filho 1
      fitness[inferiores_populacao[1]] = fitness_filho1 # Atualizando fitness
    }
    
    if(fitness_filho2 > fitness[inferiores_populacao[2]]) {
      populacao[inferiores_populacao[2], ] <- filho2
      fitness[inferiores_populacao[2]] = fitness_filho2
    }
    
    # Imprime a aptidao do melhor individuo, do pior e a media
    cat(i, "\t", max(fitness), "\t", mean(fitness), "\t", min(fitness), "\n")
  }
  
  cat("\n População final:\n")
  print(populacao)
  
  # Retorna o valor da media e do desvio-padrao
  return(c(mean(fitness), sd(fitness)))
  
}



# Geracao de cromossomo
cromossomo = round(runif(10));
cat("Cromossomo: ", cromossomo, "\n");

# Avaliacao
soma = sum(cromossomo) #Funcao objetivo
cat("Soma: ", soma, "\n")

# Nossa funcao objetivo e a aptidao tambem (Quanto mais valores 1, melhor)
melhor_aptidao = soma

# Imprimi titulo da tabela
cat("Solucao: \t Cromossomo: \t Soma: ")

for(i in 1:10) { # Criaremos 10 geracoes. A cada geracao, iremos fazer uma mutacao no individuo
    
    # Ponto que ira sofrer mutacao (Ponto de 1 ate 10 entre os genes)
    ponto_de_mutacao = sample(10, 1, replace = TRUE) # Seleciona gene que sofrera mutacao 
    # A sample vai escolher um numero de 1 ate 10. O numero escolhido sera o gene alterado. 
    # Vamos, para evoluir, alterar o valor do ponto_de_mutacao
    # Se o ponto de mutacao for 1, virara 0, se for 0, virara 1
    cromossomo[ponto_de_mutacao] = !cromossomo[ponto_de_mutacao] #Aplica mutacao no gene (ponto_de_mutacao) selecionado

    # Avalia nova solucao 
    soma = sum(cromossomo)

    # Imprimi resultados (em forma de tabela)
    cat(i, "\t") # Indice
    cat(cromossomo, "\t"); # Cromossomo
    cat(soma, "\n") # Soma de valores 1

}

# Nesta solucao temos uma evolucao aleatoria (podemos perder quantidades de 0, assim nao evoluindo como queremos)
# Vamos tentar criar uma evolucao de selecao natural
 
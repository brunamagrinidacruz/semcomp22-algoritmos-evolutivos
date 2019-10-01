# Agora vamos criar um algoritmo EVOLUTIVO. Ou seja, que evolua apenas quando necessario.
# Ou seja, nosso algoritmo cromossomo so sera substituido pelo filho se ele for melhor

# Geracao de cromossomo
cromossomo = round(runif(10));
cat("Cromossomo: ", cromossomo, "\n");

# Avaliacaoo
soma = sum(cromossomo)
cat("Soma: ", soma, "\n")

# Cria vetor que armazena o fitness
fitness = c()
# Concatenando ao fitness o valor soma
fitness = c(fitness, soma)

# Imprimi titulo da tabela
cat("Solucao: \t Cromossomo: \t Soma: ")

for(i in 1:10) { # Criaremos 10 gerações. A cada geração, iremos fazer uma mutação no individuo
    
    # Ponto que ira sofrer mutacao (Ponto de 1 ate 10 entre os genes)
    ponto_de_mutacao = sample(10, 1, replace = TRUE) # Seleciona gene que sofrera mutacao
  
    # Criaremos um filho do cromossomo com a mutacao
    filho = cromossomo
    filho[ponto_de_mutacao] = !cromossomo[ponto_de_mutacao]
    soma_filho = sum(filho) # A soma dos valores 1

    # Se o filho for mais evoluido, ele ira se tornar o cromossomo 
    if(soma_filho > soma) {
        cromossomo = filho
        soma = soma_filho
    }

    fitness = c(fitness, sum)

    # Imprimi resultados (em forma de tabela)
    cat(i, "\t") # Indice
    cat(cromossomo, "\t"); # Cromossomo
    cat(soma, "\n") # Soma de valores 1
}

# plot(fitness, "l")

# Agora, nosso cromossomo so ira evoluir se o filho for melhor


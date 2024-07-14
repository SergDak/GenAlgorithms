library(tidyverse)

# calculate fitness score: how many characters differ from target
check_fitness <- function(target,array) {
  sum(target!=array)
}

# select the best subset of population to propagate to next generation
selection <- function(target,population,n_survivors = 50) {
  fitness <- apply(population,MARGIN = 1,FUN = check_fitness,target= target_string)
  survivors <- cbind(population, fitness) %>% arrange(fitness) %>% head(n_survivors)
}

# generate new offspring based on given parents
genetics <- function(mate_1,mate_2,p_mutation = 0.1) {
  offspring <- mate_1
  p_parent <- (1-p_mutation)/2+p_mutation
  roll <- runif(length(mate_1))
  for (i in 1:length(mate_1)) {
    if (roll[i]<= p_mutation) {
      offspring[i] <- sample(candidates,1)
      next
    }
    if (roll[i] <= p_parent){
      offspring[i] <- mate_1[i]
      next
    }
    offspring[i] <- mate_2[i]
  }
  offspring <- data.frame(offspring)
  names(offspring) <- 1:length(mate_1)
  return(offspring)
}

# create next geeration from population
mate <- function(population,n_offspring=3, p_mutation = 0.1) {
  new_generation <- population %>% select(-fitness)
  for(i in 1:(nrow(population)/2)) {
    mate_1 <- population[i,] %>% select(-fitness)
    mate_2 <- sample(1:nrow(population),1)
    mate_2 <- population[mate_2,] %>% select(-fitness)
    for (j in 1:n_offspring){
      new_offspring <- genetics(mate_1,mate_2,p_mutation )
      new_generation <- rbind(new_generation,new_offspring)
    }
  }
  new_generation
}

#create target string to discover
target_string <- "hello, this is a secret"

#split the string for easier comparison
target_string <- strsplit(target_string,'',fixed = T)[[1]]

#possible options for random values
candidates <- c(letters,LETTERS,',','.',' ','!','?')

#initiate at generation 0
gen_count <- 0
pop_size <- 100

#create starting population
population <- data.frame()
for (i in 1:pop_size) {
  new_member <- sample(candidates,length(target_string),replace = T)
  population <- rbind(population,new_member)
}
names(population) <- 1:length(target_string)
#deetrmine starting fitness score
survivors <- selection(target_string,population)
best_fitness <- survivors %>% head(1) %>% pull(fitness)
# collate numer of generations to convergence for effective population of 50, mutation rate of 0.1 and 3 offspring per mating
gens_50_0.1_3 <- c()
starting_survivors <- survivors
for (i in 1:500){
# repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors)
    survivors <- selection(target_string,population)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }

  print(c(i,gen_count))
  gens_50_0.1_3 <- c(gens_50_0.1_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

hist(gens_50_0.1_3)


gens_50_0.25_3 <- c()
for (i in 1:500){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.25)
    survivors <- selection(target_string,population)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_50_0.25_3 <- c(gens_50_0.25_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

hist(gens_50_0.25_3)
summary(gens_50_0.25_3)
summary(gens_50_0.1_3)


gens_50_0.1_5 <- c()
for (i in 1:500){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.1,n_offspring = 5)
    survivors <- selection(target_string,population)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_50_0.1_5 <- c(gens_50_0.1_5, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

hist(gens_50_0.1_5)
summary(gens_50_0.1_5)


gens_100_0.1_3 <- c()
for (i in 1:500){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.1,n_offspring = 3)
    survivors <- selection(target_string,population,n_survivors = 100)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_100_0.1_3 <- c(gens_100_0.1_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}


gens_150_0.1_3 <- c()
for (i in 1:200){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.1,n_offspring = 3)
    survivors <- selection(target_string,population,n_survivors = 150)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_150_0.1_3 <- c(gens_150_0.1_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

gens_50_0.1_10 <- c()
for (i in 1:200){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.1,n_offspring = 10)
    survivors <- selection(target_string,population,n_survivors = 50)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_50_0.1_10 <- c(gens_50_0.1_10, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

gens_50_0.1_1 <- c()
for (i in 1:200){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.1,n_offspring = 1)
    survivors <- selection(target_string,population,n_survivors = 50)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_50_0.1_1 <- c(gens_50_0.1_1, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}



gens_50_0.15_3 <- c()
for (i in 1:200){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.15,n_offspring = 3)
    survivors <- selection(target_string,population,n_survivors = 50)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_50_0.15_3 <- c(gens_50_0.15_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}


gens_50_0.05_3 <- c()
for (i in 1:200){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.05,n_offspring = 3)
    survivors <- selection(target_string,population,n_survivors = 50)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_50_0.05_3 <- c(gens_50_0.05_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

gens_200_0.1_3 <- c()
for (i in 1:200){
  # repeat until convergence
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.1,n_offspring = 3)
    survivors <- selection(target_string,population,n_survivors = 200)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
    best_guess <- survivors %>% head(1) %>% select(-fitness) %>% paste0(.,collapse = '')
    #print(paste("genration:", gen_count,"value:",best_guess,"fitness:", best_fitness))
  }
  
  print(c(i,gen_count))
  gens_200_0.1_3 <- c(gens_200_0.1_3, gen_count) 
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_survivors
  
}

gens_100_0.1_3_short <- gens_100_0.1_3[1:200]
gens_50_0.1_3_short <- gens_50_0.1_3[1:200]
gens_50_0.1_5_short <- gens_50_0.1_5[1:200]
gens_50_0.25_3_short <- gens_50_0.25_3[1:200]
gens_large <- cbind.data.frame(gens_100_0.1_3_short,gens_150_0.1_3,gens_50_0.1_3_short,gens_50_0.1_1,gens_50_0.1_5_short,gens_50_0.1_10,gens_50_0.25_3_short)
write.csv(gens_large,"gens_full.csv",row.names = FALSE)
gens_large <- gens_large %>% bind_cols('gens_50_0.15_3'=gens_50_0.15_3,'gens_50_0.05_3'=gens_50_0.05_3)

gens_large <- bind_cols("base" = gens_50_0.1_3_short,
                        "population_100" = gens_100_0.1_3_short,
                        "population_150" = gens_150_0.1_3,
                        "population_200" = gens_200_0.1_3,
                        "mutation_0.05" = gens_50_0.05_3,
                        "mutation_0.15" = gens_50_0.15_3,
                        "mutation_0.25" = gens_50_0.25_3_short,
                        "offspring_1" = gens_50_0.1_1,
                        "offspring_5" = gens_50_0.1_5_short,
                        "offspring_10" = gens_50_0.1_10)



write.csv(starting_survivors,"starting_pop.csv",row.names = FALSE)












gens_200_0.05_1 <- c()
times_200_0.05_1 <- c()
for (i in 1:200){
  set.seed(i)
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_pop
  # repeat until convergence
  begin <- Sys.time()
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.05,n_offspring = 1)
    survivors <- selection(target_string,population,n_survivors = 200)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
  }
  finish <- Sys.time()
  print(c(i,gen_count,finish-begin))
  gens_200_0.05_1 <- c(gens_200_0.05_1, gen_count)
  times_200_0.05_1 <- c(times_200_0.05_1,finish-begin)
}


gens_50_0.05_1 <- c()
times_50_0.05_1 <- c()
for (i in 1:200){
  set.seed(i)
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_pop
  # repeat until convergence
  begin <- Sys.time()
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors,p_mutation = 0.05,n_offspring = 1)
    survivors <- selection(target_string,population)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
  }
  finish <- Sys.time()
  print(c(i,gen_count,finish-begin))
  gens_50_0.05_1 <- c(gens_50_0.05_1, gen_count) 
  times_50_0.05_1 <- c(times_50_0.05_1,finish-begin)
  
  
}


gens_50_0.1_3 <- c()
times_50_0.1_3 <- c()
for (i in 1:200){
  set.seed(i)
  best_fitness <- 1
  gen_count <- 0
  survivors <- starting_pop
  # repeat until convergence
  begin <- Sys.time()
  while (best_fitness>0) {
    gen_count <- gen_count+1
    population <- mate(survivors)
    survivors <- selection(target_string,population)
    best_fitness <- survivors %>% head(1) %>% pull(fitness)
  }
  finish <- Sys.time()
  print(c(i,gen_count,finish-begin))
  gens_50_0.1_3 <- c(gens_50_0.1_3, gen_count)
  times_50_0.1_3 <- c(times_50_0.1_3,finish-begin)
}

times_200_0.05_1 <- ifelse(times_200_0.05_1<10,times_200_0.05_1*60,times_200_0.05_1)
times_50_0.05_1 <- ifelse(times_50_0.05_1<10,times_50_0.05_1*60,times_50_0.05_1)
times_50_0.1_3 <- ifelse(times_50_0.1_3<10,times_50_0.1_3*60,times_50_0.1_3)

times_big <- cbind.data.frame(times_50_0.1_3,times_200_0.05_1,times_50_0.05_1)
gens_mixed <- cbind.data.frame(gens_50_0.1_3,gens_200_0.05_1,gens_50_0.05_1)
write.csv(times_big,"times_mixed.csv",row.names = FALSE)
write.csv(gens_mixed,"gens_mixed.csv",row.names = FALSE)
times_big %>%
  pivot_longer(cols=everything(),names_to = "source", values_to = "value")  %>%
  ggplot() + geom_histogram(aes(x=value,fill=source),binwidth = 25,alpha = 0.4, position = 'identity') +
  theme_light()+
  labs(x="minutes",y="Count", title = "Histogram of Completion Timew in minutes", subtitle = "For varying N-population P-mutation and M-offspring",fill = "Model") +
  scale_fill_manual(labels = c("n=200 p=0.05 m=1","n=50 p=0.05 m=1","Baseline"), values = c("blue","red","green")) +
  theme(panel.grid.minor.y = element_blank())

times_big %>% 
  pivot_longer(cols=everything(),names_to = "source", values_to = "value")  %>% group_by(source) %>%
  summarize(mean = mean(value),variance = var(value)) %>% arrange(mean)

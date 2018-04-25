library(dplyr)
library(magrittr)

# Plots for iteration vs. accuracy on the Neural Network

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/NN Train vs Validation.jpg', width = 900, height = 600)
par(mfrow = c(2, 2))
NN0 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/BACKPROP_LOG.txt')

NN0 %$% plot(iteration, acc_trg, type = 'l', ylim = c(0, 1), col = 'blue', main = 'Backpropagation Accuracy by Iteration', xlab = 'Iteration', ylab = 'Accuracy')
NN0 %$% lines(iteration, acc_val, col = 'red')
abline(v = NN0[NN0_conv, 'iteration'], col = 'gray', lty = 2)
legend('bottomright', legend=c('Training Accuracy', 'Validation Accuracy'), col=c('blue', 'red'), lty = 1)

NN1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/RHC_LOG.txt')

NN1 %$% plot(iteration, acc_trg, type = 'l', ylim = c(0, 1), col = 'blue', main = 'Randomized Hill Climbing Accuracy by Iteration', xlab = 'Iteration', ylab = 'Accuracy')
NN1 %$% lines(iteration, acc_val, col = 'red')
abline(v = NN1[NN1_conv, 'iteration'], col = 'gray', lty = 2)
legend('bottomright', legend=c('Training Accuracy', 'Validation Accuracy'), col=c('blue', 'red'), lty = 1)

NN2.15 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/SA0.15_LOG.txt')
NN2.35 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/SA0.35_LOG.txt')
NN2.55 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/SA0.55_LOG.txt')
NN2.7 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/SA0.7_LOG.txt')
NN2.95 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/SA0.95_LOG.txt')

min(NN2.15$MSE_val,
NN2.35$MSE_val,
NN2.55$MSE_val,
NN2.7$MSE_val,
NN2.95$MSE_val)

NN2.35 %$% plot(iteration, acc_trg, type = 'l', ylim = c(0, 1), col = 'blue', main = 'Simulated Annealling Accuracy by Iteration', xlab = 'Iteration', ylab = 'Accuracy')
NN2.35 %$% lines(iteration, acc_val, col = 'red')
abline(v = NN2.35[NN2_conv, 'iteration'], col = 'gray', lty = 2)
legend('bottomright', legend=c('Training Accuracy', 'Validation Accuracy'), col=c('blue', 'red'), lty = 1)

NN3_10_10 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/GA_50_10_10_LOG.txt')
NN3_10_20 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/GA_50_10_20_LOG.txt')
NN3_20_10 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/GA_50_20_10_LOG.txt')
NN3_20_20 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/GA_50_20_20_LOG.txt')

min(NN3_10_10$MSE_val,
NN3_10_20$MSE_val,
NN3_20_10$MSE_val,
NN3_20_20$MSE_val)

NN3_20_10 %$% plot(iteration, acc_trg, type = 'l', ylim = c(0, 1), col = 'blue', main = 'Genetic Algorithm Accuracy by Iteration', xlab = 'Iteration', ylab = 'Accuracy')
NN3_20_10 %$% lines(iteration, acc_val, col = 'red')
abline(v = NN3_20_10[NN3_conv, 'iteration'], col = 'gray', lty = 2)
legend('bottomright', legend=c('Training Accuracy', 'Validation Accuracy'), col=c('blue', 'red'), lty = 1)

dev.off()

NN0[NN0_conv, 'acc_val']
NN1[NN1_conv, 'acc_val']
NN2.35[NN2_conv, 'acc_val']
NN3_20_10[NN3_conv, 'acc_val']

NN0[NN0_conv, 'acc_tst']
NN1[NN1_conv, 'acc_tst']
NN2.35[NN2_conv, 'acc_tst']
NN3_20_10[NN3_conv, 'acc_tst']

NN0[NN0_conv, 'elapsed']
NN1[NN1_conv, 'elapsed']
NN2.35[NN2_conv, 'elapsed']
NN3_20_10[NN3_conv, 'elapsed']


NN0 %$% max(acc_val)
NN0 %$% max(acc_tst)

NN1 %$% max(acc_val)
NN1 %$% max(acc_tst)

NN2.35 %$% max(acc_val)
NN2.35 %$% max(acc_tst)

NN3_20_10 %$% max(acc_val)
NN3_20_10 %$% max(acc_tst)

#par(mfrow = c(2, 2))

NN0$mse_delta <- (c(NN0$MSE_val, 0) - c(0, NN0$MSE_val))[1:600]
NN1$mse_delta <- (c(NN1$MSE_val, 0) - c(0, NN1$MSE_val))[1:600]
NN2.35$mse_delta <- (c(NN2.35$MSE_val, 0) - c(0, NN2.35$MSE_val))[1:600]
NN3_20_10$mse_delta <- (c(NN3_20_10$MSE_val, 0) - c(0, NN3_20_10$MSE_val))[1:600]

epsilon <- .000005

NN0_conv <- which(abs(NN0$mse_delta) < epsilon)[1]
NN1_conv <- which(abs(NN1$mse_delta) < epsilon)[1]
NN2_conv <- which(abs(NN2.35$mse_delta) < epsilon)[1]
NN3_conv <- which(abs(NN3_20_10$mse_delta) < epsilon)[1]

time_lim = c(0, max(c(NN0$elapsed, NN1$elapsed, NN2.35$elapsed, NN3_20_10$elapsed)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/NN Convergence.jpg', width = 900, height = 600)
par(mfrow = c(2, 2))
NN0 %$% plot(iteration, elapsed, ylim = time_lim, type = 'l', col = 'gray', main = 'Backpropagation Clock Time by Iteration', xlab = 'Iteration', ylab = 'Time Elapsed (s)')
abline(v = NN0[NN0_conv, 'iteration'], col = 'red', lty = 2)
NN1 %$% plot(iteration, elapsed, ylim = time_lim, type = 'l', col = 'gray', main = 'Randomized Hill Climbing Clock Time by Iteration', xlab = 'Iteration', ylab = 'Time Elapsed (s)')
abline(v = NN1[NN1_conv, 'iteration'], col = 'red', lty = 2)
NN2.35 %$% plot(iteration, elapsed, ylim = time_lim, type = 'l', col = 'gray', main = 'Simulated Annealling Clock Time by Iteration', xlab = 'Iteration', ylab = 'Time Elapsed (s)')
abline(v = NN2.35[NN2_conv, 'iteration'], col = 'red', lty = 2)
NN3_20_10 %$% plot(iteration, elapsed, ylim = time_lim, type = 'l', col = 'gray', main = 'Genetic Algorithm Clock Time by Iteration', xlab = 'Iteration', ylab = 'Time Elapsed (s)')
abline(v = NN3_20_10[NN3_conv, 'iteration'], col = 'red', lty = 2)

dev.off()

## Here begins the problem domain optimizations ##

soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  ga_10_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_10_1_LOG.txt')
  ga_10_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_10_2_LOG.txt')
  ga_10_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_10_3_LOG.txt')
  ga_10_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_10_4_LOG.txt')
  ga_10_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_10_5_LOG.txt')
  
  ga_10_10 <- rbind(ga_10_10_1,
                    ga_10_10_2,
                    ga_10_10_3,
                    ga_10_10_4,
                    ga_10_10_5)
  
  ga_10_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_10_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_30_1_LOG.txt')
  ga_10_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_30_2_LOG.txt')
  ga_10_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_30_3_LOG.txt')
  ga_10_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_30_4_LOG.txt')
  ga_10_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_30_5_LOG.txt')
  
  ga_10_30 <- rbind(ga_10_30_1,
                    ga_10_30_2,
                    ga_10_30_3,
                    ga_10_30_4,
                    ga_10_30_5)
  
  ga_10_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_10_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_50_1_LOG.txt')
  ga_10_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_50_2_LOG.txt')
  ga_10_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_50_3_LOG.txt')
  ga_10_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_50_4_LOG.txt')
  ga_10_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_10_50_5_LOG.txt')
  
  ga_10_50 <- rbind(ga_10_50_1,
                    ga_10_50_2,
                    ga_10_50_3,
                    ga_10_50_4,
                    ga_10_50_5)
  
  ga_10_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_10_1_LOG.txt')
  ga_30_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_10_2_LOG.txt')
  ga_30_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_10_3_LOG.txt')
  ga_30_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_10_4_LOG.txt')
  ga_30_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_10_5_LOG.txt')
  
  ga_30_10 <- rbind(ga_30_10_1,
                    ga_30_10_2,
                    ga_30_10_3,
                    ga_30_10_4,
                    ga_30_10_5)
  
  ga_30_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_30_1_LOG.txt')
  ga_30_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_30_2_LOG.txt')
  ga_30_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_30_3_LOG.txt')
  ga_30_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_30_4_LOG.txt')
  ga_30_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_30_5_LOG.txt')
  
  ga_30_30 <- rbind(ga_30_30_1,
                    ga_30_30_2,
                    ga_30_30_3,
                    ga_30_30_4,
                    ga_30_30_5)
  
  ga_30_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_50_1_LOG.txt')
  ga_30_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_50_2_LOG.txt')
  ga_30_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_50_3_LOG.txt')
  ga_30_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_50_4_LOG.txt')
  ga_30_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_30_50_5_LOG.txt')
  
  ga_30_50 <- rbind(ga_30_50_1,
                    ga_30_50_2,
                    ga_30_50_3,
                    ga_30_50_4,
                    ga_30_50_5)
  
  ga_30_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_10_1_LOG.txt')
  ga_50_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_10_2_LOG.txt')
  ga_50_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_10_3_LOG.txt')
  ga_50_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_10_4_LOG.txt')
  ga_50_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_10_5_LOG.txt')
  
  ga_50_10 <- rbind(ga_50_10_1,
                    ga_50_10_2,
                    ga_50_10_3,
                    ga_50_10_4,
                    ga_50_10_5)
  
  ga_50_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_30_1_LOG.txt')
  ga_50_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_30_2_LOG.txt')
  ga_50_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_30_3_LOG.txt')
  ga_50_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_30_4_LOG.txt')
  ga_50_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_30_5_LOG.txt')
  
  ga_50_30 <- rbind(ga_50_30_1,
                    ga_50_30_2,
                    ga_50_30_3,
                    ga_50_30_4,
                    ga_50_30_5)
  
  ga_50_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_50_1_LOG.txt')
  ga_50_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_50_2_LOG.txt')
  ga_50_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_50_3_LOG.txt')
  ga_50_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_50_4_LOG.txt')
  ga_50_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_GA100_50_50_5_LOG.txt')
  
  ga_50_50 <- rbind(ga_50_50_1,
                    ga_50_50_2,
                    ga_50_50_3,
                    ga_50_50_4,
                    ga_50_50_5)
  
  ga_50_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(ga_10_10$fitness,
                   ga_10_30$fitness,
                   ga_10_50$fitness,
                   ga_30_10$fitness,
                   ga_30_30$fitness,
                   ga_30_50$fitness,
                   ga_50_10$fitness,
                   ga_50_30$fitness,
                   ga_50_50$fitness)),
                 max(c(ga_10_10$fitness,
                 ga_10_30$fitness,
                 ga_10_50$fitness,
                 ga_30_10$fitness,
                 ga_30_30$fitness,
                 ga_30_50$fitness,
                 ga_50_10$fitness,
                 ga_50_30$fitness,
                 ga_50_50$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop GA Iter.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

# By # iterations
ga_10_10 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - Genetic Algorithm', xlab = 'Iterations', ylab = 'Fitness Score')
ga_10_30 %$% lines(iterations, fitness, col = 'organe', lty = 2)
ga_10_50 %$% lines(iterations, fitness, col = 'orange', lty = 3)
ga_30_10 %$% lines(iterations, fitness, col = 'darkgreen', lty = 1)
ga_30_30 %$% lines(iterations, fitness, col = 'darkgreen', lty = 2)
ga_30_50 %$% lines(iterations, fitness, col = 'darkgreen', lty = 3)
ga_50_10 %$% lines(iterations, fitness, col = 'blue', lty = 1)
ga_50_30 %$% lines(iterations, fitness, col = 'blue', lty = 2)
ga_50_50 %$% lines(iterations, fitness, col = 'blue', lty = 3)

legend('topleft', c('Crossover 10 Mutate 10',
                    'Crossover 10 Mutate 30',
                    'Crossover 10 Mutate 50',
                    'Crossover 30 Mutate 10',
                    'Crossover 30 Mutate 30',
                    'Crossover 30 Mutate 50',
                    'Crossover 50 Mutate 10',
                    'Crossover 50 Mutate 30',
                    'Crossover 50 Mutate 50'), lty = rep(c("solid", "dotted", "dashed"), 3), col = c(rep('orange', 3), rep('darkgreen', 3), rep('blue', 3)))


dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop GA Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

# By # evaluations
ga_10_10 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - Genetic Algorithm', xlab = 'Function Evaluations', ylab = 'Fitness Score')
ga_10_30 %$% lines(fevals, fitness, col = 'orange', lty = 2)
ga_10_50 %$% lines(fevals, fitness, col = 'orange', lty = 3)
ga_30_10 %$% lines(fevals, fitness, col = 'darkgreen', lty = 1)
ga_30_30 %$% lines(fevals, fitness, col = 'darkgreen', lty = 2)
ga_30_50 %$% lines(fevals, fitness, col = 'darkgreen', lty = 3)
ga_50_10 %$% lines(fevals, fitness, col = 'blue', lty = 1)
ga_50_30 %$% lines(fevals, fitness, col = 'blue', lty = 2)
ga_50_50 %$% lines(fevals, fitness, col = 'blue', lty = 3)
legend('topleft', c('Crossover 10 Mutate 10',
                        'Crossover 10 Mutate 30',
                        'Crossover 10 Mutate 50',
                        'Crossover 30 Mutate 10',
                        'Crossover 30 Mutate 30',
                        'Crossover 30 Mutate 50',
                        'Crossover 50 Mutate 10',
                        'Crossover 50 Mutate 30',
                        'Crossover 50 Mutate 50'), lty = rep(c("solid", "dotted", "dashed"), 3), col = c(rep('orange', 3), rep('darkgreen', 3), rep('blue', 3)))

dev.off()





soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  ga_10_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_10_1_LOG.txt')
  ga_10_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_10_2_LOG.txt')
  ga_10_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_10_3_LOG.txt')
  ga_10_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_10_4_LOG.txt')
  ga_10_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_10_5_LOG.txt')
  
  ga_10_10 <- rbind(ga_10_10_1,
                    ga_10_10_2,
                    ga_10_10_3,
                    ga_10_10_4,
                    ga_10_10_5)
  
  ga_10_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_10_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_30_1_LOG.txt')
  ga_10_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_30_2_LOG.txt')
  ga_10_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_30_3_LOG.txt')
  ga_10_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_30_4_LOG.txt')
  ga_10_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_30_5_LOG.txt')
  
  ga_10_30 <- rbind(ga_10_30_1,
                    ga_10_30_2,
                    ga_10_30_3,
                    ga_10_30_4,
                    ga_10_30_5)
  
  ga_10_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_10_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_50_1_LOG.txt')
  ga_10_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_50_2_LOG.txt')
  ga_10_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_50_3_LOG.txt')
  ga_10_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_50_4_LOG.txt')
  ga_10_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_10_50_5_LOG.txt')
  
  ga_10_50 <- rbind(ga_10_50_1,
                    ga_10_50_2,
                    ga_10_50_3,
                    ga_10_50_4,
                    ga_10_50_5)
  
  ga_10_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_10_1_LOG.txt')
  ga_30_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_10_2_LOG.txt')
  ga_30_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_10_3_LOG.txt')
  ga_30_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_10_4_LOG.txt')
  ga_30_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_10_5_LOG.txt')
  
  ga_30_10 <- rbind(ga_30_10_1,
                    ga_30_10_2,
                    ga_30_10_3,
                    ga_30_10_4,
                    ga_30_10_5)
  
  ga_30_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_30_1_LOG.txt')
  ga_30_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_30_2_LOG.txt')
  ga_30_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_30_3_LOG.txt')
  ga_30_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_30_4_LOG.txt')
  ga_30_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_30_5_LOG.txt')
  
  ga_30_30 <- rbind(ga_30_30_1,
                    ga_30_30_2,
                    ga_30_30_3,
                    ga_30_30_4,
                    ga_30_30_5)
  
  ga_30_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_50_1_LOG.txt')
  ga_30_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_50_2_LOG.txt')
  ga_30_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_50_3_LOG.txt')
  ga_30_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_50_4_LOG.txt')
  ga_30_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_30_50_5_LOG.txt')
  
  ga_30_50 <- rbind(ga_30_50_1,
                    ga_30_50_2,
                    ga_30_50_3,
                    ga_30_50_4,
                    ga_30_50_5)
  
  ga_30_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_10_1_LOG.txt')
  ga_50_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_10_2_LOG.txt')
  ga_50_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_10_3_LOG.txt')
  ga_50_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_10_4_LOG.txt')
  ga_50_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_10_5_LOG.txt')
  
  ga_50_10 <- rbind(ga_50_10_1,
                    ga_50_10_2,
                    ga_50_10_3,
                    ga_50_10_4,
                    ga_50_10_5)
  
  ga_50_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_30_1_LOG.txt')
  ga_50_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_30_2_LOG.txt')
  ga_50_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_30_3_LOG.txt')
  ga_50_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_30_4_LOG.txt')
  ga_50_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_30_5_LOG.txt')
  
  ga_50_30 <- rbind(ga_50_30_1,
                    ga_50_30_2,
                    ga_50_30_3,
                    ga_50_30_4,
                    ga_50_30_5)
  
  ga_50_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_50_1_LOG.txt')
  ga_50_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_50_2_LOG.txt')
  ga_50_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_50_3_LOG.txt')
  ga_50_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_50_4_LOG.txt')
  ga_50_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_GA100_50_50_5_LOG.txt')
  
  ga_50_50 <- rbind(ga_50_50_1,
                    ga_50_50_2,
                    ga_50_50_3,
                    ga_50_50_4,
                    ga_50_50_5)
  
  ga_50_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

par(mfrow = c(1, 1))
fit_lim <- c(min(c(ga_10_10$fitness,
                   ga_10_30$fitness,
                   ga_10_50$fitness,
                   ga_30_10$fitness,
                   ga_30_30$fitness,
                   ga_30_50$fitness,
                   ga_50_10$fitness,
                   ga_50_30$fitness,
                   ga_50_50$fitness)),
             max(c(ga_10_10$fitness,
                   ga_10_30$fitness,
                   ga_10_50$fitness,
                   ga_30_10$fitness,
                   ga_30_30$fitness,
                   ga_30_50$fitness,
                   ga_50_10$fitness,
                   ga_50_30$fitness,
                   ga_50_50$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP GA Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

# By # iterations
ga_10_10 %$% plot(iterations, fitness, type = 'l', col = 'gold', ylim = fit_lim, main = 'TSP - 100 Destinations  - Genetic Algorithm', xlab = 'Iterations', ylab = 'Fitness Score')
ga_10_30 %$% lines(iterations, fitness, col = 'gold', lty = 2)
ga_10_50 %$% lines(iterations, fitness, col = 'gold', lty = 3)
ga_30_10 %$% lines(iterations, fitness, col = 'darkgreen', lty = 1)
ga_30_30 %$% lines(iterations, fitness, col = 'darkgreen', lty = 2)
ga_30_50 %$% lines(iterations, fitness, col = 'darkgreen', lty = 3)
ga_50_10 %$% lines(iterations, fitness, col = 'blue', lty = 1)
ga_50_30 %$% lines(iterations, fitness, col = 'blue', lty = 2)
ga_50_50 %$% lines(iterations, fitness, col = 'blue', lty = 3)

legend('topleft', c('Crossover 10 Mutate 10',
                    'Crossover 10 Mutate 30',
                    'Crossover 10 Mutate 50',
                    'Crossover 30 Mutate 10',
                    'Crossover 30 Mutate 30',
                    'Crossover 30 Mutate 50',
                    'Crossover 50 Mutate 10',
                    'Crossover 50 Mutate 30',
                    'Crossover 50 Mutate 50'), lty = rep(c("solid", "dotted", "dashed"), 3), col = c(rep('orange', 3), rep('darkgreen', 3), rep('blue', 3)))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP GA Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))
# By # evaluations
ga_10_10 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 1000 Destinations - Genetic Algorithm', xlab = 'Function Evaluations', ylab = 'Fitness Score')
ga_10_30 %$% lines(fevals, fitness, col = 'orange', lty = 2)
ga_10_50 %$% lines(fevals, fitness, col = 'orange', lty = 3)
ga_30_10 %$% lines(fevals, fitness, col = 'darkgreen', lty = 1)
ga_30_30 %$% lines(fevals, fitness, col = 'darkgreen', lty = 2)
ga_30_50 %$% lines(fevals, fitness, col = 'darkgreen', lty = 3)
ga_50_10 %$% lines(fevals, fitness, col = 'blue', lty = 1)
ga_50_30 %$% lines(fevals, fitness, col = 'blue', lty = 2)
ga_50_50 %$% lines(fevals, fitness, col = 'blue', lty = 3)

legend('topleft', c('Crossover 10 Mutate 10',
                    'Crossover 10 Mutate 30',
                    'Crossover 10 Mutate 50',
                    'Crossover 30 Mutate 10',
                    'Crossover 30 Mutate 30',
                    'Crossover 30 Mutate 50',
                    'Crossover 50 Mutate 10',
                    'Crossover 50 Mutate 30',
                    'Crossover 50 Mutate 50'), lty = rep(c("solid", "dotted", "dashed"), 3), col = c(rep('orange', 3), rep('darkgreen', 3), rep('blue', 3)))

dev.off()



soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  ga_10_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_10_1_LOG.txt')
  ga_10_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_10_2_LOG.txt')
  ga_10_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_10_3_LOG.txt')
  ga_10_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_10_4_LOG.txt')
  ga_10_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_10_5_LOG.txt')
  
  ga_10_10 <- rbind(ga_10_10_1,
                    ga_10_10_2,
                    ga_10_10_3,
                    ga_10_10_4,
                    ga_10_10_5)
  
  ga_10_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_10_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_30_1_LOG.txt')
  ga_10_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_30_2_LOG.txt')
  ga_10_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_30_3_LOG.txt')
  ga_10_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_30_4_LOG.txt')
  ga_10_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_30_5_LOG.txt')
  
  ga_10_30 <- rbind(ga_10_30_1,
                    ga_10_30_2,
                    ga_10_30_3,
                    ga_10_30_4,
                    ga_10_30_5)
  
  ga_10_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_10_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_50_1_LOG.txt')
  ga_10_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_50_2_LOG.txt')
  ga_10_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_50_3_LOG.txt')
  ga_10_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_50_4_LOG.txt')
  ga_10_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_10_50_5_LOG.txt')
  
  ga_10_50 <- rbind(ga_10_50_1,
                    ga_10_50_2,
                    ga_10_50_3,
                    ga_10_50_4,
                    ga_10_50_5)
  
  ga_10_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_10_1_LOG.txt')
  ga_30_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_10_2_LOG.txt')
  ga_30_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_10_3_LOG.txt')
  ga_30_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_10_4_LOG.txt')
  ga_30_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_10_5_LOG.txt')
  
  ga_30_10 <- rbind(ga_30_10_1,
                    ga_30_10_2,
                    ga_30_10_3,
                    ga_30_10_4,
                    ga_30_10_5)
  
  ga_30_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_30_1_LOG.txt')
  ga_30_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_30_2_LOG.txt')
  ga_30_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_30_3_LOG.txt')
  ga_30_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_30_4_LOG.txt')
  ga_30_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_30_5_LOG.txt')
  
  ga_30_30 <- rbind(ga_30_30_1,
                    ga_30_30_2,
                    ga_30_30_3,
                    ga_30_30_4,
                    ga_30_30_5)
  
  ga_30_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_30_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_50_1_LOG.txt')
  ga_30_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_50_2_LOG.txt')
  ga_30_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_50_3_LOG.txt')
  ga_30_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_50_4_LOG.txt')
  ga_30_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_30_50_5_LOG.txt')
  
  ga_30_50 <- rbind(ga_30_50_1,
                    ga_30_50_2,
                    ga_30_50_3,
                    ga_30_50_4,
                    ga_30_50_5)
  
  ga_30_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_10_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_10_1_LOG.txt')
  ga_50_10_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_10_2_LOG.txt')
  ga_50_10_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_10_3_LOG.txt')
  ga_50_10_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_10_4_LOG.txt')
  ga_50_10_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_10_5_LOG.txt')
  
  ga_50_10 <- rbind(ga_50_10_1,
                    ga_50_10_2,
                    ga_50_10_3,
                    ga_50_10_4,
                    ga_50_10_5)
  
  ga_50_10 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_30_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_30_1_LOG.txt')
  ga_50_30_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_30_2_LOG.txt')
  ga_50_30_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_30_3_LOG.txt')
  ga_50_30_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_30_4_LOG.txt')
  ga_50_30_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_30_5_LOG.txt')
  
  ga_50_30 <- rbind(ga_50_30_1,
                    ga_50_30_2,
                    ga_50_30_3,
                    ga_50_30_4,
                    ga_50_30_5)
  
  ga_50_30 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  ga_50_50_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_50_1_LOG.txt')
  ga_50_50_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_50_2_LOG.txt')
  ga_50_50_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_50_3_LOG.txt')
  ga_50_50_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_50_4_LOG.txt')
  ga_50_50_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_GA100_50_50_5_LOG.txt')
  
  ga_50_50 <- rbind(ga_50_50_1,
                    ga_50_50_2,
                    ga_50_50_3,
                    ga_50_50_4,
                    ga_50_50_5)
  
  ga_50_50 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

par(mfrow = c(1, 1))
fit_lim <- c(min(c(ga_10_10$fitness,
                   ga_10_30$fitness,
                   ga_10_50$fitness,
                   ga_30_10$fitness,
                   ga_30_30$fitness,
                   ga_30_50$fitness,
                   ga_50_10$fitness,
                   ga_50_30$fitness,
                   ga_50_50$fitness)),
             max(c(ga_10_10$fitness,
                   ga_10_30$fitness,
                   ga_10_50$fitness,
                   ga_30_10$fitness,
                   ga_30_30$fitness,
                   ga_30_50$fitness,
                   ga_50_10$fitness,
                   ga_50_30$fitness,
                   ga_50_50$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks GA Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

# By # iterations
ga_10_10 %$% plot(iterations, fitness, type = 'l', col = 'gold', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - Genetic Algorithm', xlab = 'Iterations', ylab = 'Fitness Score')
ga_10_30 %$% lines(iterations, fitness, col = 'gold', lty = 2)
ga_10_50 %$% lines(iterations, fitness, col = 'gold', lty = 3)
ga_30_10 %$% lines(iterations, fitness, col = 'darkgreen', lty = 1)
ga_30_30 %$% lines(iterations, fitness, col = 'darkgreen', lty = 2)
ga_30_50 %$% lines(iterations, fitness, col = 'darkgreen', lty = 3)
ga_50_10 %$% lines(iterations, fitness, col = 'blue', lty = 1)
ga_50_30 %$% lines(iterations, fitness, col = 'blue', lty = 2)
ga_50_50 %$% lines(iterations, fitness, col = 'blue', lty = 3)

legend('topleft', c('Crossover 10 Mutate 10',
                    'Crossover 10 Mutate 30',
                    'Crossover 10 Mutate 50',
                    'Crossover 30 Mutate 10',
                    'Crossover 30 Mutate 30',
                    'Crossover 30 Mutate 50',
                    'Crossover 50 Mutate 10',
                    'Crossover 50 Mutate 30',
                    'Crossover 50 Mutate 50'), lty = rep(c("solid", "dotted", "dashed"), 3), col = c(rep('orange', 3), rep('darkgreen', 3), rep('blue', 3)))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous GA Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

# By # evaluations
ga_10_10 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - Genetic Algorithm', xlab = 'Function Evaluations', ylab = 'Fitness Score')
ga_10_30 %$% lines(fevals, fitness, col = 'orange', lty = 2)
ga_10_50 %$% lines(fevals, fitness, col = 'orange', lty = 3)
ga_30_10 %$% lines(fevals, fitness, col = 'darkgreen', lty = 1)
ga_30_30 %$% lines(fevals, fitness, col = 'darkgreen', lty = 2)
ga_30_50 %$% lines(fevals, fitness, col = 'darkgreen', lty = 3)
ga_50_10 %$% lines(fevals, fitness, col = 'blue', lty = 1)
ga_50_30 %$% lines(fevals, fitness, col = 'blue', lty = 2)
ga_50_50 %$% lines(fevals, fitness, col = 'blue', lty = 3)

legend('topleft', c('Crossover 10 Mutate 10',
                    'Crossover 10 Mutate 30',
                    'Crossover 10 Mutate 50',
                    'Crossover 30 Mutate 10',
                    'Crossover 30 Mutate 30',
                    'Crossover 30 Mutate 50',
                    'Crossover 50 Mutate 10',
                    'Crossover 50 Mutate 30',
                    'Crossover 50 Mutate 50'), lty = rep(c("solid", "dotted", "dashed"), 3), col = c(rep('orange', 3), rep('darkgreen', 3), rep('blue', 3)))

dev.off()






#### MIMIC #####






soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  mimic_.1_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_1_LOG.txt')
  mimic_.1_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_2_LOG.txt')
  mimic_.1_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_3_LOG.txt')
  mimic_.1_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_4_LOG.txt')
  mimic_.1_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_5_LOG.txt')
  
  mimic_.1 <- rbind(mimic_.1_1,
                    mimic_.1_2,
                    mimic_.1_3,
                    mimic_.1_4,
                    mimic_.1_5)
  
  mimic_.1 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.3_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.3_1_LOG.txt')
  mimic_.3_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.3_2_LOG.txt')
  mimic_.3_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.3_3_LOG.txt')
  mimic_.3_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.3_4_LOG.txt')
  mimic_.3_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.3_5_LOG.txt')
  
  mimic_.3 <- rbind(mimic_.3_1,
                    mimic_.3_2,
                    mimic_.3_3,
                    mimic_.3_4,
                    mimic_.3_5)
  
  mimic_.3 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.5_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.5_1_LOG.txt')
  mimic_.5_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.5_2_LOG.txt')
  mimic_.5_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.5_3_LOG.txt')
  mimic_.5_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.5_4_LOG.txt')
  mimic_.5_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.5_5_LOG.txt')
  
  mimic_.5 <- rbind(mimic_.5_1,
                    mimic_.5_2,
                    mimic_.5_3,
                    mimic_.5_4,
                    mimic_.5_5)
  
  mimic_.5 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.7_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.7_1_LOG.txt')
  mimic_.7_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.7_2_LOG.txt')
  mimic_.7_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.7_3_LOG.txt')
  mimic_.7_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.7_4_LOG.txt')
  mimic_.7_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.7_5_LOG.txt')
  
  mimic_.7 <- rbind(mimic_.7_1,
                    mimic_.7_2,
                    mimic_.7_3,
                    mimic_.7_4,
                    mimic_.7_5)
  
  mimic_.7 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.9_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.9_1_LOG.txt')
  mimic_.9_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.9_2_LOG.txt')
  mimic_.9_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.9_3_LOG.txt')
  mimic_.9_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.9_4_LOG.txt')
  mimic_.9_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.9_5_LOG.txt')
  
  mimic_.9 <- rbind(mimic_.9_1,
                    mimic_.9_2,
                    mimic_.9_3,
                    mimic_.9_4,
                    mimic_.9_5)
  
  mimic_.9 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(mimic_.1$fitness,
                   mimic_.3$fitness,
                   mimic_.5$fitness,
                   mimic_.7$fitness,
                   mimic_.9$fitness)),
             max(c(mimic_.1$fitness,
                   mimic_.3$fitness,
                   mimic_.5$fitness,
                   mimic_.7$fitness,
                   mimic_.9$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks MIMIC Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - MIMIC', xlab = 'Iterations', ylab = 'Fitness Score')
mimic_.3 %$% lines(iterations, fitness, col = 'blue')
mimic_.5 %$% lines(iterations, fitness, col = 'darkgreen')
mimic_.7 %$% lines(iterations, fitness, col = 'lightblue')
mimic_.9 %$% lines(iterations, fitness, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks MIMIC Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - MIMIC', xlab = 'Function Evaluations', ylab = 'Fitness Score')
mimic_.3 %$% lines(fevals, fitness, col = 'blue')
mimic_.5 %$% lines(fevals, fitness, col = 'darkgreen')
mimic_.7 %$% lines(fevals, fitness, col = 'lightblue')
mimic_.9 %$% lines(fevals, fitness, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

time_lim <- c(min(c(mimic_.1$time,
                    mimic_.3$time,
                    mimic_.5$time,
                    mimic_.7$time,
                    mimic_.9$time)),
              max(c(mimic_.1$time,
                    mimic_.3$time,
                    mimic_.5$time,
                    mimic_.7$time,
                    mimic_.9$time)))


jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks MIMIC Time.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(iterations, time, type = 'l', col = 'orange', ylim = time_lim, main = 'Continuous Peaks - N = 100 & T = 49 - MIMIC - Time Elapsed', xlab = 'Iterations', ylab = 'Elapsed Time (s)')
mimic_.3 %$% lines(iterations, time, col = 'blue')
mimic_.5 %$% lines(iterations, time, col = 'darkgreen')
mimic_.7 %$% lines(iterations, time, col = 'lightblue')
mimic_.9 %$% lines(iterations, time, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()







soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  mimic_.1_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_1_LOG.txt')
  mimic_.1_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_2_LOG.txt')
  mimic_.1_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_3_LOG.txt')
  mimic_.1_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_4_LOG.txt')
  mimic_.1_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_5_LOG.txt')
  
  mimic_.1 <- rbind(mimic_.1_1,
                    mimic_.1_2,
                    mimic_.1_3,
                    mimic_.1_4,
                    mimic_.1_5)
  
  mimic_.1 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.3_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.3_1_LOG.txt')
  mimic_.3_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.3_2_LOG.txt')
  mimic_.3_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.3_3_LOG.txt')
  mimic_.3_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.3_4_LOG.txt')
  mimic_.3_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.3_5_LOG.txt')
  
  mimic_.3 <- rbind(mimic_.3_1,
                    mimic_.3_2,
                    mimic_.3_3,
                    mimic_.3_4,
                    mimic_.3_5)
  
  mimic_.3 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.5_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.5_1_LOG.txt')
  mimic_.5_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.5_2_LOG.txt')
  mimic_.5_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.5_3_LOG.txt')
  mimic_.5_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.5_4_LOG.txt')
  mimic_.5_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.5_5_LOG.txt')
  
  mimic_.5 <- rbind(mimic_.5_1,
                    mimic_.5_2,
                    mimic_.5_3,
                    mimic_.5_4,
                    mimic_.5_5)
  
  mimic_.5 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.7_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.7_1_LOG.txt')
  mimic_.7_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.7_2_LOG.txt')
  mimic_.7_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.7_3_LOG.txt')
  mimic_.7_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.7_4_LOG.txt')
  mimic_.7_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.7_5_LOG.txt')
  
  mimic_.7 <- rbind(mimic_.7_1,
                    mimic_.7_2,
                    mimic_.7_3,
                    mimic_.7_4,
                    mimic_.7_5)
  
  mimic_.7 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.9_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.9_1_LOG.txt')
  mimic_.9_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.9_2_LOG.txt')
  mimic_.9_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.9_3_LOG.txt')
  mimic_.9_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.9_4_LOG.txt')
  mimic_.9_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.9_5_LOG.txt')
  
  mimic_.9 <- rbind(mimic_.9_1,
                    mimic_.9_2,
                    mimic_.9_3,
                    mimic_.9_4,
                    mimic_.9_5)
  
  mimic_.9 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(mimic_.1$fitness,
                   mimic_.3$fitness,
                   mimic_.5$fitness,
                   mimic_.7$fitness,
                   mimic_.9$fitness)),
             max(c(mimic_.1$fitness,
                   mimic_.3$fitness,
                   mimic_.5$fitness,
                   mimic_.7$fitness,
                   mimic_.9$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP MIMIC Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 100 Destinations - MIMIC', xlab = 'Iterations', ylab = 'Fitness Score')
mimic_.3 %$% lines(iterations, fitness, col = 'blue')
mimic_.5 %$% lines(iterations, fitness, col = 'darkgreen')
mimic_.7 %$% lines(iterations, fitness, col = 'lightblue')
mimic_.9 %$% lines(iterations, fitness, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP MIMIC Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 100 Destinations - MIMIC', xlab = 'Function Evaluations', ylab = 'Fitness Score')
mimic_.3 %$% lines(fevals, fitness, col = 'blue')
mimic_.5 %$% lines(fevals, fitness, col = 'darkgreen')
mimic_.7 %$% lines(fevals, fitness, col = 'lightblue')
mimic_.9 %$% lines(fevals, fitness, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

time_lim <- c(min(c(mimic_.1$time,
                    mimic_.3$time,
                    mimic_.5$time,
                    mimic_.7$time,
                    mimic_.9$time)),
              max(c(mimic_.1$time,
                    mimic_.3$time,
                    mimic_.5$time,
                    mimic_.7$time,
                    mimic_.9$time)))


jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP MIMIC Time.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(iterations, time, type = 'l', col = 'orange', ylim = time_lim, main = 'TSP - 100 Destinations - MIMIC - Time Elapsed', xlab = 'Iterations', ylab = 'Elapsed Time (s)')
mimic_.3 %$% lines(iterations, time, col = 'blue')
mimic_.5 %$% lines(iterations, time, col = 'darkgreen')
mimic_.7 %$% lines(iterations, time, col = 'lightblue')
mimic_.9 %$% lines(iterations, time, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()





soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  mimic_.1_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_1_LOG.txt')
  mimic_.1_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_2_LOG.txt')
  mimic_.1_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_3_LOG.txt')
  mimic_.1_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_4_LOG.txt')
  mimic_.1_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_5_LOG.txt')
  
  mimic_.1 <- rbind(mimic_.1_1,
                    mimic_.1_2,
                    mimic_.1_3,
                    mimic_.1_4,
                    mimic_.1_5)
  
  mimic_.1 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.3_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.3_1_LOG.txt')
  mimic_.3_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.3_2_LOG.txt')
  mimic_.3_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.3_3_LOG.txt')
  mimic_.3_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.3_4_LOG.txt')
  #mimic_.3_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.3_5_LOG.txt')
  
  mimic_.3 <- rbind(mimic_.3_1,
                    mimic_.3_2,
                    mimic_.3_3,
                    mimic_.3_4)#,
                    #mimic_.3_5)
  
  mimic_.3 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.5_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.5_1_LOG.txt')
  mimic_.5_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.5_2_LOG.txt')
  mimic_.5_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.5_3_LOG.txt')
  mimic_.5_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.5_4_LOG.txt')
  #mimic_.5_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.5_5_LOG.txt')
  
  mimic_.5 <- rbind(mimic_.5_1,
                    mimic_.5_2,
                    mimic_.5_3,
                    mimic_.5_4)#,
                    #mimic_.5_5)
  
  mimic_.5 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.7_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.7_1_LOG.txt')
  mimic_.7_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.7_2_LOG.txt')
  mimic_.7_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.7_3_LOG.txt')
  mimic_.7_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.7_4_LOG.txt')
  #mimic_.7_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.7_5_LOG.txt')
  
  mimic_.7 <- rbind(mimic_.7_1,
                    mimic_.7_2,
                    mimic_.7_3,
                    mimic_.7_4)#,
                    #mimic_.7_5)
  
  mimic_.7 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  mimic_.9_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.9_1_LOG.txt')
  mimic_.9_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.9_2_LOG.txt')
  mimic_.9_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.9_3_LOG.txt')
  mimic_.9_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.9_4_LOG.txt')
  #mimic_.9_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.9_5_LOG.txt')
  
  mimic_.9 <- rbind(mimic_.9_1,
                    mimic_.9_2,
                    mimic_.9_3,
                    mimic_.9_4)#,
                    #mimic_.9_5)
  
  mimic_.9 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(mimic_.1$fitness,
                   mimic_.3$fitness,
                   mimic_.5$fitness,
                   mimic_.7$fitness,
                   mimic_.9$fitness)),
             max(c(mimic_.1$fitness,
                   mimic_.3$fitness,
                   mimic_.5$fitness,
                   mimic_.7$fitness,
                   mimic_.9$fitness)))
jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop MIMIC Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - MIMIC', xlab = 'Iterations', ylab = 'Fitness Score')
mimic_.3 %$% lines(iterations, fitness, col = 'blue')
mimic_.5 %$% lines(iterations, fitness, col = 'darkgreen')
mimic_.7 %$% lines(iterations, fitness, col = 'lightblue')
mimic_.9 %$% lines(iterations, fitness, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop MIMIC Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - MIMIC', xlab = 'Function Evaluations', ylab = 'Fitness Score')
mimic_.3 %$% lines(fevals, fitness, col = 'blue')
mimic_.5 %$% lines(fevals, fitness, col = 'darkgreen')
mimic_.7 %$% lines(fevals, fitness, col = 'lightblue')
mimic_.9 %$% lines(fevals, fitness, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

time_lim <- c(min(c(mimic_.1$time,
                    mimic_.3$time,
                    mimic_.5$time,
                    mimic_.7$time,
                    mimic_.9$time)),
              max(c(mimic_.1$time,
                    mimic_.3$time,
                    mimic_.5$time,
                    mimic_.7$time,
                    mimic_.9$time)))


jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop MIMIC Time.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

mimic_.1 %$% plot(iterations, time, type = 'l', col = 'orange', ylim = time_lim, main = 'Flip Flop - 1000 Bits - MIMIC - Time Elapsed', xlab = 'Iterations', ylab = 'Elapsed Time (s)')
mimic_.3 %$% lines(iterations, time, col = 'blue')
mimic_.5 %$% lines(iterations, time, col = 'darkgreen')
mimic_.7 %$% lines(iterations, time, col = 'lightblue')
mimic_.9 %$% lines(iterations, time, col = 'gray')

legend('bottomright', c('Fitness Threshold: 0.1', 'Fitness Threshold: 0.3', 'Fitness Threshold: 0.5', 'Fitness Threshold: 0.7', 'Fitness Threshold: 0.9'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()



### RHC ####




soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  RHC_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_1_LOG.txt')
  RHC_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_2_LOG.txt')
  RHC_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_3_LOG.txt')
  RHC_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_4_LOG.txt')
  RHC_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_MIMIC100_50_0.1_5_LOG.txt')
  
  RHC <- rbind(RHC_1,
               RHC_2,
               RHC_3,
               RHC_4,
               RHC_5)
  
  RHC %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(RHC$fitness), max(RHC$fitness))
      
jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop RHC Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

RHC %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - RHC', xlab = 'Iterations', ylab = 'Fitness Score')

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop RHC Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

RHC %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - RHC', xlab = 'Function Evaluations', ylab = 'Fitness Score')

dev.off()







soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  RHC_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_1_LOG.txt')
  RHC_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_2_LOG.txt')
  RHC_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_3_LOG.txt')
  RHC_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_4_LOG.txt')
  RHC_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_MIMIC100_50_0.1_5_LOG.txt')
  
  RHC <- rbind(RHC_1,
               RHC_2,
               RHC_3,
               RHC_4,
               RHC_5)
  
  RHC %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(RHC$fitness), max(RHC$fitness))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks RHC Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

RHC %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - RHC', xlab = 'Iterations', ylab = 'Fitness Score')

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks RHC Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

RHC %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - RHC', xlab = 'Function Evaluations', ylab = 'Fitness Score')

dev.off()












soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  RHC_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_1_LOG.txt')
  RHC_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_2_LOG.txt')
  RHC_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_3_LOG.txt')
  RHC_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_4_LOG.txt')
  RHC_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_MIMIC100_50_0.1_5_LOG.txt')
  
  RHC <- rbind(RHC_1,
               RHC_2,
               RHC_3,
               RHC_4,
               RHC_5)
  
  RHC %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(RHC$fitness), max(RHC$fitness))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP RHC Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

RHC %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 100 Destinations - RHC', xlab = 'Iterations', ylab = 'Fitness Score')

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP RHC Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

RHC %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 100 Destinations - RHC', xlab = 'Function Evaluations', ylab = 'Fitness Score')

dev.off()



#### SA ######



soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  sa_.15_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.15_1_LOG.txt')
  sa_.15_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.15_2_LOG.txt')
  sa_.15_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.15_3_LOG.txt')
  sa_.15_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.15_4_LOG.txt')
  sa_.15_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.15_5_LOG.txt')
  
  sa_.15 <- rbind(sa_.15_1,
                  sa_.15_2,
                  sa_.15_3,
                  sa_.15_4,
                  sa_.15_5)
  
  sa_.15 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.35_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.35_1_LOG.txt')
  sa_.35_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.35_2_LOG.txt')
  sa_.35_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.35_3_LOG.txt')
  sa_.35_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.35_4_LOG.txt')
  sa_.35_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.35_5_LOG.txt')
  
  sa_.35 <- rbind(sa_.35_1,
                  sa_.35_2,
                  sa_.35_3,
                  sa_.35_4,
                  sa_.35_5)
  
  sa_.35 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.55_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.55_1_LOG.txt')
  sa_.55_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.55_2_LOG.txt')
  sa_.55_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.55_3_LOG.txt')
  sa_.55_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.55_4_LOG.txt')
  sa_.55_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.55_5_LOG.txt')
  
  sa_.55 <- rbind(sa_.55_1,
                  sa_.55_2,
                  sa_.55_3,
                  sa_.55_4,
                  sa_.55_5)
  
  sa_.55 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.75_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.75_1_LOG.txt')
  sa_.75_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.75_2_LOG.txt')
  sa_.75_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.75_3_LOG.txt')
  sa_.75_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.75_4_LOG.txt')
  sa_.75_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.75_5_LOG.txt')
  
  sa_.75 <- rbind(sa_.75_1,
                  sa_.75_2,
                  sa_.75_3,
                  sa_.75_4,
                  sa_.75_5)
  
  sa_.75 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.95_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.95_1_LOG.txt')
  sa_.95_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.95_2_LOG.txt')
  sa_.95_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.95_3_LOG.txt')
  sa_.95_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.95_4_LOG.txt')
  sa_.95_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/FLIPFLOP/FLIPFLOP_SA0.95_5_LOG.txt')
  
  sa_.95 <- rbind(sa_.95_1,
                  sa_.95_2,
                  sa_.95_3,
                  sa_.95_4,
                  sa_.95_5)
  
  sa_.95 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(sa_.15$fitness,
                   sa_.35$fitness,
                   sa_.55$fitness,
                   sa_.75$fitness,
                   sa_.95$fitness)),
             max(c(sa_.15$fitness,
                   sa_.35$fitness,
                   sa_.55$fitness,
                   sa_.75$fitness,
                   sa_.95$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop SA Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

sa_.15 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - SA', xlab = 'Iterations', ylab = 'Fitness Score')
sa_.35 %$% lines(iterations, fitness, col = 'blue')
sa_.55 %$% lines(iterations, fitness, col = 'darkgreen')
sa_.75 %$% lines(iterations, fitness, col = 'lightblue')
sa_.95 %$% lines(iterations, fitness, col = 'gray')

legend('bottomright', c('Cooling: 0.15', 'Cooling: 0.35', 'Cooling: 0.55', 'Cooling: 0.75', 'Cooling: 0.95'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Flip Flop SA Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

sa_.15 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Flip Flop - 1000 Bits - SA', xlab = 'Function Evaluations', ylab = 'Fitness Score')
sa_.35 %$% lines(fevals, fitness, col = 'blue')
sa_.55 %$% lines(fevals, fitness, col = 'darkgreen')
sa_.75 %$% lines(fevals, fitness, col = 'lightblue')
sa_.95 %$% lines(fevals, fitness, col = 'gray')

legend('bottomright', c('Cooling: 0.15', 'Cooling: 0.35', 'Cooling: 0.55', 'Cooling: 0.75', 'Cooling: 0.95'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()





soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  sa_.15_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.15_1_LOG.txt')
  sa_.15_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.15_2_LOG.txt')
  sa_.15_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.15_3_LOG.txt')
  sa_.15_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.15_4_LOG.txt')
  sa_.15_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.15_5_LOG.txt')
  
  sa_.15 <- rbind(sa_.15_1,
                  sa_.15_2,
                  sa_.15_3,
                  sa_.15_4,
                  sa_.15_5)
  
  sa_.15 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.35_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.35_1_LOG.txt')
  sa_.35_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.35_2_LOG.txt')
  sa_.35_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.35_3_LOG.txt')
  sa_.35_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.35_4_LOG.txt')
  sa_.35_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.35_5_LOG.txt')
  
  sa_.35 <- rbind(sa_.35_1,
                  sa_.35_2,
                  sa_.35_3,
                  sa_.35_4,
                  sa_.35_5)
  
  sa_.35 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.55_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.55_1_LOG.txt')
  sa_.55_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.55_2_LOG.txt')
  sa_.55_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.55_3_LOG.txt')
  sa_.55_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.55_4_LOG.txt')
  sa_.55_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.55_5_LOG.txt')
  
  sa_.55 <- rbind(sa_.55_1,
                  sa_.55_2,
                  sa_.55_3,
                  sa_.55_4,
                  sa_.55_5)
  
  sa_.55 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.75_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.75_1_LOG.txt')
  sa_.75_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.75_2_LOG.txt')
  sa_.75_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.75_3_LOG.txt')
  sa_.75_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.75_4_LOG.txt')
  sa_.75_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.75_5_LOG.txt')
  
  sa_.75 <- rbind(sa_.75_1,
                  sa_.75_2,
                  sa_.75_3,
                  sa_.75_4,
                  sa_.75_5)
  
  sa_.75 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.95_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.95_1_LOG.txt')
  sa_.95_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.95_2_LOG.txt')
  sa_.95_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.95_3_LOG.txt')
  sa_.95_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.95_4_LOG.txt')
  sa_.95_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/CONTPEAKS/CONTPEAKS_SA0.95_5_LOG.txt')
  
  sa_.95 <- rbind(sa_.95_1,
                  sa_.95_2,
                  sa_.95_3,
                  sa_.95_4,
                  sa_.95_5)
  
  sa_.95 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(sa_.15$fitness,
                   sa_.35$fitness,
                   sa_.55$fitness,
                   sa_.75$fitness,
                   sa_.95$fitness)),
             max(c(sa_.15$fitness,
                   sa_.35$fitness,
                   sa_.55$fitness,
                   sa_.75$fitness,
                   sa_.95$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks SA Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

sa_.15 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - SA', xlab = 'Iterations', ylab = 'Fitness Score')
sa_.35 %$% lines(iterations, fitness, col = 'blue')
sa_.55 %$% lines(iterations, fitness, col = 'darkgreen')
sa_.75 %$% lines(iterations, fitness, col = 'lightblue')
sa_.95 %$% lines(iterations, fitness, col = 'gray')

legend('bottomright', c('Cooling: 0.15', 'Cooling: 0.35', 'Cooling: 0.55', 'Cooling: 0.75', 'Cooling: 0.95'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/Continuous Peaks SA Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

sa_.15 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'Continuous Peaks - N = 100 & T = 49 - SA', xlab = 'Function Evaluations', ylab = 'Fitness Score')
sa_.35 %$% lines(fevals, fitness, col = 'blue')
sa_.55 %$% lines(fevals, fitness, col = 'darkgreen')
sa_.75 %$% lines(fevals, fitness, col = 'lightblue')
sa_.95 %$% lines(fevals, fitness, col = 'gray')

legend('bottomright', c('Cooling: 0.15', 'Cooling: 0.35', 'Cooling: 0.55', 'Cooling: 0.75', 'Cooling: 0.95'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()






soIcanCollapseThisHugeMess <- T
while(soIcanCollapseThisHugeMess){
  sa_.15_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.15_1_LOG.txt')
  sa_.15_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.15_2_LOG.txt')
  sa_.15_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.15_3_LOG.txt')
  sa_.15_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.15_4_LOG.txt')
  sa_.15_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.15_5_LOG.txt')
  
  sa_.15 <- rbind(sa_.15_1,
                  sa_.15_2,
                  sa_.15_3,
                  sa_.15_4,
                  sa_.15_5)
  
  sa_.15 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.35_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.35_1_LOG.txt')
  sa_.35_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.35_2_LOG.txt')
  sa_.35_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.35_3_LOG.txt')
  sa_.35_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.35_4_LOG.txt')
  sa_.35_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.35_5_LOG.txt')
  
  sa_.35 <- rbind(sa_.35_1,
                  sa_.35_2,
                  sa_.35_3,
                  sa_.35_4,
                  sa_.35_5)
  
  sa_.35 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.55_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.55_1_LOG.txt')
  sa_.55_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.55_2_LOG.txt')
  sa_.55_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.55_3_LOG.txt')
  sa_.55_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.55_4_LOG.txt')
  sa_.55_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.55_5_LOG.txt')
  
  sa_.55 <- rbind(sa_.55_1,
                  sa_.55_2,
                  sa_.55_3,
                  sa_.55_4,
                  sa_.55_5)
  
  sa_.55 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.75_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.75_1_LOG.txt')
  sa_.75_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.75_2_LOG.txt')
  sa_.75_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.75_3_LOG.txt')
  sa_.75_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.75_4_LOG.txt')
  sa_.75_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.75_5_LOG.txt')
  
  sa_.75 <- rbind(sa_.75_1,
                  sa_.75_2,
                  sa_.75_3,
                  sa_.75_4,
                  sa_.75_5)
  
  sa_.75 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  sa_.95_1 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.95_1_LOG.txt')
  sa_.95_2 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.95_2_LOG.txt')
  sa_.95_3 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.95_3_LOG.txt')
  sa_.95_4 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.95_4_LOG.txt')
  sa_.95_5 <- read.csv('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/TSP/TSP_SA0.95_5_LOG.txt')
  
  sa_.95 <- rbind(sa_.95_1,
                  sa_.95_2,
                  sa_.95_3,
                  sa_.95_4,
                  sa_.95_5)
  
  sa_.95 %<>% group_by(iterations) %>% summarise_all(mean) %>% as.data.frame
  
  soIcanCollapseThisHugeMess <- F
}

fit_lim <- c(min(c(sa_.15$fitness,
                   sa_.35$fitness,
                   sa_.55$fitness,
                   sa_.75$fitness,
                   sa_.95$fitness)),
             max(c(sa_.15$fitness,
                   sa_.35$fitness,
                   sa_.55$fitness,
                   sa_.75$fitness,
                   sa_.95$fitness)))

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP SA Iters.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

sa_.15 %$% plot(iterations, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 100 Destinations - SA', xlab = 'Iterations', ylab = 'Fitness Score')
sa_.35 %$% lines(iterations, fitness, col = 'blue')
sa_.55 %$% lines(iterations, fitness, col = 'darkgreen')
sa_.75 %$% lines(iterations, fitness, col = 'lightblue')
sa_.95 %$% lines(iterations, fitness, col = 'gray')

legend('bottomright', c('Cooling: 0.15', 'Cooling: 0.35', 'Cooling: 0.55', 'Cooling: 0.75', 'Cooling: 0.95'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()

jpeg('/Users/kmertan/Documents/CS7641/CS-7641-assignment-2/ABAGAIL/test/Plots/TSP SA Evals.jpg', width= 900, height = 600)
par(mfrow = c(1, 1))

sa_.15 %$% plot(fevals, fitness, type = 'l', col = 'orange', ylim = fit_lim, main = 'TSP - 100 Destinations - SA', xlab = 'Function Evaluations', ylab = 'Fitness Score')
sa_.35 %$% lines(fevals, fitness, col = 'blue')
sa_.55 %$% lines(fevals, fitness, col = 'darkgreen')
sa_.75 %$% lines(fevals, fitness, col = 'lightblue')
sa_.95 %$% lines(fevals, fitness, col = 'gray')

legend('bottomright', c('Cooling: 0.15', 'Cooling: 0.35', 'Cooling: 0.55', 'Cooling: 0.75', 'Cooling: 0.95'), lty = 1, col = c('orange', 'blue', 'darkgreen', 'lightblue', 'gray'))

dev.off()
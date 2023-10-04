rm(list=ls())
library(dplyr)
library(ggplot2)
library(latex2exp)
library(readr)
set.seed(1)

# obtain epsilons
percentages = seq(from = 0.01, to = 0.99, by = 0.01)

epsilon_range = c()
for (percent in percentages){
  epsilon = log((-percent-1)/(percent-1))
  epsilon_range = rbind(epsilon_range, epsilon)
}

proportions = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
sample_sizes = c(1e2) 
answers = c(2)
iterations = 1e3

trade_off = tidyr::expand_grid(proportions, epsilon= epsilon_range, sample_sizes, answers)
trade_off$bias = NA
trade_off$bias_corrected = NA

trade_off_collection = c()
for (g in 1:iterations){
  print(g)
  for (t in answers){
  #print(t)
    for (p in proportions){
    #print(p)
      for (j in sample_sizes){
        #print(j)
        n = j
        pop = sample(1:2, n, replace = T, prob = c(1-p,p)) ## sample whether an individual has a sensitive attribute
        for (epsilon in epsilon_range){
          # forced_response & disguised response
          responses = c()
          P = matrix(nrow = t, ncol = t)
          diag(P) = (exp(epsilon))/(t-1+exp(epsilon))
          P[is.na(P)==T] = (1)/(t-1+exp(epsilon))
          truth = (t*P[1,1]-1)/(t-1)
          for (i in 1:n){
            if(pop[i] == 1){responses = rbind(responses, sample(x = c(1:t),size = 1,prob= P[1,]))}
            else if(pop[i] == 2){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[2,]))}
            else if(pop[i] == 3){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[3,]))}
            else if(pop[i] == 4){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[4,]))}
            else if(pop[i] == 5){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[5,]))}
            else if(pop[i] == 6){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[6,]))}
            else if(pop[i] == 7){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[7,]))}
            else if(pop[i] == 8){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[8,]))}
            else if(pop[i] == 9){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[9,]))}
            else{responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[10,]))}
          }
          real_proportion =  data.frame(k = pop) %>% group_by(k) %>% summarize(prop = n()/n)
          observed_proportion =  data.frame(k = responses) %>% group_by(k) %>% summarize(prop = n()/n)
          observed_proportion$corrected = (truth + t * observed_proportion$prop -1)/(t*truth)
          bias = real_proportion$prop - observed_proportion$prop
          bias_corrected = real_proportion$prop - observed_proportion$corrected
          trade_off$bias[trade_off$epsilon == epsilon & trade_off$sample_sizes == n & trade_off$proportions == p & trade_off$answers == t] = bias[2]
          trade_off$bias_corrected[trade_off$epsilon == epsilon & trade_off$sample_sizes == n & trade_off$proportions == p & trade_off$answers == t] = bias_corrected[2]
        }
      }
    }
  }
  trade_off$iteration = g
  trade_off_collection = rbind(trade_off_collection, trade_off)
}

#write_rds(trade_off_collection,file = "trade_off_collection_n_100.RDS")

# line
trade_off_collection_100 = read_rds("trade_off_collection_n_100.RDS")
trade_off_collection_1000 = read_rds("trade_off_collection_n_1000.RDS")
trade_off_collection= rbind(trade_off_collection_100, trade_off_collection_1000)
trade_off_collection= trade_off_collection %>% group_by(proportions, epsilon, sample_sizes) %>% summarize(mean_bias = mean(bias),
                                                                mean_corrected_bias = mean(bias_corrected)) %>% tidyr::pivot_longer(cols = c(mean_bias, mean_corrected_bias))

trade_off_collection$name[trade_off_collection$name == "mean_bias"] = "biased estimator"
trade_off_collection$name[trade_off_collection$name == "mean_corrected_bias"] = "unbiased estimator"

trade_off_collection %>% filter(sample_sizes == 1000) %>% ggplot(aes(x = epsilon , y = value, group = interaction(epsilon, proportions), color = proportions)) + geom_point() + geom_line() + facet_grid(~name)  + ylab("bias") +xlab("privacy risk (epsilon)") + theme_bw() +
  theme(legend.position="bottom") + theme(text = element_text(size = 14), axis.text = element_text(size = 14, color = "black"), strip.text.x = element_text(size = 14),strip.text.y = element_text(size = 14), axis.text.x = element_text(size = 14, color = 'black'),legend.text=element_text(size=12, color = 'black'))+ geom_hline(yintercept = 0, color = "red", size = 1) + scale_color_continuous(breaks = c(0.1,0.3,0.5,0.7,0.9))

# density
trade_off_collection_100 = read_rds("trade_off_collection_n_100.RDS")
trade_off_collection_1000 = read_rds("trade_off_collection_n_1000.RDS")
trade_off_collection= rbind(trade_off_collection_100, trade_off_collection_1000)

trade_off_collection$sample_sizes[trade_off_collection$sample_sizes == 100] = "n = 100"
trade_off_collection$sample_sizes[trade_off_collection$sample_sizes == 1000] = "n = 1,000"
trade_off_collection$sample_sizes = factor(trade_off_collection$sample_sizes, levels = c("n = 100", "n = 1,000"))
trade_off_collection %>% group_by(sample_sizes, iteration) %>% summarize(mean_bias = mean(bias),mean_corrected_bias = mean(bias_corrected))%>% 
  tidyr::pivot_longer(cols = c(mean_bias, mean_corrected_bias)) %>% filter(name == "mean_corrected_bias") %>%
  ggplot(aes(x = value)) + geom_histogram(binwidth = 0.001, color = "black") + theme_bw() + facet_grid(~sample_sizes)  + geom_vline(xintercept = 0, color ="red", size=1)+ theme(text = element_text(size = 14), axis.text = element_text(size = 14, color = "black"), strip.text.x = element_text(size = 14),strip.text.y = element_text(size = 14), axis.text.x = element_text(size = 14, color = 'black'),legend.text=element_text(size=12, color = 'black')) + xlab("bias")


#write_rds(trade_off_collection,file = "trade_off_collection_n_10000.RDS")
trade_off_collection = read_rds("trade_off_collection_n_1000.RDS")

# US census ---------------------------------------------------------------

income_9 =data.frame(income = factor(c("< 15,999", "16,000 - 29,999", "30,000 - 39,999", "40,000 - 49,999", "50,000 - 59,999", "60,000 - 79,999",	"80,000 - 99,999",	"100,000 - 149,999", "> 150,000"), 
                           levels = c("< 15,999", "16,000 - 29,999", "30,000 - 39,999", "40,000 - 49,999", "50,000 - 59,999", "60,000 - 79,999",	"80,000 - 99,999",	"100,000 - 149,999", "> 150,000")), 
                           p = c(0.112235511, 0.106058615, 0.089805932, 0.089849739, 0.095676173,0.148946423,0.114951592,0.146098918,0.096377097), k = "k = 9")

income_5 =data.frame(income = factor(c("< 15,999","16,000 - 49,999",	"50,000 - 99,999" ,"100,000 - 149,999", "> 150,000"), 
                                     levels = c("< 15,999","16,000 - 49,999",	"50,000 - 99,999" ,"100,000 - 149,999", "> 150,000")), 
                     p = c(0.112235511, 0.106058615 + 0.089805932 + 0.089849739, 0.095676173 + 0.148946423 + 0.114951592, 0.146098918,0.096377097), k = "k = 5")

income_2 =data.frame(income = factor(c("< 79,999","> 79,999"), 
                                     levels = c("< 79,999","> 79,999")), 
                     p = c(0.112235511 + 0.106058615 + 0.089805932 + 0.089849739 + 0.095676173 + 0.148946423, 0.114951592 + 0.146098918 +0.096377097), k = "k = 2")

income = rbind(income_9, income_5, income_2)
income$income = factor(income$income, levels = c(levels(income_2$income),levels(income_5$income)[1:3],levels(income_9$income)[-1]))
income %>% ggplot(aes(x = income, y = p, group = k, label = round(p,2))) + facet_grid(~k, scales = "free_x") +
  geom_point(size = 2.5) + geom_text(nudge_y = 0.05, size = 4.5) +
  geom_line() +
  xlab("") + ylab(TeX("$Pr(X = m)$")) + theme_bw(base_size = 14) + theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  theme(text = element_text(size = 14, color = "black"), axis.text = element_text(size = 13, color = "black"), strip.text.x = element_text(size = 14, color = "black"), axis.text.x = element_text(size = 13, color = 'black'),legend.text=element_text(size=14, color = 'black'))+
  scale_x_discrete(breaks = income$income,
                   labels = income$income)

# create placeholder
answers = c(5,9)
sample_sizes = c(1e2)
trade_off_9 = tidyr::expand_grid(epsilon= epsilon_range, sample_sizes, income = income_9$income, answers = 9)
trade_off_5 = tidyr::expand_grid(epsilon= epsilon_range, sample_sizes, income = income_5$income, answers = 5)
trade_off = rbind(trade_off_5, trade_off_9)
trade_off$bias = NA
trade_off$bias_corrected = NA
iterations = 1000

# simulation
trade_off_collection = c()
for (g in 1:iterations){
  t0 <- Sys.time()
  print(g)
  for (t in answers){
    #print(t)
    for (n in sample_sizes){
      #print(n)
      pop = sample(1:t, n, replace = T, prob = (as.vector(t(income %>% dplyr::filter(k == paste0("k = ",t))%>% select(p)))))
      for (epsilon in epsilon_range){
        #print(epsilon)
        # forced_response & disguised response
        responses = c()
        P = matrix(nrow = t, ncol = t)
        diag(P) = (exp(epsilon))/(t-1+exp(epsilon))
        P[is.na(P)==T] = (1)/(t-1+exp(epsilon))
        truth = (t*P[1,1]-1)/(t-1)
        for (i in 1:n){
          if(pop[i] == 1){responses = rbind(responses, sample(x = c(1:t),size = 1,prob= P[1,]))}
          else if(pop[i] == 2){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[2,]))}
          else if(pop[i] == 3){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[3,]))}
          else if(pop[i] == 4){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[4,]))}
          else if(pop[i] == 5){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[5,]))}
          else if(pop[i] == 6){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[6,]))}
          else if(pop[i] == 7){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[7,]))}
          else if(pop[i] == 8){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[8,]))}
          else if(pop[i] == 9){responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[9,]))}
          else{responses = rbind(responses, sample(x = c(1:t),size = 1,prob=P[10,]))}
        }
      
        real_proportion = data.frame(k = pop) %>% group_by(k) %>% summarize(prop = n()/n)
        observed_proportion = data.frame(k = responses) %>% group_by(k) %>% summarize(prop = n()/n)
        observed_proportion$corrected = (truth + t * observed_proportion$prop -1)/(t*truth)
      
        bias = real_proportion - observed_proportion %>% select(k,prop)
        bias_corrected = real_proportion - observed_proportion %>% select(k,corrected)
      
        trade_off$bias[trade_off$epsilon == epsilon & trade_off$sample_sizes == n & trade_off$answers == t] = bias$prop
        trade_off$bias_corrected[trade_off$epsilon == epsilon & trade_off$sample_sizes == n & trade_off$answers == t] = bias_corrected$prop
      }
    }
  }
  t1 <- Sys.time()
  print(t1-t0)
  trade_off$iteration = g
  trade_off_collection = rbind(trade_off_collection, trade_off)
}
trade_off_collection 

trade_off_collection %>% group_by(epsilon, sample_sizes) %>% summarize(mean_bias = mean(bias),mean_corrected_bias = mean(bias_corrected))

write_rds(trade_off_collection,file = "trade_off_collection_k_n_100.RDS")

# Prolific study set up --------------------------------------------------------
# spinners percentages
# extra schaal toevoegen
# niet full factorial 
# redeneren vanuit epsilon

# k = 2

percentages = c(0.05,0.25,0.5,0.75,0.95)

# epsilons
epsilon_range = c()
for (percent in percentages){
  epsilon = log((-percent-1)/(percent-1)) # assuming second spinner is 1/2
  epsilon_range = rbind(epsilon_range, epsilon)
}
print(length(epsilon_range))
plot(epsilon_range)
epsilon_range

# simulate probabilities
answers = c(2,5,9)
epsilon_range = c(0.1000835,0.5108256,1.0986123,1.9459101,3.6635616)
framing = expand.grid(epsilon = epsilon_range, k = answers)
probabilities = c()
for (k in answers){
  print(paste0("for ", k ," answers:"))
  for (e in epsilon_range){
    p = (k*(exp(e))/(k-1+exp(e))-1) / (k-1)
    print(paste0("for epsilon ", round(e,2) ," percentage truthfully is ",round(p,2)))
    probabilities = rbind(probabilities, round(p,2))
  }
}
framing$p = round(probabilities,2)
framing$pplot = paste0("q = ",probabilities)
framing$epsilon = paste0("e = ",round(framing$epsilon,2))
framing$k = paste0("k = ",framing$k)
ggplot(framing, aes(x="", y=p, label = pplot, fill = "red")) +
  geom_bar(stat="identity", width=1) + geom_text(color = "black", nudge_x = -1, size = 4) + 
  coord_polar("y", start = 0) + facet_grid(k~epsilon, switch = "y") + ylab("") + xlab("") + ylim(0,1) + theme(legend.position = "none", text = element_text(size=14), strip.text = element_text(size = 12),axis.text = element_blank(),
                                                                                                              axis.ticks = element_blank(),
                                                                                                              panel.grid  = element_blank(),panel.grid.major = element_blank(),
                                                                                                              panel.grid.minor = element_blank(),
                                                                                                              panel.border = element_blank(),
                                                                                                              panel.background = element_blank())

## second spinners
k = 10
second_spinners = c()
for (a in 2:k){
  second_spinners = rbind(second_spinners, data.frame(k = rep(a,a), p = round(1/a,a), color = 1:a))
} 

k_2 = data.frame(k = rep(2,2), p = round(1/2,2), color = c("red","white"))
k_5 = data.frame(k = rep(5,5), p = round(1/5,2), color = c("red","white","blue","yellow","green"))
k_9 = data.frame(k = rep(9,9), p = round(1/9,4), color = c("red","white","blue","yellow","green", "purple", "black", "pink", "orange"))
sec_spinner = rbind(k_2,k_5,k_9)
sec_spinner$color = factor(sec_spinner$color, levels = c("red","white","blue","yellow","green", "purple", "black", "pink", "orange"))

ggplot(sec_spinner, aes(x="", y=p, label = p, fill = as.factor(color))) +
  geom_bar(stat="identity", width=1) + coord_polar("y", start = 0) + facet_grid(~k, switch = "y") + ylab("") + xlab("") + ylim(0,1) + theme(legend.position = "none", text = element_text(size=14), 
                                                                                                       strip.text = element_text(size = 12),
                                                                                                       axis.text = element_blank(),
                                                                                                       axis.ticks = element_blank(),
                                                                                                       panel.grid  = element_blank(),
                                                                                                       panel.grid.major = element_blank(),
                                                                                                       panel.grid.minor = element_blank(),
                                                                                                       panel.border = element_blank(),
                                                                                                       panel.background = element_blank()) + scale_fill_manual(values = c("red","white","blue","yellow","green", "purple", "black", "pink", "orange"))


# different designs
epsilon_range = c(0.1000835,0.5108256,1.0986123,1.9459101,3.6635616)
Spinners = c("0", "1", "2")
Sensitive_q = c("Income")  # Goldfarb & Tucker (2012)
Categories = c("2","5","9")

expand.grid(epsilon_range, Spinners, Categories)

# costs of survey
dollar_to_euro = 1.17
price_pp = 1.38
n_per_condition = 50*1.1

library(xtable)
print(xtable(data.frame(expand.grid(epsilon = epsilon_range, no_spinners = Spinners, no_answers = Categories))), type = "latex")

# control surveys
expand.grid("direct_q", Categories)
# treatment surveys
expand.grid(spinners = Spinners, answers = Categories)

## next steps with control and multiple questions for the future
no_questions = c("1","5")
control = c("no","yes")
transparency = c("no","yes")
epsilon_range

expand.grid(epsilon_range, no_questions, transparency,control)

control_costs = (nrow(expand.grid("direct_q", no_questions, transparency, control)) * n_per_condition * price_pp)  * dollar_to_euro
treatment_costs = (nrow(expand.grid(epsilon_range, no_questions, transparency,control)) * n_per_condition * price_pp)  * dollar_to_euro
total_costs_next_steps = (control_costs + treatment_costs) + (control_costs + treatment_costs)*0.25
total_costs_next_steps # -1000 from SOM

# analysis lab study ------------------------------------------------------
library("readxl")
library(dplyr)
direct <- read_excel("C:/Users/Gilia/Dropbox/PhD/Projects/2nd project - data collection/Qualtrics results/test/direct.xlsx")
no_spinner <- read_excel("C:/Users/Gilia/Dropbox/PhD/Projects/2nd project - data collection/Qualtrics results/test/no-spinner.xlsx")
spinner_1 <- read_excel("C:/Users/Gilia/Dropbox/PhD/Projects/2nd project - data collection/Qualtrics results/test/1-spinner.xlsx")
spinner_2 <- read_excel("C:/Users/Gilia/Dropbox/PhD/Projects/2nd project - data collection/Qualtrics results/test/2-spinner.xlsx")

direct = direct %>% select(`question (2)`, `question (5)`, `question (9)`, Q180, Q181, State, Education)
direct$S = "direct"
no_spinner = no_spinner %>% select(`question (2)`, `question (5)`, `question (9)`, Q180, Q181, State, Education, differential_privacy, k, result)
no_spinner$S = "no spinner"
spinner_1 = spinner_1 %>% select(`question (2)`, `question (5)`, `question (9)`, Q180, Q181, State, Education, differential_privacy, k, result)
spinner_1$S = "1 spinner"
spinner_2 = spinner_2 %>% select(`question (2)`, `question (5)`, `question (9)`, Q180, Q181, State, Education, differential_privacy, k, result)
spinner_2$S = "2 spinners"

test_dataset = bind_rows(direct, no_spinner, spinner_1, spinner_2)
table(as.numeric(test_dataset$differential_privacy)) # should be random/uniform
table(test_dataset$k) # should be random/uniform


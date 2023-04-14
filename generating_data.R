## generating random data

library(lubridate)
library(stringr)
library(dplyr)

n_data = 900
seed = 1

# selecting and sampling dates
datas = c(ymd_h("2023-01-22-00"), ymd_h("2023-04-20-24"))
datas

# creating a sequence
(datas_seq = seq(from = datas[1], to = datas[2], by = 'hours'))
print(length(datas_seq))

set.seed(seed)
indexes = sample(1:length(datas_seq), size = n_data)

datas_seq = datas_seq[indexes]

datetime = str_split_fixed(datas_seq, pattern = ' ', n = 2)

date_f = datetime[, 1]
horas_f = datetime[, 2]




# I'll consider a random person with a salary of 10 000 monetary units (MU)
# (e.g. 10k Reais). The person will have a expense quantity with a 
# standardized distribution and sometimes will have big expenses.

# So the person may be a person who has a great salary but spend a lot and
# occasionally spends to much. So the Variance will be of 10 000 and a mean o
# 150 MU/day with a salary of 10 000. If the result is less than 0, the total
# spend amount will be 0

set.seed(seed)
money = round(rnorm(n = n_data, mean = 150, sd = 100), 2)

for (i in 1:length(money)) {
  if(money[i] < 0){
    money[i] = 0
  }
}

money





# Transaction type
# I'll apply the same logic, but using binomial distribution.

set.seed(seed)
earnspend = as.factor(rbinom(n = n_data, size = 2, prob = 0.3 ))
earnspend = factor(earnspend, labels = c('Spend', "Moved", 'Earned'))%>% 
  as.character()
earnspend





# Picking a bank
# Let's say that the user have three bank acounts. So, let's distribuit'em 
# randomly using a custom funciton

random_selection = function(size, choices){
  n = length(choices)
  
  if(n == 1) return(rep(choices, times = size))
  
  vec = c()
  for (i in 1:size) {
    index = sample(1:n, size = 1)
    vec[i] = choices[index]
  }
  
  return(vec)
}

bank = random_selection(size = n_data, 
                        choices = c('Inter', 'Nubank', 'Santander'))



# Money destination

MoneyDestination = random_selection(size = n_data,
                                     choices = c('iFood', 'McDonalds', 'Clothes',
                                                 'Person1', 'WashingClothes',
                                                 'Things', 'Gas'))


# Monser Source type

MonerSourceType = random_selection(size = n_data,
                                   choices = c('Debit', 'Credit', 'Cash'))




# Ending date for credit cards

EndCreditDay = random_selection(size = n_data,
                                choices = c("2023-02-01",
                                            "2023-03-01",
                                            "2023-04-01"))


example_DF = data.frame(Dates = date_f,
                        Hours = horas_f,
                        AmountMoney = money,
                        KindOfMovement = earnspend,
                        Banks = bank,
                        Detination = MoneyDestination,
                        MonerSourceType = MonerSourceType,
                        EndCreditDay = EndCreditDay)

head(example_DF)


## Final Data Treatment

for (i in 1:nrow(example_DF)) {
  if(example_DF[i, 'MonerSourceType'] != 'Credit'){
    example_DF[i, 'EndCreditDay'] = NA
  }
}

head(example_DF)

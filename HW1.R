company_df <- read.csv("~/Downloads/Data608/inc5000_data.csv", stringsAsFactors = FALSE)
library(dplyr)
library(ggplot2)

#Question 1
state_ct <- count(company_df,State) %>% arrange(-n)
ggplot(data=state_ct, aes(x=State, y=n)) +
  geom_bar(stat="identity") +
  coord_flip()

#Question 2

company_df_3rd <- filter(company_df, State==state_ct$State[3])
emp_ind <- company_df_3rd %>% group_by(Industry) %>% summarise(avg=mean(Employees),total=sum(Employees))

#average number of employees PER COMPANY within an industry
ggplot(data=emp_ind, aes(x=reorder(Industry,avg), y=avg)) + geom_bar(stat="identity") + coord_flip()

#total number of employees WITHIN INDUSTRY
ggplot(data=emp_ind, aes(x=reorder(Industry,total), y=total)) + geom_bar(stat="identity") + coord_flip()

#Question 3

company_df_complete <- company_df[complete.cases(company_df),]
rev_emp <- company_df_complete %>% group_by(Industry) %>% summarise(per_cap_rev=sum(Revenue)/sum(Employees))
ggplot(data=rev_emp, aes(y=per_cap_rev, x=reorder(Industry, per_cap_rev))) + geom_bar(stat="identity") + coord_flip()

       
########################################Plot frame usage and salience all newspapers######################################
all_date <- aggregate(x = meta_theta_df_ALL[,c(7:(ncol(meta_theta_df_ALL)))], 
                      by = list(meta_theta_df_ALL$date), FUN = "mean")

all_1 <- all_date %>% 
  select("Group.1","V1", "V2", "V3", "V6", "V9", "V14", "V15", "V16", "V17", 
         "V18", "V22", "V23", "V27", "V29", "V30", "V32", "V33", "V35")

all_1.1 <- all_1 %>% 
  mutate(Midterm = rowSums(.[2:19])) %>% 
  select(Group.1, Midterm)

all_2 <- all_date %>% 
  select("Group.1","V4", "V7", "V8", "V11", "V12", "V13", "V19", "V21", "V24", 
         "V25", "V26", "V28", "V31", "V34")

all_2.2<- all_2 %>% 
  mutate(Caravan = rowSums(.[2:15])) %>% 
  select(Group.1, Caravan)

all.frames.all <- tribble()
all.frames.all <- cbind(all_1.1, all_2.2$Caravan)

colnames(all.frames.all)[colnames(all.frames.all)=="all_2.2$Caravan"] <- "Caravan"


meta_theta_df_ALL$forsum <- 1

all_n <- aggregate(x = meta_theta_df_ALL[,"forsum"], 
                   by = list(meta_theta_df_ALL$date), FUN = "sum")

all.frames.all <- full_join(all.frames.all, all_n)

all_plot <- ggplot(all.frames.all, aes(x = Group.1))+
  geom_line(aes(y=Midterm),color="red")+
  geom_line(aes(y=Caravan),color="blue")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  ylim(0.0, 1) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Frames Over Time")

Time <- meta_theta_df_ALL %>% 
  count(date)

time_plot <- ggplot(Time, aes(x = date)) +
  geom_line(aes(y=n),color="black")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  theme_bw() +
  ylim(0, 90) +
  xlab("") +
  ylab("Articles Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Frequency of News Articles")

library(grid)
library(gridExtra)

grid.arrange(time_plot, all_plot, nrow = 2)

############################################Plot frame usage by type of newspaper##############################################
part <- meta_theta_df_ALL[meta_theta_df_ALL$publication == "The Examiner" | 
                             meta_theta_df_ALL$publication == "The Daily Beast",]


part_date <- aggregate(x = part[,c(7:(ncol(part)))], 
                        by = list(part$date), FUN = "mean")

part_1 <- part_date %>% 
  select("Group.1","V1", "V2", "V3", "V6", "V9", "V14", "V15", "V16", "V17", 
         "V18", "V22", "V23", "V27", "V29", "V30", "V32", "V33", "V35")

part_1.1 <- part_1 %>% 
  mutate(Politics = rowSums(.[2:19])) %>% 
  select(Group.1, Politics)

part_2 <- part_date %>% 
  select("Group.1","V4", "V7", "V8", "V11", "V12", "V13", "V19", "V21", "V24", 
         "V25", "V26", "V28", "V31", "V34")

part_2.2<- part_2 %>% 
  mutate(Caravan = rowSums(.[2:15])) %>% 
  select(Group.1, Caravan)

all.frames.part <- tribble()
all.frames.part <- cbind(part_1.1, part_2.2$Caravan)

colnames(all.frames.part)[colnames(all.frames.part)=="part_2.2$Caravan"] <- "Caravan"

part$forsum <- 1

part_n <- aggregate(x = part[,"forsum"], 
                     by = list(part$date), FUN = "sum")

all.frames.part <- full_join(all.frames.part, part_n)

part_plot <- ggplot(all.frames.part, aes(x = Group.1))+
  geom_line(aes(y=Politics),color="red")+
  geom_line(aes(y=Caravan),color="blue")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  ylim(0.0, 1) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Partisan Press")

part_plot_n <- ggplot(part_n, aes(x = Group.1)) +
  geom_line(aes(y=x),color="black")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  theme_bw() +
  ylim(0, 60) +
  xlab("") +
  ylab("Articles Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Frequency of Partisan News Articles")

##mainstream

nat <- meta_theta_df_ALL[meta_theta_df_ALL$publication == "New York Times" | 
                           meta_theta_df_ALL$publication == "The Washington Post" |
                           meta_theta_df_ALL$publication == "USA Today" |
                           meta_theta_df_ALL$publication == "Wall Street Journal",]

nat_date <- aggregate(x = nat[,c(7:(ncol(nat)))], 
                      by = list(nat$date), FUN = "mean")

nat_1 <- nat_date %>% 
  select("Group.1","V1", "V2", "V3", "V6", "V9", "V14", "V15", "V16", "V17", 
         "V18", "V22", "V23", "V27", "V29", "V30", "V32", "V33", "V35")

nat_1.1 <- nat_1 %>% 
  mutate(Politics = rowSums(.[2:19])) %>% 
  select(Group.1, Politics)

nat_2 <- nat_date %>% 
  select("Group.1","V4", "V7", "V8", "V11", "V12", "V13", "V19", "V21", "V24", 
         "V25", "V26", "V28", "V31", "V34")

nat_2.2<- nat_2 %>% 
  mutate(Caravan = rowSums(.[2:15])) %>% 
  select(Group.1, Caravan)

all.frames.nat <- tribble()
all.frames.nat <- cbind(nat_1.1, nat_2.2$Caravan)

colnames(all.frames.nat)[colnames(all.frames.nat)=="nat_2.2$Caravan"] <- "Caravan"

nat$forsum <- 1

nat_n <- aggregate(x = nat[,"forsum"], 
                   by = list(nat$date), FUN = "sum")

all.frames.nat <- full_join(all.frames.nat, nat_n)

nat_plot <- ggplot(all.frames.nat, aes(x = Group.1))+
  geom_line(aes(y=Politics),color="red")+
  geom_line(aes(y=Caravan),color="blue")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  ylim(0.0, 1) +
  theme_bw() +
  xlab("") +
  ylab("Frame Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "National Press")

nat_plot_n <- ggplot(nat_n, aes(x = Group.1)) +
  geom_line(aes(y=x),color="black")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  theme_bw() +
  ylim(0, 60) +
  xlab("") +
  ylab("Articles Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Frequency of National News Articles")

local <- setdiff(meta_theta_df_ALL$publication, part$publication)

local <- setdiff(local, part$publication)

local <- meta_theta_df_ALL[which(meta_theta_df_ALL$publication %in% local), ]

local_date <- aggregate(x = local[,c(7:(ncol(local)))], 
                        by = list(local$date), FUN = "mean")

local_1 <- local_date %>% 
  select("Group.1","V1", "V2", "V3", "V6", "V9", "V14", "V15", "V16", "V17", 
         "V18", "V22", "V23", "V27", "V29", "V30", "V32", "V33", "V35")

local_1.1 <- local_1 %>% 
  mutate(Politics = rowSums(.[2:19])) %>% 
  select(Group.1, Politics)

local_2 <- local_date %>% 
  select("Group.1","V4", "V7", "V8", "V11", "V12", "V13", "V19", "V21", "V24", 
         "V25", "V26", "V28", "V31", "V34")

local_2.2<- local_2 %>% 
  mutate(Caravan = rowSums(.[2:15])) %>% 
  select(Group.1, Caravan)

all.frames.local <- tribble()
all.frames.local <- cbind(local_1.1, local_2.2$Caravan)

colnames(all.frames.local)[colnames(all.frames.local)=="local_2.2$Caravan"] <- "Caravan"

local$forsum <- 1

local_n <- aggregate(x = local[,"forsum"], 
                     by = list(local$date), FUN = "sum")

all.frames.local <- full_join(all.frames.local, local_n)

colnames(all.frames.local)[colnames(all.frames.local)=="x"] <- "Salience.Local"

local_plot <- ggplot(all.frames.local, aes(x = Group.1))+
  geom_line(aes(y=Politics),color="red")+
  geom_line(aes(y=Caravan),color="blue")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  ylim(0.0, 1) +
  theme_bw() +
  xlab("Date") +
  ylab("Frame Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Local Press")

local_plot_n <- ggplot(local_n, aes(x = Group.1)) +
  geom_line(aes(y=x),color="black")+
  geom_vline(xintercept = all.frames.all$Group.1[23], linetype = 4) +
  theme_bw() +
  ylim(0, 60) +
  xlab("") +
  ylab("Articles Frequency") +
  theme(text = element_text(size = 16)) +
  labs(title = "Frequency of Local News Articles")

grid.arrange(part_plot_n, part_plot, nat_plot_n, nat_plot, local_plot_n, local_plot, nrow = 6)

---
title: "Heroes of Might and Magic III analysis"
author: "Piotr Ocalewicz"
date: "2/7/2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r libraries, include=FALSE}
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(rvest)
library(stringr)
library(corrplot)
library(ggcorrplot)
library(radarchart) 
library(plotly)
library(cluster)
library(factoextra)
library(purrr)
```



```{r web_scrapping}
website_link <- read_html("http://heroes.thelazy.net/wiki/List_of_creatures")
homm3_data <- website_link %>% html_node("table") %>% html_table()
```



```{r data_manipulation, results='hide'}
homm3_data <- 
  homm3_data %>% dplyr::rename("Cost_gold" = Cost,
                               "Cost_resources" = "")



homm3_data$Cost_resources <- 
  homm3_data$Cost_resources %>%
  str_replace("[+]", "") %>%
  str_replace("[, ]", "") %>% 
  str_trim() %>%
  as.numeric()

# Dorobić informację, czy to upgrade
homm3_data$Creature_upgrade <- c(rep(c('Basic','Upgrade'), 2, nrow(filter(homm3_data, Town != 'Neutral'))), 
                               rep('Neutral', nrow(filter(homm3_data, Town == 'Neutral'))))

# Dorobić zmienną shooter/flyer/walker
homm3_data <- 
  homm3_data %>%
    mutate(
      Type = case_when(
        Name %in% c('Archer', 'Marksmen', 'Monk', 'Zealot', 'Wood Elf', 'Grand Elf', 'Master Gremlin', 'Mage' ,'Arch Mage', 'Titan','Gog', 'Magog', 'Lich', 'Power Lich', 'Beholder', 'Evil Eye', 'Medusa' ,'Medusa Queen', 'Orc', 'Orc Chieftain',
                    'Cyclops', 'Cyclops King', 'Lizardman', 'Lizard Warrior', 'Storm Elemental', 'Ice Elemental', 'Halfling',
                    'Sharpshooter', 'Enchanter') ~ "Shooter",
        Name %in% c('Griffin', 'Royal Griffin', 'Angel' ,'Archangel', 'Pegasus', 'Silver Pegasus', 'Green Dragon', 'Gold Dragon',
                    'Stone Gargoyle', 'Obsidian Gargoyle', 'Genie', 'Master Genie', 'Efreet', 'Efreet Sultan', 'Devil', 
                    'Arch Devil', 'Wight', 'Wraith', 'Vampire', 'Vampire Lord', 'Bone Dragon', 'Ghost Dragon', 'Harpy', 'Harpy Hag', 'Manticore','Scorpicore', 'Red Dragon', 'Black Dragon', 'Roc', 'Thunderbird', 'Serpent Fly', 'Dragon Fly', 'Wyvern', 'Wyvern Monarch', 'Pixie', 'Sprite', 'Storm Elemental', 'Energy Elemental', 'Firebird', 'Phoenix', 'Faerie Dragon', 'Rust Dragon', 'Azure Dragon') ~ "Flyer",
        TRUE ~ "Walker"
      )
    )

# Add new variables - the average damage made by a unit and a dispertion of the damage
homm3_data <- 
  homm3_data %>% mutate(Davg = (Dmax + Dmin)/2)
#                        ,Ddisp = )


# Add new variable - weekly growth of a unit
homm3_data <- 
  homm3_data %>%
    mutate(
      Basic_growth = case_when(
        (Lvl == 7 & !Name %in% c('Firebird', 'Phoenix')) | (Town == 'Neutral' & str_detect(Name, 'Dragon') == TRUE) ~ 1,
        (Lvl == 6 & !Name %in% c('Wyvern', 'Wyvern Monarch')) | (Name %in% c('Firebird', 'Phoenix','Enchanter','Diamond Golem')) ~2,
        (Lvl == 5 & !Name %in% c('Earth Elemental','Magma Elemental')) | (Name %in% c('Gold Golem','Troll')) ~3,
        (Lvl == 4 & !Name %in% c('Pegasus','Silver Pegasus','Fire Elemental','Energy Elemental')) | (Name %in% c('Wyvern','Wyvern Monarch','Earth Elemental','Magma Elemental','Sharpshooter')) ~4,
        (Name %in% c('Pegasus','Silver Pegasus','Fire Elemental','Energy Elemental','Hell Hound','Cerberus')) ~5,
        (Name %in% c('Stone Golem','Iron Golem','Air Elemental','Storm Elemental','Water Elemental','Ice Elemental')) ~6,
        (Lvl == 3 & Town %in% c('Castle','Rampart','Necropolis','Dungeon','Stronghold')) | (Name %in% c('Mummy','Nomad')) ~ 7,
        (Lvl == 2 & Town %in% c('Castle','Tower','Stronghold','Fortress')) ~9,
        (Lvl == 1 & Town %in% c('Necropolis','Fortress')) ~12,
        (Lvl == 1 & Town %in% c('Castle','Rampart','Dungeon')) ~14,
        (Lvl == 1 & Town %in% c('Inferno','Stronghold')) | (Name == 'Halfling') ~15,
        (Lvl == 1 & Town %in% c('Tower')) ~16,
        (Lvl == 1 & Town %in% c('Conflux')) ~20,
        (Name == 'Peasant') ~25,
        TRUE ~ 8
      )
    )



```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

```{r, warning=FALSE}
homm3_data %>% filter(Town != 'Neutral') %>% 
  ggplot(aes(x = as.factor(Town), y = Cost_gold)) +  #fill = Creature_upgrade
  geom_boxplot() + 
  #geom_point() + 
  geom_text_repel(aes(label = ifelse(Lvl == 7, Name, NA)), size = 2) + 
  theme_minimal() + 
  theme(legend.position = 'top')
  #guides(fill=FALSE) +
  #coord_flip()
```

```{r}
homm3_data %>% 
  #filter(Town != 'Neutral') %>% 
  select_if(is.numeric) %>% 
  select(-Cost_resources) %>%
  cor() %>% 
  #corrplot(method = 'number')
  ggcorrplot(type = "upper", ggtheme = ggplot2::theme_minimal, lab = TRUE)
```

```{r}
homm3_data %>% filter(Town != 'Neutral') %>% 
  group_by(Town, Creature_upgrade) %>% summarise(Cost_gold = sum(Cost_gold * Basic_growth)) %>%
  ggplot(aes(x = reorder(Town, desc(Cost_gold)), y = Cost_gold)) + 
    geom_bar(aes(fill = Town), stat = 'identity', position = 'dodge') +
    geom_text(aes(label = round(Cost_gold/1000,1)), vjust=1.3, color = "white") +
    theme_minimal() +
    facet_wrap(~Creature_upgrade)
```

```{r}

homm3_data %>% filter(Lvl == 1, Creature_upgrade == 'Basic') %>% 
  select(Name, Att, Def, Spd, Davg) %>% 
  chartJSRadar(pcol = c("seagreen","indianred","midnightblue","goldenrod"))
  #mutate_at(vars(-Name), funs(rescale))

homm3_data %>% filter(Lvl == 5) %>%
  select(Name, Att, Def, Spd, Davg, Basic_growth, Cost_gold, Type) %>%
  ggplot(aes(x = Att, Spd)) + geom_point() +
    geom_text_repel(aes(label = Name, color = Type))

  
compare_creatures <- function(dataset, cr1, cr2) 
{
  dataset %>% select(Name, Att, Spd) %>%
              filter(Name %in% c(cr1, cr2)) %>%
              ggplot(aes(x = Att, y = Spd)) + geom_point() +
              geom_text_repel(aes(label = Name))
}
compare_creatures(homm3_data, 'Skeleton', 'Pikeman')
```

```{r}
c1 <- homm3_data %>% filter(Town != 'Neutral', Creature_upgrade %in% c('Upgrade','Basic')) %>%
      select(Name, Town, Lvl, Att, Def, HP, Spd, Cost_gold, Basic_growth, Davg, Type) %>%
      ggplot(aes(x = HP, y = Spd, color = Type)) + 
        geom_point()
ggplotly(c1)
```

PCA

```{r pca}

```


```{r kmeans}

homm3_data_kmeans <- homm3_data %>% filter(Town != 'Neutral') %>% select(Name:Cost_gold, Davg:Basic_growth)
homm3_data_kmeans$class <- kmeans(select(homm3_data_kmeans, -Name, -Town), 3)$cluster

c1 <- homm3_data_kmeans %>%
      ggplot(aes(x = Davg, y = Basic_growth, color = as.factor(class))) +
        geom_point() + 
        geom_jitter() +
        stat_ellipse(aes(color = as.factor(class)))
  ggplotly(c1)
```

```{r}
homm3_data_matrix <- as.matrix(select(homm3_data_kmeans, -Town, -class) %>% filter(Lvl == 7)) 
rownames(homm3_data_matrix) <- homm3_data_matrix[,"Name"]
homm3_data_matrix <- homm3_data_matrix[,c(-1, -2)]

class(homm3_data_matrix) <- "numeric"

fviz_nbclust(homm3_data_matrix, FUN = hcut, method = "wss")
fviz_nbclust(homm3_data_matrix, FUN = hcut, method = "silhouette")

#mahalanobis(homm3_data_matrix, colMeans(homm3_data_matrix), cov(homm3_data_matrix))

res.dist <- get_dist(homm3_data_matrix, stand = TRUE, method = "kendall")
fviz_dist(res.dist,
   gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
```


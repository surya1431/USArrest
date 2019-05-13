
# Done By
# S.Surya
# V.Priyanka

install.packages("tidyr")
library(tidyr)

install.packages("dplyr")
library(dplyr)

install.packages("tidyverse", dependencies=TRUE)
library(tidyverse)

install.packages("cluster")
library(cluster)

install.packages("factoextra")
library(factoextra)

df <- USArrests
df

df <- na.omit(df)

df <- scale(df)
head(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
distance[0:50][]

k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)

k2

k2 <- kmeans(df, centers = 2, nstart = 25)
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
library(gridExtra)
grid.arrange(p1)

df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()

k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df, 4, nstart = 25)
print(final)

fviz_cluster(final, data = df)

USArrests %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

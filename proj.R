
############ Load the source code of the hashes ############

library(Rcpp)
sourceCpp(file = "CppSource/HashTableSource.cpp")

# Test that the hash functions are working as expected
stopifnot(KRHash("Hello") == 69609650);
stopifnot(KRHash("Potato :)") == 74689960);
stopifnot(MurmurHash("Hello") == 2494215904);
stopifnot(MurmurHash("Potato :)") == 496492254);

############ Load the books ############

load_book <- function (book_name) {
  lines <- readLines(book_name)
  words <- strsplit(lines, "[^A-zÀ-ú]+")
  unlist(words)
}

books <- list(
  load_book("Books/DonQuijote.txt"),
  load_book("Books/Hamlet.txt"))


############ Hash Table Code ############

create_hash_table <- function(hash, capacity) {
  table <- character(capacity)
  list(table = table, hash = hash, elements = 0)
}

insert <- function(hash_table, word) {
  capacity <- length(hash_table$table)
  
  if (capacity <= hash_table$elements) {
    stop("Hash table is full")
  }
  
  index <- hash_table$hash(word) %% capacity
  
  collisions <- 0
  while (hash_table$table[index + 1] != "" & hash_table$table[index + 1] != word) {
    collisions <- collisions + 1
    index <- (index + 1) %% capacity
  }
  
  if (hash_table$table[index + 1] != word) {
    hash_table$table[index + 1] <- word
    hash_table$elements <- hash_table$elements + 1
  }
  
  list(hash_table=hash_table, collisions=collisions)
}

load_factor <- function(hash_table) {
  hash_table$elements / length(hash_table$table)
}

############ Experiment Code ############

experiment <- function(H, L, S, n, Book) {
  word_index <- 1 + (ceiling(S * L) + 1) * (n - 1) # Fix this
  
  
  hash_table <- create_hash_table(hash = c(KRHash, MurmurHash)[[H]], capacity = S)
  
  words <- books[[Book]]
  
  C <- 0
  inserted_words <- 0
  
  while (load_factor(hash_table) < L) {
    if (length(words) < word_index) { stop("Not enought data") }
    insert_result <- insert(hash_table, words[word_index])
    hash_table <- insert_result$hash_table
    C <- C + insert_result$collisions
    
    inserted_words <- inserted_words + 1
    word_index <- word_index + 1
  }
  
  # if (length(words) < word_index) { stop("Not enought data") }
  # insert(hash_table, words[word_index])$collisions
  C / inserted_words
}


############ Data Collection ############

Load <- seq(0.1, 0.9, by=0.1)
Size <- c(40)
Book <- 1:length(books)
Hash <- c(1, 2)
n <- 1:(100 / length(books))

data <- expand.grid(H=Hash, L=Load, S=Size, B=Book, n=n)

data$C <- apply(data, 1, function(params) {
  do.call(experiment, as.list(params))
})

# Export the data
write.csv(data,'data.csv')

data_KR <- data[data$H == 1,]
data_Murmur <- data[data$H == 2,]

data_diff <- data_KR
data_diff$C <- apply(data_KR, 1, function(KR_row) {
  L <- round(KR_row[["L"]], 2)
  S <- KR_row[["S"]]
  n <- KR_row[["n"]]
  B <- KR_row[["B"]]
  Murmur_row <- data_Murmur[data_Murmur$S==S & data_Murmur$B==B & round(data_Murmur$L, 2)==L & data_Murmur$n==n,]
  KR_row[["C"]] - Murmur_row[["C"]]
})



############ Graph ############


library(ggplot2)

KR_d <- data_KR[data_KR$S==Size[1],]
KR_d_mean <- aggregate(C~L, data=KR_d, mean)

Murmur_d <- data_Murmur[data_Murmur$S==Size[1],]
Murmur_d_mean <- aggregate(C~L, data=Murmur_d, mean)


colKR <- "#44a5c2"
colMurmur <- "#ffae49"

ggplot() +
  ggtitle("Esperança de col·lisions") + ylab("E(C)") +
  geom_line(data=KR_d_mean, aes(L, C), color=colKR,  lwd=1) +
  geom_line(data=Murmur_d_mean, aes(L, C), color=colMurmur,  lwd=1) +
  geom_point(data=KR_d_mean, aes(L, C, color='KR Hash'), size=5, pch=17) +
  geom_point(data=Murmur_d_mean, aes(L, C, color='Murmur Hash'), size=5, pch=16) +
  scale_colour_manual(name = 'Hash',
                      values =c('KR Hash'=colKR,'Murmur Hash'=colMurmur),
                      breaks = c('KR Hash','Murmur Hash')) +
  guides(shape = FALSE, colour = guide_legend(override.aes = list(shape = c(17, 16))))


##### C_KR - C_Murmur ##### 

diff_d <- data_diff[data_diff$S==Size[1],]
diff_d_mean <- aggregate(C~L, data=diff_d, mean)
diff_d_sd <- aggregate(C~L, data=diff_d, sd)

ggplot(data=diff_d_mean, aes(L, C)) +
  ggtitle("Esperança de la diferència de col·lisions") +
  ylab(bquote(E(C[KR] - C[Murmur]))) +
  geom_line(color=colKR, lwd=1) +
  geom_point(color=colKR, size=5)

hist_L <- 0.7
diff_C <- diff_d[round(data$L, 2)==hist_L,]$C
hist(diff_C, main=paste("Histograma L = ", hist_L), xlab=bquote(C[KR] - C[Murmur]))

tqqplot(diff_C, main=paste("Normal Q-Q Plot L = ", hist_L))
qqline(diff_C, main=paste("Normal Q-Q Plot L = ", hist_L))

##### Box plot C_KR i C_Murmur ##### 

boxplot_L <- 0.8
data_boxplot <- data[data$S==Size[1] & round(data$L, 2)==boxplot_L,]
boxplot_hash_name <- sapply(data_boxplot$H, function(hash) { c("K&R", "Murmur")[[hash]] })
boxplot(data_boxplot$C ~ boxplot_hash_name, main=paste("Boxplot L = ", boxplot_L), xlab="Hash", ylab="C")








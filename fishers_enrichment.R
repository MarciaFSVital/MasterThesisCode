setwd("/Users/marcia/Library/CloudStorage/OneDrive-UniversidadedeLisboa/MARCIA")
library(stats)
library(vcd)

# Load the data
results_df <- read.csv("enrichment_results.csv")
filtered_drivers_df <- read.csv("driver_stats.csv", row.names = "driver")  # Set driver as index

# Calculate the number of drivers in each category
num_drivers_D0 <- nrow(subset(filtered_drivers_df, group == 'non_sign'))
num_drivers_D1 <- nrow(subset(filtered_drivers_df, group == 'neg'))
num_drivers_D2 <- nrow(subset(filtered_drivers_df, group == 'pos'))
num_drivers_D3 <- nrow(subset(filtered_drivers_df, group == 'more_pos'))
num_drivers_D4 <- nrow(subset(filtered_drivers_df, group == 'more_neg'))
num_drivers_D5 <- nrow(subset(filtered_drivers_df, group == 'neutral'))

# Combine all driver counts into a single vector
num_drivers_D <- c(num_drivers_D0, num_drivers_D1, num_drivers_D2, num_drivers_D3, num_drivers_D4, num_drivers_D5)

# Initialize an empty list to store results
data_list <- list()

# Loop over each term to perform Fisher's exact test
for (term in 1:nrow(results_df)) {
  values_df <- as.numeric(results_df[term, c("non_sign", "neg", "pos", "more_pos", "more_neg", "neutral")])
  remaining_values <- num_drivers_D - values_df
  matrix_2x6 <- rbind(values_df, remaining_values)
  
  fisher_results <- fisher.test(matrix_2x6, simulate.p.value = TRUE)
  
  # Store the results in the list
  data_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    p.value = fisher_results$p.value
  )
}

# Create a DataFrame from the Fisher test results
fisher_df <- do.call(rbind, lapply(data_list, as.data.frame))
fisher_df$adjusted.p.value <- p.adjust(fisher_df$p.value, method = "BH")

# Initialize lists for Chi-Square and Cramer's V results
chi_square_list <- list()
cramers_v_list <- list()

# Loop to perform Chi-Square test and calculate Cramer's V
for (term in 1:nrow(results_df)) {
  values_df <- as.numeric(results_df[term, c("non_sign", "neg", "pos", "more_pos", "more_neg", "neutral")])
  remaining_values <- num_drivers_D - values_df
  matrix_2x6 <- rbind(values_df, remaining_values)
  
  chi_square_result <- chisq.test(matrix_2x6, simulate.p.value = TRUE)
  chi_square_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    chi_square_p.value = chi_square_result$p.value
  )
  
  cramers_v <- assocstats(matrix_2x6)$cramer
  cramers_v_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    cramers_v = cramers_v
  )
}

# Create DataFrames for Chi-Square and Cramer's V results
chi_square_df <- do.call(rbind, lapply(chi_square_list, as.data.frame))
cramers_v_df <- do.call(rbind, lapply(cramers_v_list, as.data.frame))

# Merge all results into a final DataFrame
final_results <- merge(fisher_df, chi_square_df, by = c("Source", "Name"))
final_results <- merge(final_results, cramers_v_df, by = c("Source", "Name"))

# Save the final results to a CSV file
write.csv(final_results, "combined_statistical_results.csv", row.names = FALSE)




























setwd("/Users/marcia/Library/CloudStorage/OneDrive-UniversidadedeLisboa/MARCIA")
library(stats)
library(vcd)

# Ler os dados
results_df <- read.csv("enrichment_results.csv")
filtered_drivers_df <- read.csv("driver_stats.csv")

# Calcular o número de drivers por categoria
num_drivers_D0 <- nrow(subset(filtered_drivers_df, group == 'non_sign'))
num_drivers_D1 <- nrow(subset(filtered_drivers_df, group == 'neg'))
num_drivers_D2 <- nrow(subset(filtered_drivers_df, group == 'pos'))
num_drivers_D3 <- nrow(subset(filtered_drivers_df, group == 'more_pos'))
num_drivers_D4 <- nrow(subset(filtered_drivers_df, group == 'more_neg'))

num_drivers_D <- c(num_drivers_D0, num_drivers_D1, num_drivers_D2, num_drivers_D3, num_drivers_D4)
data_list <- list()

# Loop sobre os termos para realizar o teste de Fisher
for (term in 1:nrow(results_df)) {
  values_df <- as.numeric(results_df[term, c("N_Sig", "Neg", "Pos", "More_Pos", "More_Neg")])
  remaining_values <- num_drivers_D - values_df
  matrix_2x5 <- rbind(values_df, remaining_values)
  fisher_results <- fisher.test(matrix_2x5, simulate.p.value = TRUE)
  data_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    p.value = fisher_results$p.value
  )
}

# Criar o DataFrame com os resultados do teste de Fisher
fisher_df <- do.call(rbind, lapply(data_list, as.data.frame))
fisher_df$adjusted.p.value <- p.adjust(fisher_df$p.value, method = "BH")

# Preparar listas para os testes qui-quadrado e V de Cramer
chi_square_list <- list()
cramers_v_list <- list()

for (term in 1:nrow(results_df)) {
  values_df <- as.numeric(results_df[term, c("N_Sig", "Neg", "Pos", "More_Pos", "More_Neg")])
  remaining_values <- num_drivers_D - values_df
  matrix_2x5 <- rbind(values_df, remaining_values)
  
  chi_square_result <- chisq.test(matrix_2x5, simulate.p.value = TRUE)
  chi_square_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    chi_square_p.value = chi_square_result$p.value
  )
  
  cramers_v <- assocstats(matrix_2x5)$cramer
  cramers_v_list[[term]] <- list(
    Source = results_df[term, 'Source'],
    Name = results_df[term, 'Name'],
    cramers_v = cramers_v
  )
}

# Criar DataFrames para os resultados dos testes
chi_square_df <- do.call(rbind, lapply(chi_square_list, as.data.frame))
cramers_v_df <- do.call(rbind, lapply(cramers_v_list, as.data.frame))

# Combinar todos os resultados em um único DataFrame
final_results <- merge(fisher_df, chi_square_df, by = c("Source", "Name"))
final_results <- merge(final_results, cramers_v_df, by = c("Source", "Name"))

# Escrever os resultados em um arquivo CSV
write.csv(final_results, "combined_statistical_results.csv", row.names = FALSE)
#write.csv(fisher_df, "results_fisher_test_adjusted_2.csv", row.names = FALSE)


####HERE##################

#if (!requireNamespace("BiocManager", quietly = TRUE)) {
#  install.packages("BiocManager")
#}
#BiocManager::install("GOSemSim", force = TRUE)
#BiocManager::install("org.Hs.eg.db")

library(GOSemSim)
library(org.Hs.eg.db)

#-------------------------- Functions -----------------------------------
parse_intersections <- function(intersections) {
  genes <- gsub("\\[|\\]|'|\\s", "", intersections)
  gene_list <- strsplit(genes, ",")[[1]]
  return(gene_list)
}
#------------------------------------------------------------------------

final_results_df <- read.csv("enrichment_results.csv")
combined_stat <- read.csv("combined_statistical_results.csv")

#combined_stat_sig <- combined_stat[combined_stat$adjusted.p.value < 0.05, ]
combined_stat_sig <- combined_stat[
  combined_stat$adjusted.p.value < 0.05 & combined_stat$chi_square_p.value < 0.05, 
]

combined_stat_sig <- merge(
  final_results_df, combined_stat_sig, by = "Name", all = FALSE
)

bp_terms <- combined_stat_sig[
  grep("GO:BP", combined_stat_sig$Source), "native"]

#hsGO_BP <- godata(
#  anno.Hs.eg.db, ont = "BP", keytype = "SYMBOL", computeIC = TRUE
#)
# Preparar dados GO para as três ontologias
hsGO_BP <- godata('org.Hs.eg.db', ont="BP", keytype = "SYMBOL", computeIC=TRUE)
#hsGO_MF <- godata('org.Hs.eg.db', ont="MF", keytype = "SYMBOL", computeIC=TRUE)
#hsGO_CC <- godata('org.Hs.eg.db', ont="CC", keytype = "SYMBOL", computeIC=TRUE)

# create pairs of GO terms
term_pairs <- combn(bp_terms, 2, simplify = FALSE)

# measure semantic similarity between pairs
pairwise_similarity <- sapply(term_pairs, function(terms) {
  goSim(terms[1], terms[2], semData=hsGO_BP, measure="Wang")
})

# create matrix with results
relation_matrix <- matrix(0, nrow = length(bp_terms), ncol = length(bp_terms))
rownames(relation_matrix) <- colnames(relation_matrix) <- as.character(bp_terms)

for (i in seq_along(term_pairs)) {
  pair <- term_pairs[[i]]
  relation_matrix[pair[1], pair[2]] <- pairwise_similarity[i]
  relation_matrix[pair[2], pair[1]] <- pairwise_similarity[i]
}

# Identificar colunas com variância não-zero
non_zero_var <- apply(relation_matrix, 2, var) != 0

# Filtrar a matriz
filtered_matrix <- relation_matrix[, non_zero_var]

# Realizar PCA na matriz filtrada
pca_result <- prcomp(filtered_matrix, center = TRUE, scale. = TRUE)

# Criar um dataframe com os resultados para os dois primeiros componentes principais
pca_df <- data.frame(
  PC1 = pca_result$x[,1],
  PC2 = pca_result$x[,2],
  term = rownames(filtered_matrix)
)

# Plotar os resultados
ggplot(pca_df, aes(x = PC1, y = PC2, label = term)) +
  geom_point() +
  geom_text_repel(max.overlaps = 20) +  # Ajuste este valor conforme necessário
  theme_minimal() +
  labs(title = "PCA of GO:BP Terms Semantic Similarity",
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))

# Imprimir a proporção de variância explicada pelos primeiros componentes
print(summary(pca_result)$importance[2,1:5])

#usando qui-quadrado pasa de 1299600 para 1265625
#install.packages("pheatmap")
library(pheatmap)

create_distribution_heatmap <- function(cluster_names) {
  distribution_matrix <- matrix(0, nrow = length(cluster_names), ncol = 5)
  rownames(distribution_matrix) <- cluster_names
  colnames(distribution_matrix) <- c("D0", "D1", "D2", "D3", "D4")
  
  for (i in seq_along(cluster_names)) {
    cluster_genes <- parse_intersections(combined_stat_sig$intersections[combined_stat_sig$Name == cluster_names[i]])
    gene_counts <- table(filtered_drivers_df$new_category[filtered_drivers_df$driver %in% cluster_genes])
    distribution_matrix[i,] <- gene_counts[colnames(distribution_matrix)]
  }
  
  pheatmap(distribution_matrix, 
           main = "Distribution of genes across clusters and groups",
           color = colorRampPalette(c("white", "red"))(50),
           cluster_rows = FALSE, 
           cluster_cols = FALSE,
           display_numbers = TRUE)
}

# Exemplo de uso:
# Exemplo de filtragem usando PCA
selected_terms <- rownames(pca_df)[abs(pca_df$PC1) > 1 | abs(pca_df$PC2) > 1]  # Ajuste o limiar conforme necessário

filtered_stat_sig <- combined_stat_sig[combined_stat_sig$native %in% selected_terms, ]

# Geração do heatmap
cluster_names <- unique(filtered_stat_sig$Name)
create_distribution_heatmap(cluster_names)

#cluster_names <- unique(combined_stat_sig$Name)
#create_distribution_heatmap(cluster_names)

library(igraph)

create_semantic_similarity_network <- function(similarity_threshold = 0.7) {
  graph <- graph_from_adjacency_matrix(relation_matrix >= similarity_threshold, mode = "undirected", diag = FALSE)

  plot(graph,
       vertex.size = 10,
       vertex.label.cex = 0.8,
       edge.width = 1,
       main = "Semantic Similarity Network of GO:BP Terms")
}

create_semantic_similarity_network()

#nstall.packages("ggdendro")
library(ggplot2)
library(ggdendro)
library(pheatmap)

# Função para realizar a clusterização hierárquica e criar o dendrograma
create_hierarchical_clustering <- function(similarity_matrix) {
  # Converter similaridade em distância
  dist_matrix <- as.dist(1 - similarity_matrix)
  
  # Realizar clusterização hierárquica
  hc <- hclust(dist_matrix, method = "complete")
  
  # Criar dendrograma
  dend <- as.dendrogram(hc)
  
  # Plotar dendrograma
  ggdendrogram(dend, rotate = TRUE, size = 2) +
    labs(title = "Hierarchical Clustering of GO:BP Terms",
         x = "Height", y = "GO:BP Terms") +
    theme(axis.text.y = element_text(size = 6))
}

# Função para criar heatmap com clusterização
create_similarity_heatmap <- function(similarity_matrix) {
  pheatmap(similarity_matrix,
           clustering_distance_rows = as.dist(1 - similarity_matrix),
           clustering_distance_cols = as.dist(1 - similarity_matrix),
           main = "Semantic Similarity Heatmap with Hierarchical Clustering",
           color = colorRampPalette(c("white", "red"))(100),
           show_rownames = FALSE,
           show_colnames = FALSE,
           treeheight_row = 100,
           treeheight_col = 100)
}

# Executar as funções
# Filtrando a relation_matrix para os termos selecionados
filtered_relation_matrix <- relation_matrix[selected_terms, selected_terms]

# Criando o dendrograma e heatmap com a matriz filtrada
dendrogram_plot <- create_hierarchical_clustering(filtered_relation_matrix)
print(dendrogram_plot)

similarity_heatmap <- create_similarity_heatmap(filtered_relation_matrix)
print(similarity_heatmap)

#dendrogram_plot <- create_hierarchical_clustering(relation_matrix)
#print(dendrogram_plot)

#similarity_heatmap <- create_similarity_heatmap(relation_matrix)
#print(similarity_heatmap)

# Se quiser salvar os gráficos
#ggsave("go_bp_dendrogram.png", dendrogram_plot, width = 12, height = 8)
#ggsave("go_bp_similarity_heatmap.png", similarity_heatmap, width = 12, height = 10)




# Preparar dados GO para as três ontologias
hsGO_BP <- godata('org.Hs.eg.db', ont="BP", keytype = "SYMBOL", computeIC=TRUE)
#hsGO_MF <- godata('org.Hs.eg.db', ont="MF", keytype = "SYMBOL", computeIC=TRUE)
#hsGO_CC <- godata('org.Hs.eg.db', ont="CC", keytype = "SYMBOL", computeIC=TRUE)

# Read your data
final_results_df <- read.csv('enrichment_results.csv')
combined_stat <- read.csv("combined_statistical_results.csv")

# Filter significant terms
combined_stat_sig <- combined_stat[combined_stat$adjusted.p.value < 0.05, ]
significant_terms <- combined_stat_sig$Name

# Merge dataframes
combined_stat_sig <- merge(final_results_df, combined_stat_sig, by = "Name", all = FALSE)

# Transformar a coluna `intersections` em uma lista de clusters de genes
parse_intersections <- function(intersections) {
  # Remover caracteres não desejados e transformar em lista
  genes <- gsub("\\[|\\]|'|\\s", "", intersections)
  gene_list <- strsplit(genes, ",")[[1]]
  return(gene_list)
}

# Criar lista de clusters de genes para BP, MF e CC
bp_clusters <- lapply(combined_stat_sig$intersections[grep("GO:BP", combined_stat_sig$Source)], parse_intersections)
#mf_clusters <- lapply(combined_stat_sig$intersections[grep("GO:MF", combined_stat_sig$Source)], parse_intersections)
#cc_clusters <- lapply(combined_stat_sig$intersections[grep("GO:CC", combined_stat_sig$Source)], parse_intersections)

# Filtrar clusters que tenham mais de um gene para calcular a similaridade
bp_clusters <- Filter(function(x) length(x) > 1, bp_clusters)
#mf_clusters <- Filter(function(x) length(x) > 1, mf_clusters)
#cc_clusters <- Filter(function(x) length(x) > 1, cc_clusters)

# Função para calcular a similaridade entre clusters de genes usando mclusterSim
calculate_cluster_similarity <- function(clusters, semData) {
  n_cores <- min(detectCores(), 4)
  cluster_similarity <- mclapply(clusters, function(x) {
    if (length(x) > 1) {
      mclusterSim(list(x), semData = semData, measure = "Wang", combine = "BMA")
    } else {
      NULL
    }
  }, mc.cores = n_cores)
  return(cluster_similarity)
}

# Calcular similaridade entre os clusters para BP, MF e CC
bp_cluster_similarity <- calculate_cluster_similarity(bp_clusters, hsGO_BP)
#mf_cluster_similarity <- calculate_cluster_similarity(mf_clusters, hsGO_MF)
#cc_cluster_similarity <- calculate_cluster_similarity(cc_clusters, hsGO_CC)

# Verificar os resultados
if (!all(sapply(bp_cluster_similarity, is.null))) {
  print("Similaridade entre clusters calculada com sucesso para BP!")
  print(unlist(bp_cluster_similarity))
} else {
  print("Falha ao calcular similaridade entre clusters para BP.")
}

if (!all(sapply(mf_cluster_similarity, is.null))) {
  print("Similaridade entre clusters calculada com sucesso para MF!")
  print(unlist(mf_cluster_similarity))
} else {
  print("Falha ao calcular similaridade entre clusters para MF.")
}

if (!all(sapply(cc_cluster_similarity, is.null))) {
  print("Similaridade entre clusters calculada com sucesso para CC!")
  print(unlist(cc_cluster_similarity))
} else {
  print("Falha ao calcular similaridade entre clusters para CC.")
}


# Função ajustada para calcular similaridade semântica com tratamento de erros
calculate_similarity <- function(genes, semData) {
  if (length(genes) > 1) {
    sim_matrix <- tryCatch({
      mgeneSim(genes=genes, semData=semData, measure="Resnik", combine="max")
    }, error = function(e) {
      message("Erro ao calcular similaridade: ", e)
      return(NULL)
    })
    return(sim_matrix)
  } else {
    return(NULL)
  }
}

# Processar os genes em subconjuntos sequencialmente (sem paralelização)
process_in_chunks <- function(gene_ids, semData, chunk_size = 50) {
  gene_chunks <- split(gene_ids, ceiling(seq_along(gene_ids)/chunk_size))
  similarity_list <- lapply(gene_chunks, calculate_similarity, semData = semData)
  
  # Remover matrizes nulas e garantir que todas tenham o mesmo número de colunas
  similarity_list <- Filter(Negate(is.null), similarity_list)
  
  # Combinar as matrizes de similaridade de forma robusta
  if (length(similarity_list) > 0) {
    combined_similarity <- do.call(rbind, lapply(similarity_list, function(x) {
      if (is.null(dim(x))) return(matrix(NA, ncol = length(gene_ids), nrow = 0))
      return(x)
    }))
    return(combined_similarity)
  } else {
    return(NULL)
  }
}

# Função para mapear termos GO para genes e processar similaridade
map_and_process <- function(go_terms, semData) {
  gene_ids <- AnnotationDbi::select(org.Hs.eg.db, keys = go_terms, columns = "ENTREZID", keytype = "GO")
  gene_ids <- unique(na.omit(gene_ids$ENTREZID))
  process_in_chunks(gene_ids, semData)
}

# Obter termos GO do seu dataset filtrado e processar
bp_terms <- combined_stat_sig$native[grep("GO:BP", combined_stat_sig$Source)]
bp_similarity <- map_and_process(bp_terms, hsGO_BP)

# Verificar se bp_similarity é válida
if (!is.null(bp_similarity)) {
  print("Similaridade calculada com sucesso!")
} else {
  print("Falha ao calcular similaridade.")
}

# Você pode repetir o mesmo processo para MF e CC:
mf_terms <- combined_stat_sig$native[grep("GO:MF", combined_stat_sig$Source)]
mf_similarity <- map_and_process(mf_terms, hsGO_MF)

cc_terms <- combined_stat_sig$native[grep("GO:CC", combined_stat_sig$Source)]
cc_similarity <- map_and_process(cc_terms, hsGO_CC)

# Verificar se as similaridades foram calculadas corretamente
if (!is.null(mf_similarity)) {
  print("Similaridade calculada com sucesso para MF!")
} else {
  print("Falha ao calcular similaridade para MF.")
}

if (!is.null(cc_similarity)) {
  print("Similaridade calculada com sucesso para CC!")
} else {
  print("Falha ao calcular similaridade para CC.")
}

#########


# Load required libraries
library(GOSemSim)
library(org.Hs.eg.db)
#library(clusterProfiler)
library(dplyr)
library(AnnotationDbi)
library(parallel)


hsGO_BP <- godata('org.Hs.eg.db', ont="BP", computeIC=TRUE)
hsGO_MF <- godata('org.Hs.eg.db', ont="MF", computeIC=TRUE)
hsGO_CC <- godata('org.Hs.eg.db', ont="CC", computeIC=TRUE)

# Read your data
final_results_df <- read.csv('enrichment_results.csv')
combined_stat <- read.csv("combined_statistical_results.csv")

# Filter significant terms
combined_stat_sig <- combined_stat[combined_stat$adjusted.p.value < 0.05, ]
significant_terms <- combined_stat_sig$Name

# Merge dataframes
combined_stat_sig <- merge(final_results_df, combined_stat_sig, by = "Name", all = FALSE)

# Mapear termos GO:BP para genes
bp_terms <- combined_stat_sig %>% filter(grepl("GO:BP", Source))
bp_genes <- unique(bp_terms$native)
bp_gene_ids <- AnnotationDbi::select(org.Hs.eg.db, keys = bp_genes, columns = "ENTREZID", keytype = "GO")
bp_gene_ids <- unique(na.omit(bp_gene_ids$ENTREZID))

# Mapear termos GO:MF para genes
mf_terms <- combined_stat_sig %>% filter(grepl("GO:MF", Source))
mf_genes <- unique(mf_terms$native)
mf_gene_ids <- AnnotationDbi::select(org.Hs.eg.db, keys = mf_genes, columns = "ENTREZID", keytype = "GO")
mf_gene_ids <- unique(na.omit(mf_gene_ids$ENTREZID))

# Mapear termos GO:CC para genes
cc_terms <- combined_stat_sig %>% filter(grepl("GO:CC", Source))
cc_genes <- unique(cc_terms$native)
cc_gene_ids <- AnnotationDbi::select(org.Hs.eg.db, keys = cc_genes, columns = "ENTREZID", keytype = "GO")
cc_gene_ids <- unique(na.omit(cc_gene_ids$ENTREZID))

# Função ajustada para calcular similaridade semântica com tratamento de erros
calculate_similarity <- function(genes, semData) {
  if (length(genes) > 1) {
    sim_matrix <- tryCatch({
      mgeneSim(genes=genes, semData=semData, measure="Resnik", combine="max")
    }, error = function(e) {
      message("Erro ao calcular similaridade: ", e)
      return(NULL)
    })
    return(sim_matrix)
  } else {
    return(NULL)
  }
}

# Processar os genes em subconjuntos
process_in_chunks <- function(gene_ids, semData, chunk_size = 50) {
  gene_chunks <- split(gene_ids, ceiling(seq_along(gene_ids)/chunk_size))
  similarity_list <- mclapply(gene_chunks, calculate_similarity, semData = semData, mc.cores = detectCores())
  
  # Remover matrizes nulas e garantir que todas tenham o mesmo número de colunas
  similarity_list <- Filter(Negate(is.null), similarity_list)
  
  # Combinar as matrizes de similaridade de forma robusta
  if (length(similarity_list) > 0) {
    combined_similarity <- do.call(rbind, lapply(similarity_list, function(x) {
      if (is.null(dim(x))) return(matrix(NA, ncol = length(gene_ids), nrow = 0))
      return(x)
    }))
    return(combined_similarity)
  } else {
    return(NULL)
  }
}

# # Mapear termos GO para genes (para BP, MF, e CC)
map_and_process <- function(go_terms, semData) {
  gene_ids <-
    AnnotationDbi::select(org.Hs.eg.db,
                          keys = go_terms,
                          columns = "ENTREZID",
                          keytype = "GO")
  gene_ids <- unique(na.omit(gene_ids$ENTREZID))
  process_in_chunks(gene_ids, semData)
}
# Obter termos GO do seu dataset filtrado e processar
bp_terms <- combined_stat_sig$native[grep("GO:BP", combined_stat_sig$Source)]
bp_similarity <- map_and_process(bp_terms, hsGO_BP)

# Verificar se bp_similarity é válida
if (!is.null(bp_similarity)) {
  print("Similaridade calculada com sucesso!")
} else {
  print("Falha ao calcular similaridade.")
}


# # Função para calcular similaridade semântica com opções menos computacionais
# calculate_similarity <- function(genes, semData) {
#   if (length(genes) > 1) {
#     mgeneSim(
#       genes = genes,
#       semData = semData,
#       measure = "Wang",
#       combine = "BMA"
#     )
#   } else {
#     NA
#   }
# }
# 
# # Processar os genes em subconjuntos
# process_in_chunks <- function(gene_ids, semData, chunk_size = 100) {
#   gene_chunks <-
#     split(gene_ids, ceiling(seq_along(gene_ids) / chunk_size))
#   similarity_list <-
#     mclapply(gene_chunks,
#              calculate_similarity,
#              semData = semData,
#              mc.cores = detectCores())
#   do.call(rbind, lapply(similarity_list, as.matrix))
# }
# 

# 
# # Obter termos GO do seu dataset filtrado e processar
# bp_terms <-
#   combined_stat_sig$native[grep("GO:BP", combined_stat_sig$Source)]
# bp_similarity <- map_and_process(bp_terms, hsGO_BP)
# 
# mf_terms <-
#   combined_stat_sig$native[grep("GO:MF", combined_stat_sig$Source)]
# mf_similarity <- map_and_process(mf_terms, hsGO_MF)
# 
# cc_terms <-
#   combined_stat_sig$native[grep("GO:CC", combined_stat_sig$Source)]
# cc_similarity <- map_and_process(cc_terms, hsGO_CC)
# 
# # Clusterização hierárquica e corte de clusters
# cluster_and_cut <- function(similarity_matrix, k = 3) {
#   dist_matrix <- as.dist(1 - similarity_matrix)
#   hclust_res <- hclust(dist_matrix, method = "average")
#   cutree(hclust_res, k = k)
# }
# 
# bp_clusters <- cluster_and_cut(bp_similarity)
# mf_clusters <- cluster_and_cut(mf_similarity)
# cc_clusters <- cluster_and_cut(cc_similarity)
# 
# # Medir similaridade entre clusters
# measure_cluster_similarity <- function(clusters, semData) {
#   cluster_groups <- split(names(clusters), clusters)
#   mclusterSim(
#     cluster_groups,
#     semData = semData,
#     measure = "Resnik",
#     combine = "max"
#   )
# }
# 
# bp_cluster_similarity <-
#   measure_cluster_similarity(bp_clusters, hsGO_BP)
# mf_cluster_similarity <-
#   measure_cluster_similarity(mf_clusters, hsGO_MF)
# cc_cluster_similarity <-
#   measure_cluster_similarity(cc_clusters, hsGO_CC)
# 
# # Exibir os resultados
# print("Similaridade entre clusters para GO:BP")
# print(bp_cluster_similarity)
# 
# print("Similaridade entre clusters para GO:MF")
# print(mf_cluster_similarity)
# 
# print("Similaridade entre clusters para GO:CC")
# print(cc_cluster_similarity)

##
# Calcular a similaridade semântica para GO:BP
bp_similarity <- mgeneSim(genes=bp_gene_ids, semData=hsGO_BP, measure="Wang", combine="BMA")

# Calcular a similaridade semântica para GO:MF
mf_similarity <- mgeneSim(genes=mf_gene_ids, semData=hsGO_MF, measure="Wang", combine="BMA")

# Calcular a similaridade semântica para GO:CC
cc_similarity <- mgeneSim(genes=cc_gene_ids, semData=hsGO_CC, measure="Wang", combine="BMA")

# Clusterização hierárquica para GO:BP
bp_dist <- as.dist(1 - bp_similarity)
bp_hclust <- hclust(bp_dist, method="average")

# Clusterização hierárquica para GO:MF
mf_dist <- as.dist(1 - mf_similarity)
mf_hclust <- hclust(mf_dist, method="average")

# Clusterização hierárquica para GO:CC
cc_dist <- as.dist(1 - cc_similarity)
cc_hclust <- hclust(cc_dist, method="average")

# Definir o número de clusters
k <- 3

# Para GO:BP
bp_clusters <- cutree(bp_hclust, k = k)
bp_cluster_groups <- split(names(bp_clusters), bp_clusters)
bp_cluster_similarity <- mclusterSim(bp_cluster_groups, semData=hsGO_BP, measure="Wang", combine="BMA")
print("Similaridade entre clusters para GO:BP")
print(bp_cluster_similarity)

# Para GO:MF
mf_clusters <- cutree(mf_hclust, k = k)
mf_cluster_groups <- split(names(mf_clusters), mf_clusters)
mf_cluster_similarity <- mclusterSim(mf_cluster_groups, semData=hsGO_MF, measure="Wang", combine="BMA")
print("Similaridade entre clusters para GO:MF")
print(mf_cluster_similarity)

# Para GO:CC
cc_clusters <- cutree(cc_hclust, k = k)
cc_cluster_groups <- split(names(cc_clusters), cc_clusters)
cc_cluster_similarity <- mclusterSim(cc_cluster_groups, semData=hsGO_CC, measure="Wang", combine="BMA")
print("Similaridade entre clusters para GO:CC")
print(cc_cluster_similarity)






bp_terms <- combined_stat_sig[grep("GO:BP", combined_stat_sig$Source), ]
bp_genes <- bp_terms$intersections

mf_terms <- combined_stat_sig[grep("GO:MF", combined_stat_sig$Source), ]
mf_genes <- mf_terms$intersections

cc_terms <- combined_stat_sig[grep("GO:CC", combined_stat_sig$Source), ]
cc_genes <- cc_terms$intersections

# Calcular a similaridade semântica para os genes em BP
bp_similarity <- mgeneSim(genes=bp_genes, semData=hsGO_BP, measure="Wang", combine="BMA")
print("Similaridade Semântica para GO:BP")
print(bp_similarity)

# Calcular a similaridade semântica para os genes em MF
mf_similarity <- mgeneSim(genes=mf_genes, semData=hsGO_MF, measure="Wang", combine="BMA")
print("Similaridade Semântica para GO:MF")
print(mf_similarity)

# Calcular a similaridade semântica para os genes em CC
cc_similarity <- mgeneSim(genes=cc_genes, semData=hsGO_CC, measure="Wang", combine="BMA")
print("Similaridade Semântica para GO:CC")
print(cc_similarity)



# go_terms_df <- combined_stat_sig[grep("^GO:", combined_stat_sig$Source), ]
# 
# go_genes <- unique(unlist(strsplit(go_terms_df$intersections, ", ")))
# entrez_go_genes <- mapIds(org.Hs.eg.db, go_genes, "ENTREZID", "SYMBOL")
# entrez_go_genes <- entrez_go_genes[!is.na(entrez_go_genes)]
# 
# # Calcular similaridade semântica
# go_sim <- mgeneSim(entrez_go_genes, ont = "MF", organism = "human", measure = "Wang")
# 
# # Clustering hierárquico
# hc_go <- hclust(as.dist(1 - go_sim), method = "average")
# plot(hc_go)
# 
# # Cortar a árvore em clusters
# mycl_go <- cutree(hc_go, h = max(hc_go$height)/2)
# 
# # Separar os clusters
# subcls_go <- lapply(unique(mycl_go), function(x) names(mycl_go[mycl_go == x]))
# 
# # Calcular similaridades entre clusters
# cluster_go_similarity <- clusterSim(subcls_go[[1]], subcls_go[[2]], ont = "MF", organism = "human", measure = "Wang")
# mcluster_go_similarity <- mclusterSim(subcls_go, ont = "MF", organism = "human", measure = "Wang")
# 
# # Exibir resultados
# cluster_go_similarity
# mcluster_go_similarity

# Function to extract genes from intersection string
#extract_genes <- function(intersection_str) {
#  genes <- unlist(strsplit(gsub("\\[|\\]|'", "", intersection_str), ", "))
#  return(genes)
#}

# Extract genes for each GO term
#combined_stat_sig$gene_list <- lapply(combined_stat_sig$intersections, extract_genes)

# Calculate semantic similarity
#hsGO <- godata('org.Hs.eg.db', ont="BP", keytype="SYMBOL")

# # Function to calculate similarity between two gene lists
# calc_similarity <- function(genes1, genes2) {
#   sim <- mgeneSim(genes1, genes2, semData=hsGO, measure="Wang", combine="BMA")
#   return(mean(sim, na.rm = TRUE))
# }
# 
# # Calculate pairwise similarities
# n <- nrow(combined_stat_sig)
# sim_matrix <- matrix(0, nrow=n, ncol=n)
# 
# for (i in 1:(n-1)) {
#   for (j in (i+1):n) {
#     sim <- calc_similarity(combined_stat_sig$gene_list[[i]], combined_stat_sig$gene_list[[j]])
#     sim_matrix[i,j] <- sim
#     sim_matrix[j,i] <- sim
#   }
#   # Print progress
#   if (i %% 10 == 0) cat("Processed", i, "of", n, "terms\n")
# }
# 
# rownames(sim_matrix) <- colnames(sim_matrix) <- combined_stat_sig$Name
# 
# # Perform hierarchical clustering
# hr <- hclust(as.dist(1-sim_matrix), method="average")
# 
# # Cut tree to get clusters
# k <- 10  # You can adjust this number
# clusters <- cutree(hr, k=k)
# 
# # Add cluster information to the dataframe
# combined_stat_sig$cluster <- clusters
# 
# # Plot dendrogram
# pdf("go_term_dendrogram.pdf", width=20, height=15)
# plot(hr, main="GO Term Clustering Dendrogram", xlab="", sub="", labels=FALSE)
# dev.off()
# 
# # Print cluster memberships
# for (i in 1:k) {
#   cat("\nCluster", i, "members:\n")
#   print(combined_stat_sig$Name[combined_stat_sig$cluster == i])
# }
# 
# # Save results
# write.csv(combined_stat_sig, "go_terms_clustered.csv", row.names=FALSE)
# 
# 
# #library(stats)
# 
# results_df <- read.csv("enrichment_results_all_2.csv")
# filtered_drivers_df <- read.csv("filtered_drivers_df_2.csv")
# 
# num_drivers_D <- c(nrow(subset(
#   filtered_drivers_df, Majority_Group == 'D0'
# )),
# nrow(subset(
#   filtered_drivers_df, Majority_Group == 'D1'
# )),
# nrow(subset(
#   filtered_drivers_df, Majority_Group == 'D2'
# )),
# nrow(subset(
#   filtered_drivers_df, Majority_Group == 'D3'
# )),
# nrow(subset(
#   filtered_drivers_df, Majority_Group == 'D4'
# )))
# 
# data_list <- list()
# 
# for (term in 1:nrow(results_df)) {
#   values_df <-
#     as.numeric(results_df[term, c("D0", "D1", "D2", "D3", "D4")])
#   remaining_values <- num_drivers_D - values_df
#   matrix_2x5 <- rbind(values_df, remaining_values)
#   
#   # Check if both rows have at least one non-zero value
#   if (any(rowSums(matrix_2x5) > 0)) {
#     fisher_results <- tryCatch({
#       fisher.test(matrix_2x5, simulate.p.value = TRUE)
#     }, error = function(e) {
#       # If an error occurs, return NA
#       return(list(p.value = NA))
#     })
#     data_list[results_df[term, 'Name']] <- fisher_results$p.value
#   } else {
#     # If all values in a row are zero, assign NA
#     data_list[results_df[term, 'Name']] <- NA
#   }
# }
# 
# fisher_df <-
#   data.frame(
#     Name = names(data_list),
#     p.value = unlist(data_list),
#     stringsAsFactors = FALSE
#   )
# fisher_df$adjusted.p.value <-
#   p.adjust(fisher_df$p.value, method = "BH")
# write.csv(fisher_df, "results_fisher_test_adjusted_2.csv", row.names = FALSE)
# 


#data_list <- list()
#for (term in 1:nrow(results_df)) {
#  values_df <- as.numeric(results_df[term, c("D0", "D1", "D2", "D3", "D4")])
#  remaining_values <- num_drivers_D - values_df
#  term_data <- rbind(values_df, remaining_values)
#   data_list[[term]] <- term_data
#}
#dat <- do.call(rbind, data_list)
#colnames(dat) <- c("D0", "D1", "D2", "D3", "D4")

#test <- fisher.test(dat, simulate.p.value = TRUE)
#test

#p_values <- numeric(nrow(results_df))
#for (term in 1:nrow(results_df)) {
#  term_data <- dat[(2*term-1):(2*term), ] 
#  test <- fisher.test(term_data, simulate.p.value = TRUE)
#  p_values[term] <- test$p.value
#}

#significant_terms <- results_df[p_values < 0.05, ]
#significant_terms

#non_significant_terms <- results_df[p_values > 0.05, ]
#non_significant_terms

#write.csv(significant_terms, "significant_terms.csv", row.names = FALSE)
#write.csv(non_significant_terms, "non_significant_terms.csv", row.names = FALSE)
#write.csv(p_values[p_values < 0.05], "significant_p_values.csv", row.names = FALSE)
#write.csv(p_values[p_values > 0.05], "non_significant_p_values.csv", row.names = FALSE)


library(tidygraph)
library(ggraph)
library(visNetwork)
library(corpustools)

n_2018 <- trade_data %>% 
  filter(year == 2018) %>% 
  transmute(year, 
         from = reporter_fullname_english, 
         to = partner_fullname_english,
         weight = report_export_pct,
         value = weight) %>%
  as_tbl_graph()

bb <- backbone_filter(n_2018)

bb

visIgraph(bb) %>% 
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = list(enabled = TRUE,
                                     main = "Select by Country"),
             selectedBy = "name") %>% 
  visEdges(color = list(color = "#d1d3d4",
                        highlight = "darkred"),
           arrows = list(to = list(enabled = TRUE,
                                   scaleFactor = 1))) %>% 
  visNodes(color = list(highlight = "darkred"),
           scaling = list(min = 1,
                          max = 20),
           font = list(face = "Calibri"))

vn <- bb %>% 
  toVisNetworkData() 

nodes <- vn$nodes
edges <- vn$edges

visNetwork(nodes,
           edges) %>% 
  visEdges() %>%
  visNodes()


# change size from ratio to in-strength
nodes$size <- sqrt(nodes$sin)

visNetwork(nodes, edges,
           height = "700px", width = "100%") %>%
  visOptions(highlightNearest = TRUE,
             nodesIdSelection = list(enabled = TRUE,
                                     main = "Select by SUA"),
             selectedBy = "community") %>%
  visEdges(color = list(color = "#d1d3d4",
                        highlight = "darkred"),
           arrows = list(to = list(enabled = TRUE,
                                   scaleFactor = 1))) %>%
  visNodes(scaling = list(min = 1,
                          max = 20),
           font = list(face = "Calibri"))
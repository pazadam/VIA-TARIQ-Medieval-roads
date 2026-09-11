####Data preparation, network creation
##Load libraries
library(sf)
library(sfnetworks)
library(dplyr)
library(tidyr)
library(igraph)
library(ggplot2)

##Load data, sites=nodes, roads=edges
#Roman (ROM)
sites_rom <- sf::st_read("data/sites_roman.shp")
roads_rom <- sf::st_read("data/roads_roman.shp")
#Early Islamic (EI)
sites_ei <- sf::st_read("data/sites_early_islamic.shp")
roads_ei <- sf::st_read("data/roads_early_islamic.shp")
#Middle Islamic (MI)
sites_mi <- sf::st_read("data/sites_middle_islamic.shp")
roads_mi <- sf::st_read("data/roads_middle_islamic.shp")
#Early Mamluk (EM)
sites_em <- sf::st_read("data/sites_early_mamluk.shp")
roads_em <- sf::st_read("data/roads_early_mamluk.shp")
#Late Mamluk (LM)
sites_lm <- sf::st_read("data/sites_late_mamluk.shp")
roads_lm <- sf::st_read("data/roads_late_mamluk.shp")

###Creating networks
##In order to link the nodes to the edges, we need a node key associated with the edges. We get start and end points of the edges and associate them with the nodes IDs, then add 2 new fields <from_ID> and <to_ID> to the edge objects
get_endpoints <- function(line, position = "start") {
  coords <- st_coordinates(line)
  if (position == "start") {
    st_point(coords[1, 1:2])
  } else {
    st_point(coords[nrow(coords), 1:2])
  }
}

##Roman roads
start_points <- st_sfc(lapply(st_geometry(roads_rom), get_endpoints, "start"), crs = st_crs(roads_rom))
end_points   <- st_sfc(lapply(st_geometry(roads_rom), get_endpoints, "end"), crs = st_crs(roads_rom))

start_sf <- st_sf(geometry = start_points)
end_sf   <- st_sf(geometry = end_points)

start_nearest <- st_nearest_feature(start_sf, sites_rom)
end_nearest   <- st_nearest_feature(end_sf, sites_rom)

roads_rom$from_id <- sites_rom$id[start_nearest]
roads_rom$to_id   <- sites_rom$id[end_nearest]

#Convert id, from_id and to_id fields to character. They are currently numeric type which sfnetworks does not accept.
roads_rom$from_id <- as.character(roads_rom$from_id)
roads_rom$to_id   <- as.character(roads_rom$to_id)
sites_rom$id   <- as.character(sites_rom$id)

#Reorder columns
roads_rom_ord <- roads_rom[, c("from_id", "to_id", "id", "Type", "typeWeight", "itAnt", "lengthGeo", "avgSlope", "pace", "timeWeight", "geometry")]
roads_rom_ord <- rename(roads_rom_ord, "typeCode" = "typeWeight")
#Drop Z values
roads_rom_ord <- st_zm(roads_rom_ord, drop = TRUE, what = "ZM")

#Build the network
road_network_rom <- as_sfnetwork(x = sites_rom, edges = roads_rom_ord, node_key = "id", from = "from_id", to = "to_id", directed = FALSE, edges_as_lines = TRUE, length_as_weight = FALSE)

##Early Islamic roads
start_points <- st_sfc(lapply(st_geometry(roads_ei), get_endpoints, "start"), crs = st_crs(roads_ei))
end_points   <- st_sfc(lapply(st_geometry(roads_ei), get_endpoints, "end"), crs = st_crs(roads_ei))

start_sf <- st_sf(geometry = start_points)
end_sf   <- st_sf(geometry = end_points)

start_nearest <- st_nearest_feature(start_sf, sites_ei)
end_nearest   <- st_nearest_feature(end_sf, sites_ei)

roads_ei$from_id <- sites_ei$Id[start_nearest]
roads_ei$to_id   <- sites_ei$Id[end_nearest]

#Convert id, from_id and to_id fields to character. They are currently numeric type which sfnetworks does not accept.
roads_ei$from_id <- as.character(roads_ei$from_id)
roads_ei$to_id   <- as.character(roads_ei$to_id)
sites_ei$Id   <- as.character(sites_ei$Id)

#Reorder columns
roads_ei_ord <- roads_ei[, c("from_id", "to_id", "Id", "name", "type", "typeCode", "roadCer", "hajj", "barid", "lengthGeo", "avgSlope", "pace", "timeWeight", "geometry")]

#Build the network
road_network_ei <- as_sfnetwork(x = sites_ei, edges = roads_ei_ord, node_key = "Id", from = "from_id", to = "to_id", directed = FALSE, edges_as_lines = TRUE, length_as_weight = FALSE)

##Middle Islamic roads
start_points <- st_sfc(lapply(st_geometry(roads_mi), get_endpoints, "start"), crs = st_crs(roads_mi))
end_points   <- st_sfc(lapply(st_geometry(roads_mi), get_endpoints, "end"), crs = st_crs(roads_mi))

start_sf <- st_sf(geometry = start_points)
end_sf   <- st_sf(geometry = end_points)

start_nearest <- st_nearest_feature(start_sf, sites_mi)
end_nearest   <- st_nearest_feature(end_sf, sites_mi)

roads_mi$from_id <- sites_mi$Id[start_nearest]
roads_mi$to_id   <- sites_mi$Id[end_nearest]

#Convert id, from_id and to_id fields to character. They are currently numeric type which sfnetworks does not accept.
roads_mi$from_id <- as.character(roads_mi$from_id)
roads_mi$to_id   <- as.character(roads_mi$to_id)
sites_mi$Id   <- as.character(sites_mi$Id)

#Reorder columns
roads_mi_ord <- roads_mi[, c("from_id", "to_id", "Id", "name", "type", "typeCode", "roadCer", "hajj", "lengthGeo", "avgSlope", "pace", "timeWeight", "geometry")]

#Build the network
road_network_mi <- as_sfnetwork(x = sites_mi, edges = roads_mi_ord, node_key = "Id", from = "from_id", to = "to_id", directed = FALSE, edges_as_lines = TRUE, length_as_weight = FALSE)

##Early Mamluk
start_points <- st_sfc(lapply(st_geometry(roads_em), get_endpoints, "start"), crs = st_crs(roads_em))
end_points   <- st_sfc(lapply(st_geometry(roads_em), get_endpoints, "end"), crs = st_crs(roads_em))

start_sf <- st_sf(geometry = start_points)
end_sf   <- st_sf(geometry = end_points)

start_nearest <- st_nearest_feature(start_sf, sites_em)
end_nearest   <- st_nearest_feature(end_sf, sites_em)

roads_em$from_id <- sites_em$Id[start_nearest]
roads_em$to_id   <- sites_em$Id[end_nearest]

#Convert id, from_id and to_id fields to character. They are currently numeric type which sfnetworks does not accept.
roads_em$from_id <- as.character(roads_em$from_id)
roads_em$to_id   <- as.character(roads_em$to_id)
sites_em$Id   <- as.character(sites_em$Id)

#Reorder columns
roads_em_ord <- roads_em[, c("from_id", "to_id", "Id", "name", "type", "typeCode", "roadCer", "hajj", "barid", "lengthGeo", "avgSlope", "pace", "timeWeight", "geometry")]

#Build the network
road_network_em <- as_sfnetwork(x = sites_em, edges = roads_em_ord, node_key = "Id", from = "from_id", to = "to_id", directed = FALSE, edges_as_lines = TRUE, length_as_weight = FALSE)

##Late Mamluk
start_points <- st_sfc(lapply(st_geometry(roads_lm), get_endpoints, "start"), crs = st_crs(roads_lm))
end_points   <- st_sfc(lapply(st_geometry(roads_lm), get_endpoints, "end"), crs = st_crs(roads_lm))

start_sf <- st_sf(geometry = start_points)
end_sf   <- st_sf(geometry = end_points)

start_nearest <- st_nearest_feature(start_sf, sites_lm)
end_nearest   <- st_nearest_feature(end_sf, sites_lm)

roads_lm$from_id <- sites_lm$Id[start_nearest]
roads_lm$to_id   <- sites_lm$Id[end_nearest]

#Convert id, from_id and to_id fields to character. They are currently numeric type which sfnetworks does not accept.
roads_lm$from_id <- as.character(roads_lm$from_id)
roads_lm$to_id   <- as.character(roads_lm$to_id)
sites_lm$Id   <- as.character(sites_lm$Id)

#Reorder columns
roads_lm_ord <- roads_lm[, c("from_id", "to_id", "Id", "name", "type", "typeCode", "roadCer", "hajj", "barid", "lengthGeo", "avgSlope", "pace", "timeWeight", "geometry")]

#Build the network
road_network_lm <- as_sfnetwork(x = sites_lm, edges = roads_lm_ord, node_key = "Id", from = "from_id", to = "to_id", directed = FALSE, edges_as_lines = TRUE, length_as_weight = FALSE)

####Analysis
###Global measures (number of edges, average degree, density, diameter, global clustering coefficient, average local clustering coefficient, gamma index, alpha index, detour)
#Make a list of networks
networks <- list(roman = road_network_rom, early_islamic = road_network_ei, middle_islamic = road_network_mi, early_mamluk = road_network_em, late_mamluk = road_network_lm)

#Number of edges
number_of_edges_df <- networks %>% 
  lapply(ecount) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","number_of_edges"))

#Number of nodes
number_of_nodes_df <- networks %>% 
  lapply(vcount) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","number_of_nodes"))

#Average degree
degree_df <- networks %>% 
  lapply(degree) %>%
  lapply(mean) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","avg_degree"))

#Network diameter (time weighted)
diameter_time_df <- networks %>%
  lapply(\(g) diameter(g, weights = E(g)$timeWeight)) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","diameter_time"))

#Clustering coefficient global (CCG)
ccg_df <- networks %>%
  lapply(\(g) transitivity(g, type = "global")) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","ccg"))

#Gamma index
gamma_df <- networks %>%
  lapply(function(g) {
    ecount(g)/(3*(vcount(g)-2))
  }) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","gamma_index"))

#Alpha index
alpha_df <- networks %>%
  lapply(function(g) {
    (ecount(g)-vcount(g)+1)/(2*vcount(g)-5)
  }) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network","alpha_index"))

#Detour index (mean)
detour_df <- networks %>%
  lapply(function(net) {
    
    edges <- net %>%
      activate(edges) %>%
      st_as_sf()
    
    distance_euc <- units::drop_units(
      st_distance(
        st_transform(st_sfc(lapply(st_geometry(edges), get_endpoints, "start"), crs = st_crs(edges)), 4326),
        st_transform(st_sfc(lapply(st_geometry(edges), get_endpoints, "end"), crs = st_crs(edges)), 4326),
        by_element = TRUE
      )
    )
    
    mean(distance_euc / edges$lengthGeo, na.rm = TRUE)
  }) %>%
  stack() %>%
  dplyr::select(ind, values) %>%
  setNames(c("network", "detour_mean"))

#Networks properties table
networks_properties <- list(number_of_edges_df, number_of_nodes_df, degree_df, diameter_time_df, ccg_df, gamma_df, alpha_df, detour_df)
networks_properties <- Reduce(
  function(x, y) left_join(x, y, by = "network"),
  networks_properties
)
write.csv2(networks_properties, "outputs/networks_properties_table.csv")

###Centrality measures
##Edges
#Time-weighted betweenness
for (i in seq_along(networks)) {
  net <- networks[[i]]
  g <- net %>%
    activate(edges) %>%
    as.igraph()
  btw_time <- edge_betweenness(g, e = E(g), directed = FALSE, weights = E(g)$timeWeight)
  #Time-weighted betweenness normalised
  btw_time_n <- btw_time / max(btw_time, na.rm = TRUE)
  #Add as an edge attribute
  networks[[i]] <- net %>%
    activate("edges") %>%
    mutate(betTime = btw_time,
           betTimeN = btw_time_n
    )
}

##Nodes
#Time-weighted betweenness
for (i in seq_along(networks)) {
  net <- networks[[i]]
  g <- net %>%
    activate(edges) %>%
    as.igraph()
  btw_time <- betweenness(g, v = V(g), directed = FALSE, weights = E(g)$timeWeight)
  #Time-weighted betweenness normalised
  btw_time_n <- btw_time / max(btw_time, na.rm = TRUE)
  #Add as an node attribute
  networks[[i]] <- net %>%
    activate("nodes") %>%
    mutate(betTime = btw_time,
           betTimeN = btw_time_n
    )
}

#Time-weighted closeness
for (i in seq_along(networks)) {
  net <- networks[[i]]
  g <- net %>%
    activate(edges) %>%
    as.igraph()
  clo_time <- closeness(g, v = V(g), weights = E(g)$timeWeight)
  #Time-weighted betweenness normalised
  clo_time_n <- clo_time / max(clo_time, na.rm = TRUE)
  #Add as an node attribute
  networks[[i]] <- net %>%
    activate("nodes") %>%
    mutate(cloTime = clo_time,
           cloTimeN = clo_time_n
    )
}

#Export edges and nodes
for (i in seq_along(networks)) {
  net <- networks[[i]]
  name <- names(networks)[i]
  
  edges <- net %>%
    activate(edges) %>%
    st_as_sf()
  write_sf(edges, file.path("outputs", paste0(name, "_roads_analysis.shp")))
  
  nodes <- net %>%
    activate(nodes) %>%
    st_as_sf()
  write_sf(nodes, file.path("outputs", paste0(name, "_nodes_analysis.shp")))
}

###Robustness
###In order to evaluate robustness of the results of the centrality metrics (degree, time-weighted edge betweenness), a simple method adapted from 'Network Science in Archaeology' using Spearman's rho is used here.
sim_missing_edges <- function(net,
                              nsim = 1000,
                              props = c(0.9, 0.8, 0.7, 0.6, 0.5,
                                        0.4, 0.3, 0.2, 0.1),
                              met = NA,
                              missing_probs = NA) {
  # Initialize required library
  require(reshape2)
  
  props <- as.vector(props)
  
  if (FALSE %in% (is.numeric(props) & (props > 0) & (props <= 1))) {
    stop("Variable props must be numeric and be between 0 and 1",
         call. = F)
  }
  
  # Select measure of interest based on variable met and calculate
  if (!(met %in% c("degree", "betweenness"))) {
    stop(
      "Argument met must be either degree, betweenness, or eigenvector.
      Check function call.",
      call. = F
    )
  }
  else {
    if (met == "degree") {
      met_orig <- igraph::degree(net)
    }
    else  {
      if (met == "betweenness") {
        met_orig <- igraph::edge_betweenness(net, directed = FALSE, weights = E(net)$timeWeight)  # <<< UPDATED LINE
      }
    }
  }
  
  # Create data frame for output and name columns
  output <- matrix(NA, nsim, length(props))
  colnames(output) <- as.character(props)
  
  # Iterate over each value of props and then each value from 1 to nsim
  for (j in seq_len(length(props))) {
    for (i in 1:nsim) {
      # Run code in brackets if missing_probs is NA
      if (is.na(missing_probs)[1]) {
        sub_samp <- sample(seq(1, ecount(net)),
                           size = round(ecount(net) * props[j], 0))
        sub_net <- igraph::delete_edges(net, which(!(seq(1, ecount(net))
                                                     %in% sub_samp)))
      }
      # Run code in brackets if missing_probs contains values
      else {
        sub_samp <- sample(seq(1, ecount(net)), prob = missing_probs,
                           size = round(ecount(net) * props[j], 0))
        sub_net <- igraph::delete_edges(net, which(!(seq(1, ecount(net))
                                                     %in% sub_samp)))
      }
      
      # Select measure of interest based on met and calculate
      if (met == "degree") {
        temp_stats <- igraph::degree(sub_net)
        output[i, j] <- suppressWarnings(cor(temp_stats,
                                             met_orig,
                                             method = "spearman"))
      }
      else   {
        if (met == "betweenness") {
          temp_stats <- igraph::edge_betweenness(sub_net, directed = FALSE, weights = E(sub_net)$timeWeight)  # <<< UPDATED LINE
          kept_edges <- which(seq_len(ecount(net)) %in% sub_samp)  # <<< ADDED LINE
          
          if (length(temp_stats) == length(kept_edges) &&
              length(unique(temp_stats)) >= 2 &&
              length(unique(met_orig[kept_edges])) >= 2) {
            output[i, j] <- suppressWarnings(
              cor(temp_stats, met_orig[kept_edges], method = "spearman")  # <<< UPDATED LINE
            )
          } else {
            output[i, j] <- NA  # <<< ADDED LINE
          }
        }
      }
    }
  }
  
  # Return output as data.frame
  df_output <- suppressWarnings(melt(as.data.frame(output)))
  return(df_output)
}

##Time-weighted edge betweenness robustness
set.seed(99)
robustness <- lapply(networks, function(net) {
  
  g <- net %>%
    activate(edges) %>%
    as.igraph()
  
  sim_missing_edges(net = g, met = "betweenness")
})

##Plot the results
robustness_plots <- lapply(names(robustness), function(nm) {
  
  ggplot(robustness[[nm]]) +
    geom_boxplot(aes(x = variable, y = value)) +
    labs(x = "Sub-Sample Size as Proportion of Original", y = "Spearman's" ~ rho, title = paste0(nm, " edge betweenness robustness")) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = rel(1)),
      axis.text.y = element_text(size = rel(1)),
      axis.title.x = element_text(size = rel(1)),
      axis.title.y = element_text(size = rel(1)),
      legend.text = element_text(size = rel(1))
    )
})

names(robustness_plots) <- names(robustness)

#Export
lapply(names(robustness_plots), function(nm) {
  
  ggsave(
    filename = file.path(
      "outputs/",
      paste0(nm, "_edge_betweenness_robustness.tiff")
    ),
    plot = robustness_plots[[nm]],
    width = 8,
    height = 6,
    units = "in",
    dpi = 300
  )
  
})

#Summarise values (min, max, mean, median Spearman's rho per subsample size and period) in table
robustness_table <- bind_rows(
  lapply(seq_along(robustness), function(i) {
    robustness[[i]] %>%
      group_by(variable) %>%
      summarise(
        min = min(value, na.rm = TRUE),
        max = max(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        median = median(value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(dataframe = names(robustness)[i])
  })
) %>%
  select(dataframe, variable, min, max, mean, median)

write.csv2(robustness_table, file = "outputs/robustness_table.csv")

###############################################################################
###Plot change in normalised time-weighted betweenness (nodes)
##Roman to EI
###############################################################################
roman_nodes <- networks$roman %>%
  activate(nodes) %>%
  st_as_sf()

ei_nodes <- networks$early_islamic %>%
  activate(nodes) %>%
  st_as_sf()

#Get reciprocal nearest neighbours
ei_to_rom <- st_nearest_feature(ei_nodes, roman_nodes)
rom_to_ei <- st_nearest_feature(roman_nodes, ei_nodes)

ei_idx <- seq_len(nrow(ei_nodes))
keep <- rom_to_ei[ei_to_rom] == ei_idx

#Calculate distances and keep only matches below 10 km
distance <- st_distance(
  ei_nodes[keep, ],
  roman_nodes[ei_to_rom[keep], ],
  by_element = TRUE
)

keep2 <- units::drop_units(distance) <= 10000

matches <- data.frame(
  ei_id = ei_nodes$Id[keep][keep2],
  roman_id = roman_nodes$id[ei_to_rom[keep][keep2]])

roman_common_nodes_ei <- roman_nodes[roman_nodes$id %in% matches$roman_id, ]

ei_common_nodes_roman <- ei_nodes[ei_nodes$Id %in% matches$ei_id, ]

#Filter id, names, feature type, normalised edge betweenness
roman_common_nodes_ei_df <- roman_common_nodes_ei %>%
  select(id, name, featureTyp, betTimeN) %>%
  rename(type = featureTyp, betRom = betTimeN) %>%
  st_drop_geometry()

ei_common_nodes_roman_df <- ei_common_nodes_roman %>%
  select(Id, name, type, betTimeN) %>%
  rename(betEI = betTimeN) %>%
  st_drop_geometry()

#Match and merge
rom_ei_matched <- matches %>%
  left_join(roman_common_nodes_ei_df, by = c("roman_id" = "id"))

rom_ei_matched <- rom_ei_matched %>%
  left_join(
    ei_common_nodes_roman_df,
    by = c("ei_id" = "Id"),
    suffix = c("_roman", "_ei")
  )

rom_ei_matched <- rom_ei_matched %>%
  mutate(name = paste(name_roman, name_ei, sep = "/")) %>%
  mutate(diff = betEI - betRom) 
  
#Overview table
rom_ei_betweenness_table <- rom_ei_matched %>%
  select(name_roman, type_roman, name_ei, type_ei, betRom, betEI, diff)
write.csv2(rom_ei_betweenness_table, file = "outputs/rom_ei_betweenness.csv")
  
#Pivot to prepare data for plotting a dumbbell plot
rom_ei_matched <- rom_ei_matched  %>%
  pivot_longer(cols = c(betRom, betEI), names_to = "period", values_to = "betweenness")

#Betweenness cities (Roman)
betCity <- rom_ei_matched %>%
  filter(type_roman == "city")

betRomCity <- betCity %>%
  filter(period == "betRom")

#Order by descending betweenness
betRomOrd <- betRomCity %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betCityOrd <- betCity %>%
  mutate(name = factor(name, levels = rev(betRomOrd)))

betRomCityOrd <- betCityOrd %>%
  filter(period == "betRom")

betEICityOrd <- betCityOrd %>%
  filter(period == "betEI")

betCityRomEIPlot <- ggplot(betCityOrd)+
  geom_segment(data = betRomCityOrd,
               aes(x = betweenness, y = name,
                   yend = betEICityOrd$name, xend = betEICityOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betRom = "Roman", betEI = "Early Islamic"))+
  theme_bw()

ggsave(filename = "betweenness_cities_RomEI.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness forts (Roman)
betForts <- rom_ei_matched %>%
  filter(type_roman == "fort")

betRomForts <- betForts %>%
  filter(period == "betRom")

#Order by descending betweenness
betRomOrd <- betRomForts %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betFortsOrd <- betForts %>%
  mutate(name = factor(name, levels = rev(betRomOrd)))

betRomFortsOrd <- betFortsOrd %>%
  filter(period == "betRom")

betEIFortsOrd <- betFortsOrd %>%
  filter(period == "betEI")

betFortsRomEIPlot <- ggplot(betFortsOrd)+
  geom_segment(data = betRomFortsOrd,
               aes(x = betweenness, y = name,
                   yend = betEIFortsOrd$name, xend = betEIFortsOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betRom = "Roman", betEI = "Early Islamic"))+
  theme_bw()

ggsave(filename = "betweenness_forts_RomEI.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness stations (Roman)
betStations <- rom_ei_matched %>%
  filter(type_roman == "station")

betRomStations <- betStations %>%
  filter(period == "betRom")

#Order by descending betweenness
betRomOrd <- betRomStations %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betStationsOrd <- betStations %>%
  mutate(name = factor(name, levels = rev(betRomOrd)))

betRomStationsOrd <- betStationsOrd %>%
  filter(period == "betRom")

betEIStationsOrd <- betStationsOrd %>%
  filter(period == "betEI")

betStationsRomEIPlot <- ggplot(betStationsOrd)+
  geom_segment(data = betRomStationsOrd,
               aes(x = betweenness, y = name,
                   yend = betEIStationsOrd$name, xend = betEIStationsOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betRom = "Roman", betEI = "Early Islamic"))+
  theme_bw()

ggsave(filename = "betweenness_stations_RomEI.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Plot new nodes in EI
ei_nodes_new <- ei_nodes %>%
  filter(!Id %in% ei_common_nodes_roman$Id) %>%
  select(Id, name, type, betTimeN) %>%
  rename(betEI = betTimeN) %>%
  st_drop_geometry()

betEINewOrd <- ei_nodes_new %>%
  arrange(desc(betEI)) %>%
  mutate(name = factor(name, levels = rev(name)))

betEINewOrdPlot <- ggplot(betEINewOrd[betEINewOrd$betEI > 0, ])+
  geom_point(aes(x = betEI, y = name), colour = "#F8766D", size = 2, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness")+
  theme_bw()

ggsave(filename = "betweenness_EI_new.tiff", path = "outputs/", device = "tiff", dpi = 300)

######################################################################3
##Early Islamic to Middle Islamic
mi_nodes <- networks$middle_islamic %>%
  activate(nodes) %>%
  st_as_sf()

#Get nodes that continue (matching name, Id)
ei_mi_identical <- ei_nodes %>%
  st_drop_geometry() %>%
  select(ei_id = Id, name) %>%
  inner_join(
    mi_nodes %>%
      st_drop_geometry() %>%
      select(mi_id = Id, name),
    by = c("ei_id" = "mi_id", "name" = "name")
  )

ei_mi_identical <- ei_mi_identical %>%
  mutate(mi_id = ei_id) %>%
  select(mi_id, ei_id)

#Get remaining nodes
ei_remaining <- ei_nodes %>%
  filter(!Id %in% ei_mi_identical$ei_id)

mi_remaining <- mi_nodes %>%
  filter(!Id %in% ei_mi_identical$ei_id)

#Get reciprocal nearest neighbours
mi_to_ei <- st_nearest_feature(mi_remaining, ei_remaining)
ei_to_mi <- st_nearest_feature(ei_remaining, mi_remaining)

keep <- ei_to_mi[mi_to_ei] == seq_len(nrow(mi_remaining))

distance <- st_distance(
  mi_remaining[keep, ],
  ei_remaining[mi_to_ei[keep], ],
  by_element = TRUE
)

keep2 <- !is.na(distance) &
  units::drop_units(distance) <= 10000

matches_spatial <- data.frame(
  mi_id = mi_remaining$Id[keep][keep2],
  ei_id = ei_remaining$Id[mi_to_ei[keep][keep2]]
)

#Merge matches
matches <- bind_rows(ei_mi_identical, matches_spatial)

ei_common_nodes_mi <- ei_nodes[ei_nodes$Id %in% matches$ei_id, ]

mi_common_nodes_ei <- mi_nodes[mi_nodes$Id %in% matches$mi_id, ]

#Filter id, names, feature type, normalised edge betweenness
ei_common_nodes_mi_df <- ei_common_nodes_mi %>%
  select(Id, name, type, betTimeN) %>%
  rename(name_ei = name, betEI = betTimeN) %>%
  st_drop_geometry()

mi_common_nodes_ei_df <- mi_common_nodes_ei %>%
  select(Id, name, type, betTimeN) %>%
  rename(name_mi = name, betMI = betTimeN) %>%
  st_drop_geometry()

#Match and merge
ei_mi_matched <- matches %>%
  left_join(ei_common_nodes_mi_df, by = c("ei_id" = "Id"))

ei_mi_matched <- ei_mi_matched %>%
  left_join(
    mi_common_nodes_ei_df,
    by = c("mi_id" = "Id"),
    suffix = c("_ei", "_mi")
  )

ei_mi_matched <- ei_mi_matched %>%
  mutate(name = paste(name_ei, name_mi, sep = "/")) %>%
  mutate(diff = betMI - betEI)

#Overview table
ei_mi_betweenness_table <- ei_mi_matched %>%
  select(name_ei, type_ei, name_mi, type_mi, betEI, betMI, diff)
write.csv2(ei_mi_betweenness_table, file = "outputs/ei_mi_betweenness.csv")

#Pivot to prepare data for plotting a dumbbell plot
ei_mi_matched <- ei_mi_matched  %>%
  pivot_longer(cols = c(betEI, betMI), names_to = "period", values_to = "betweenness")

#Betweenness cities (Early Islamic)
betCity <- ei_mi_matched %>%
  filter(type_ei == "city")

betEICity <- betCity %>%
  filter(period == "betEI")

#Order by descending betweenness
betEIOrd <- betEICity %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betCityOrd <- betCity %>%
  mutate(name = factor(name, levels = rev(betEIOrd)))

betEICityOrd <- betCityOrd %>%
  filter(period == "betEI")

betMICityOrd <- betCityOrd %>%
  filter(period == "betMI")

betCityEIMIPlot <- ggplot(betCityOrd)+
  geom_segment(data = betEICityOrd,
               aes(x = betweenness, y = name,
                   yend = betMICityOrd$name, xend = betMICityOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betEI = "Early Islamic", betMI = "Middle Islamic"))+
  theme_bw()

ggsave(filename = "betweenness_cities_EIMI.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness others (Early Islamic)
betOthers <- ei_mi_matched %>%
  filter(type_ei == "fort" | type_ei == "station" | type_ei == "place" | type_ei == "bridge" | type_ei == "sanctuary")

betEIOthers <- betOthers %>%
  filter(period == "betEI")

#Order by descending betweenness
betEIOrd <- betEIOthers %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betOthersOrd <- betOthers %>%
  mutate(name = factor(name, levels = rev(betEIOrd)))

betEIOthersOrd <- betOthersOrd %>%
  filter(period == "betEI")

betMIOthersOrd <- betOthersOrd %>%
  filter(period == "betMI")

betOthersEIMIPlot <- ggplot(betOthersOrd)+
  geom_segment(data = betEIOthersOrd,
               aes(x = betweenness, y = name,
                   yend = betMIOthersOrd$name, xend = betMIOthersOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betEI = "Early Islamic", betMI = "Middle Islamic"))+
  theme_bw()

ggsave(filename = "betweenness_others_EIMI.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Plot new nodes in MI
mi_nodes_new <- mi_nodes %>%
  filter(!Id %in% mi_common_nodes_ei$Id) %>%
  select(Id, name, type, betTimeN) %>%
  rename(betMI = betTimeN) %>%
  st_drop_geometry()

betMINewOrd <- mi_nodes_new %>%
  arrange(desc(betMI)) %>%
  mutate(label = paste0(name, "-", type), label = factor(label, levels = rev(label)))

#MI Forts
betMINewFort <- betMINewOrd %>%
  filter(type == "fort")

betMINewFortPlot <- ggplot(betMINewFort)+
  geom_point(aes(x = betMI, y = label), colour = "#00BFC4", size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness")+
  theme_bw()

ggsave(filename = "betweenness_MI_new_fort.tiff", path = "outputs/", device = "tiff", dpi = 300)

#MI new others
betMINewOthers <- betMINewOrd %>%
  filter(type == "settlement" | type == "station" | type == "place" | type == "bridge" | type == "sanctuary" | type == "node")

betMINewOthersPlot <- ggplot(betMINewOthers)+
  geom_point(aes(x = betMI, y = label), colour = "#00BFC4", size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness")+
  theme_bw()

ggsave(filename = "betweenness_MI_new_others.tiff", path = "outputs/", device = "tiff", dpi = 300)

###############################################################
##Middle Islamic to Early Mamluk
em_nodes <- networks$early_mamluk %>%
  activate(nodes) %>%
  st_as_sf()

#Get nodes that continue (matching name, Id)
mi_em_identical <- mi_nodes %>%
  st_drop_geometry() %>%
  select(mi_id = Id, name) %>%
  inner_join(
    em_nodes %>%
      st_drop_geometry() %>%
      select(em_id = Id, name),
    by = c("mi_id" = "em_id", "name" = "name")
  )

mi_em_identical <- mi_em_identical %>%
  mutate(em_id = mi_id) %>%
  select(em_id, mi_id)

#Get remaining nodes
mi_remaining <- mi_nodes %>%
  filter(!Id %in% mi_em_identical$mi_id)

em_remaining <- em_nodes %>%
  filter(!Id %in% mi_em_identical$em_id)

#Get reciprocal nearest neighbours
em_to_mi <- st_nearest_feature(em_remaining, mi_remaining)
mi_to_em <- st_nearest_feature(mi_remaining, em_remaining)

keep <- mi_to_em[em_to_mi] == seq_len(nrow(em_remaining))

distance <- st_distance(
  em_remaining[keep, ],
  mi_remaining[em_to_mi[keep], ],
  by_element = TRUE
)

keep2 <- !is.na(distance) &
  units::drop_units(distance) <= 10000

matches_spatial <- data.frame(
  em_id = em_remaining$Id[keep][keep2],
  mi_id = mi_remaining$Id[em_to_mi[keep][keep2]]
)

#Merge matches
matches <- bind_rows(mi_em_identical, matches_spatial)

mi_common_nodes_em <- mi_nodes[mi_nodes$Id %in% matches$mi_id, ]

em_common_nodes_mi <- em_nodes[em_nodes$Id %in% matches$em_id, ]

#Filter id, names, feature type, normalised edge betweenness
mi_common_nodes_em_df <- mi_common_nodes_em %>%
  select(Id, name, type, betTimeN) %>%
  rename(name_mi = name, betMI = betTimeN) %>%
  st_drop_geometry()

em_common_nodes_mi_df <- em_common_nodes_mi %>%
  select(Id, name, type, betTimeN) %>%
  rename(name_em = name, betEM = betTimeN) %>%
  st_drop_geometry()

#Match and merge
mi_em_matched <- matches %>%
  left_join(mi_common_nodes_em_df, by = c("mi_id" = "Id"))

mi_em_matched <- mi_em_matched %>%
  left_join(
    em_common_nodes_mi_df,
    by = c("em_id" = "Id"),
    suffix = c("_mi", "_em")
  )

mi_em_matched <- mi_em_matched %>%
  mutate(name = paste(name_mi, name_em, sep = "/")) %>%
  mutate(diff = betEM - betMI) %>%
  filter(mi_id !="403") #Omitting spatial match between Qatra and Jisr Yibna (different roads)

#Overview table
mi_em_betweenness_table <- mi_em_matched %>%
  select(name_mi, type_mi, name_em, type_em, betMI, betEM, diff)
write.csv2(mi_em_betweenness_table, file = "outputs/mi_em_betweenness.csv")

#Pivot to prepare data for plotting a dumbbell plot
mi_em_matched <- mi_em_matched  %>%
  pivot_longer(cols = c(betMI, betEM), names_to = "period", values_to = "betweenness")

#Betweenness cities (Middle Islamic)
betCity <- mi_em_matched %>%
  filter(type_mi == "city")

betMICity <- betCity %>%
  filter(period == "betMI")

#Order by descending betweenness
betMIOrd <- betMICity %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betCityOrd <- betCity %>%
  mutate(name = factor(name, levels = rev(betMIOrd)))

betMICityOrd <- betCityOrd %>%
  filter(period == "betMI")

betEMCityOrd <- betCityOrd %>%
  filter(period == "betEM")

betCityMIEMPlot <- ggplot(betCityOrd)+
  geom_segment(data = betMICityOrd,
               aes(x = betweenness, y = name,
                   yend = betEMCityOrd$name, xend = betEMCityOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betMI = "Middle Islamic", betEM = "Early Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_cities_MIEM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness others (Middle Islamic)
betOthers <- mi_em_matched %>%
  filter(type_mi == "fort" | type_mi == "station" | type_mi == "place" | type_mi == "bridge" | type_mi == "sanctuary") %>%
  mutate(label = paste0(name, "-", mi_id))

betMIOthers <- betOthers %>%
  filter(period == "betMI")

#Order by descending betweenness
betMIOrd <- betMIOthers %>%
  arrange(desc(betweenness)) %>%
  pull(label)

betOthersOrd <- betOthers %>%
  mutate(label = factor(label, levels = rev(betMIOrd)))

#Forts
betMIFortsOrd <- betOthersOrd %>%
  filter(period == "betMI" & type_mi == "fort")

betEMFortsOrd <- betOthersOrd %>%
  filter(period == "betEM" & type_em == "fort")

betFortsMIEMPlot <- ggplot(filter(betOthersOrd, type_mi == "fort"))+
  geom_segment(data = betMIFortsOrd,
               aes(x = betweenness, y = label,
                   yend = betEMFortsOrd$label, xend = betEMFortsOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = label, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betMI = "Middle Islamic", betEM = "Early Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_forts_MIEM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Other types
betMIOthersOrd <- betOthersOrd %>%
  filter(period == "betMI", type_mi %in% c("station", "place", "bridge", "sanctuary"))

betEMOthersOrd <- betOthersOrd %>%
  filter(period == "betEM", type_mi %in% c("station", "place", "bridge", "sanctuary"))

betOthersMIEMPlot <- ggplot(filter(betOthersOrd, type_mi == "station" | type_mi == "place" | type_mi == "bridge" | type_mi == "sanctuary"))+
  geom_segment(data = betMIOthersOrd,
               aes(x = betweenness, y = label,
                   yend = betEMOthersOrd$label, xend = betEMOthersOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = label, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betMI = "Middle Islamic", betEM = "Early Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_others_MIEM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Plot new nodes in EM
em_nodes_new <- em_nodes %>%
  filter(!Id %in% em_common_nodes_mi$Id) %>%
  select(Id, name, type, betTimeN) %>%
  rename(betEM = betTimeN) %>%
  st_drop_geometry()

betEMNewOrd <- em_nodes_new %>%
  arrange(desc(betEM)) %>%
  mutate(label = paste0(name, "-", type), label = factor(label, levels = rev(label)))

betEMNewOrdPlot <- ggplot(betEMNewOrd)+
  geom_point(aes(x = betEM, y = label), colour = "#00BFC4", size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness")+
  theme_bw()

ggsave(filename = "betweenness_EM_new.tiff", path = "outputs/", device = "tiff", dpi = 300)

###############################################################
##Early Mamluk to Late Mamluk
lm_nodes <- networks$late_mamluk %>%
  activate(nodes) %>%
  st_as_sf()

#Get nodes that continue (matching name, Id)
em_lm_identical <- em_nodes %>%
  st_drop_geometry() %>%
  select(em_id = Id, name) %>%
  inner_join(
    lm_nodes %>%
      st_drop_geometry() %>%
      select(lm_id = Id, name),
    by = c("em_id" = "lm_id", "name" = "name")
  )

em_lm_identical <- em_lm_identical %>%
  mutate(lm_id = em_id) %>%
  select(lm_id, em_id, name)

em_common_nodes_lm <- em_nodes[em_nodes$Id %in% em_lm_identical$em_id, ]

lm_common_nodes_em <- lm_nodes[lm_nodes$Id %in% em_lm_identical$lm_id, ]

#Filter id, names, feature type, normalised edge betweenness
em_common_nodes_lm_df <- em_common_nodes_lm %>%
  select(Id, name, type, betTimeN) %>%
  rename(name_em = name, betEM = betTimeN) %>%
  st_drop_geometry()

lm_common_nodes_em_df <- lm_common_nodes_em %>%
  select(Id, name, type, betTimeN) %>%
  rename(name_lm = name, betLM = betTimeN) %>%
  st_drop_geometry()

#Match and merge
em_lm_matched <- em_lm_identical %>%
  left_join(em_common_nodes_lm_df, by = c("em_id" = "Id", "name" = "name_em"))

em_lm_matched <- em_lm_matched %>%
  left_join(
    lm_common_nodes_em_df,
    by = c("lm_id" = "Id", "name" = "name_lm"),
    suffix = c("_em", "_lm")
  )

em_lm_matched <- em_lm_matched %>%
  select(em_id, type_em, name, betEM, betLM) %>%
  rename(id = em_id, type = type_em) %>%
  mutate(diff = betLM - betEM)

#Overview table
em_lm_matched_table <- em_lm_matched
write.csv2(em_lm_matched_table, file = "outputs/mi_em_betweenness.csv")

#Pivot to prepare data for plotting a dumbbell plot
em_lm_matched <- em_lm_matched  %>%
  pivot_longer(cols = c(betEM, betLM), names_to = "period", values_to = "betweenness")

#Betweenness cities (Early Mamluk)
betCity <- em_lm_matched %>%
  filter(type == "city")

betEMCity <- betCity %>%
  filter(period == "betEM")

#Order by descending betweenness
betEMOrd <- betEMCity %>%
  arrange(desc(betweenness)) %>%
  pull(name)

betCityOrd <- betCity %>%
  mutate(name = factor(name, levels = rev(betEMOrd)))

betEMCityOrd <- betCityOrd %>%
  filter(period == "betEM")

betLMCityOrd <- betCityOrd %>%
  filter(period == "betLM")

betCityEMLMPlot <- ggplot(betCityOrd)+
  geom_segment(data = betEMCityOrd,
               aes(x = betweenness, y = name,
                   yend = betLMCityOrd$name, xend = betLMCityOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betEM = "Early Mamluk", betLM = "Late Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_cities_EMLM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness others (Early Mamluk)
betOthers <- em_lm_matched %>%
  filter(type == "place" | type == "bridge" | type == "sanctuary") %>%
  mutate(label = paste0(name, "-", id))

betEMOthers <- betOthers %>%
  filter(period == "betEM")

#Order by descending betweenness
betEMOrd <- betEMOthers %>%
  arrange(desc(betweenness)) %>%
  pull(label)

betOthersOrd <- betOthers %>%
  mutate(label = factor(label, levels = rev(betEMOrd)))

betEMOthersOrd <- betOthersOrd %>%
  filter(period == "betEM")

betLMOthersOrd <- betOthersOrd %>%
  filter(period == "betLM")

betOthersEMLMPlot <- ggplot(betOthersOrd)+
  geom_segment(data = betEMOthersOrd,
               aes(x = betweenness, y = label,
                   yend = betLMOthersOrd$label, xend = betLMOthersOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = label, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betEM = "Early Mamluk", betLM = "Late Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_others_EMLM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness stations (Early Mamluk)
betStations <- em_lm_matched %>%
  filter(type == "station") %>%
  mutate(label = paste0(name, "-", id))

betEMStations <- betStations %>%
  filter(period == "betEM")

#Order by descending betweenness
betEMOrd <- betEMStations %>%
  arrange(desc(betweenness)) %>%
  pull(label)

betStationsOrd <- betStations %>%
  mutate(label = factor(label, levels = rev(betEMOrd)))

betEMStationsOrd <- betStationsOrd %>%
  filter(period == "betEM")

betLMStationsOrd <- betStationsOrd %>%
  filter(period == "betLM")

betStationsEMLMPlot <- ggplot(betStationsOrd)+
  geom_segment(data = betEMStationsOrd,
               aes(x = betweenness, y = label,
                   yend = betLMStationsOrd$label, xend = betLMStationsOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = label, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betEM = "Early Mamluk", betLM = "Late Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_stations_EMLM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Betweenness forts (Early Mamluk)
betFort <- em_lm_matched %>%
  filter(type == "fort") %>%
  mutate(label = paste0(name, "-", id))

betEMFort <- betFort %>%
  filter(period == "betEM")

#Order by descending betweenness
betEMOrd <- betEMFort %>%
  arrange(desc(betweenness)) %>%
  pull(label)

betFortOrd <- betFort %>%
  mutate(label = factor(label, levels = rev(betEMOrd)))

betEMFortOrd <- betFortOrd %>%
  filter(period == "betEM")

betLMFortOrd <- betFortOrd %>%
  filter(period == "betLM")

betFortEMLMPlot <- ggplot(betFortOrd)+
  geom_segment(data = betEMFortOrd,
               aes(x = betweenness, y = label,
                   yend = betLMFortOrd$label, xend = betLMFortOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = label, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betEM = "Early Mamluk", betLM = "Late Mamluk"))+
  theme_bw()

ggsave(filename = "betweenness_forts_EMLM.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Plot new nodes in LM
lm_nodes_new <- lm_nodes %>%
  filter(!Id %in% lm_common_nodes_em$Id) %>%
  select(Id, name, type, betTimeN) %>%
  rename(betLM = betTimeN) %>%
  st_drop_geometry()

betLMNewOrd <- lm_nodes_new %>%
  arrange(desc(betLM)) %>%
  mutate(label = paste0(name, "-", type), label = factor(label, levels = rev(label)))

betLMNewOrdPlot <- ggplot(betLMNewOrd)+
  geom_point(aes(x = betLM, y = label), colour = "#00BFC4", size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness")+
  theme_bw()

ggsave(filename = "betweenness_LM_new.tiff", path = "outputs/", device = "tiff", dpi = 300)

###Degree distribution
networks_degree_df <- do.call(
  rbind,
  lapply(names(networks), function(nm) {
    
    g <- networks[[nm]] %>%
      activate(nodes) %>%
      as.igraph()
    
    deg <- degree(g)
    
    data.frame(
      network = nm,
      node_id = seq_along(deg),
      degree = as.numeric(deg)
    )
  })
)

networks_degree_dist <- networks_degree_df %>%
  count(network, degree)%>%
  mutate(network = factor(network, levels = c("roman", "early_islamic", "middle_islamic", "early_mamluk", "late_mamluk")))

networks_degree_table <- networks_degree_dist %>%
  pivot_wider(names_from = degree, values_from = n, values_fill = 0)%>%
  arrange(factor(network, levels = c("roman", "early_islamic", "middle_islamic", "early_mamluk", "late_mamluk")))%>%
  tibble::column_to_rownames("network")
write.csv2(networks_degree_table, "outputs/networks_degree_distribution_table.csv")

##Plot degree distribution
degreeDistributionPlot <- ggplot(networks_degree_dist, aes(x = degree, y = n, fill = network)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      roman = "black",
      early_islamic = "red",
      middle_islamic = "blue",
      early_mamluk = "darkgreen",
      late_mamluk = "purple"
    ),
    labels = c(
      roman = "Roman",
      early_islamic = "Early Islamic",
      middle_islamic = "Middle Islamic",
      early_mamluk = "Early Mamluk",
      late_mamluk = "Late Mamluk"
    ),
    name = "Period"
  ) +
  scale_x_continuous(breaks = scales::breaks_width(1)) +
  labs(
    x = "Degree",
    y = "Number of nodes"
  ) +
  theme_bw()

ggsave(filename = "degree_distribution.tiff", path = "outputs/", device = "tiff", dpi = 300)

####################################################################
##Plot betweenness values comparing Itinerarium Antonini and other Roman roads
roman_edges <- networks$roman %>%
  activate(edges) %>%
  st_as_sf()

IA_bet_plot <- ggplot(roman_edges, aes(x = itAnt, y = betTimeN, fill = itAnt)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "IA",
      "no"  = "Not IA"),
    name = NULL) +
  scale_x_discrete(labels = c(
      "yes" = "IA",
      "no"  = "not IA")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "IA_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

###################################################################
##Plot betweenness values comparing barid and other Early Islamic roads
ei_edges <- networks$early_islamic %>%
  activate(edges) %>%
  st_as_sf()

ei_barid_bet <- ggplot(ei_edges, aes(x = barid, y = betTimeN, fill = barid)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "barid",
      "no"  = "not barid"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "barid",
    "no"  = "not barid")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "ei_barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

##Plot betweenness values comparing barid and other Early Mamluk roads
em_edges <- networks$early_mamluk %>%
  activate(edges) %>%
  st_as_sf()

em_barid_bet <- ggplot(em_edges, aes(x = barid, y = betTimeN, fill = barid)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "barid",
      "no"  = "not barid"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "barid",
    "no"  = "not barid")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "em_barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

##Plot betweenness values comparing barid and other Late Mamluk roads
lm_edges <- networks$late_mamluk %>%
  activate(edges) %>%
  st_as_sf()

lm_barid_bet <- ggplot(lm_edges, aes(x = barid, y = betTimeN, fill = barid)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "barid",
      "no"  = "not barid"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "barid",
    "no"  = "not barid")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "lm_barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

##Plot EI, EM, LM together
barid_edges <- bind_rows(
  ei_edges %>%
    st_drop_geometry() %>%
    mutate(period = "Early Islamic"),
  em_edges %>%
    st_drop_geometry() %>%
    mutate(period = "Early Mamluk"),
  lm_edges %>%
    st_drop_geometry() %>%
    mutate(period = "Late Mamluk")
)

barid_edges$period <- factor(
  barid_edges$period,
  levels = c("Early Islamic", "Early Mamluk", "Late Mamluk")
)

barid_bet_plot <- ggplot(barid_edges, aes(x = barid, y = betTimeN, fill = barid)) +
  geom_boxplot(position = position_dodge(width = 0.8))+
  facet_wrap(~period, nrow = 1) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)
    ),
    labels = c(
      "yes" = "barid",
      "no"  = "not barid"
    ),
    name = NULL
  ) +
  scale_x_discrete(
    labels = c(
      "yes" = "barid",
      "no"  = "not barid"
    )
  ) +
  labs(
    x = NULL,
    y = "Normalised edge betweenness"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(filename = "barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

############################################################
##Plot betweenness values comparing Early Islamic Hajj route and non-Hajj routes
ei_hajj_bet <- ggplot(ei_edges, aes(x = hajj, y = betTimeN, fill = hajj)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "Hajj route",
      "no"  = "not Hajj routes"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "Hajj route",
    "no"  = "not Hajj routes")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "ei_hajj_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

##Plot betweenness values comparing Middle Islamic Hajj route and non-Hajj routes
mi_edges <- networks$middle_islamic %>%
  activate(edges) %>%
  st_as_sf()

mi_hajj_bet <- ggplot(mi_edges, aes(x = hajj, y = betTimeN, fill = hajj)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "Hajj route",
      "no"  = "not Hajj routes"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "Hajj route",
    "no"  = "not Hajj routes")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "mi_hajj_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

##Plot betweenness values comparing Early Mamluk Hajj route and non-Hajj routes
em_hajj_bet <- ggplot(em_edges, aes(x = hajj, y = betTimeN, fill = hajj)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "Hajj route",
      "no"  = "not Hajj routes"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "Hajj route",
    "no"  = "not Hajj routes")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "em_hajj_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

##Plot betweenness values comparing Early Mamluk Hajj route and non-Hajj routes
lm_hajj_bet <- ggplot(lm_edges, aes(x = hajj, y = betTimeN, fill = hajj)) +
  geom_boxplot(
    colour = "black",
    outlier.shape = 16,
    outlier.size = 1.5) +
  scale_fill_manual(
    values = c(
      "yes" = scales::alpha("#FF0000", 0.5),
      "no"  = scales::alpha("#0070FF", 0.5)),
    labels = c(
      "yes" = "Hajj route",
      "no"  = "not Hajj routes"),
    name = NULL) +
  scale_x_discrete(labels = c(
    "yes" = "Hajj route",
    "no"  = "not Hajj routes")) +
  labs(x = NULL, y = "Normalised edge betweenness") +
  theme_bw() +
  theme(legend.position = "none")

ggsave(filename = "lm_hajj_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

################################################################################
###Nodes associated with Itinerarium Antonini and barid comparing betweenness values and weighted node degree
##ItAnt and EI Barid
#Get nodes associated with Itinerarium Antonini
itAnt_edges <- roman_edges %>%
  filter(itAnt == "yes")

nodes_near <- st_is_within_distance(roman_nodes, itAnt_edges, dist = 1)
itAnt_nodes <- roman_nodes[lengths(nodes_near) > 0, ]

#Get nodes associated with EI barid
ei_barid <- ei_edges %>%
  filter(barid == "yes")

nodes_near <- st_is_within_distance(ei_nodes, ei_barid, dist = 1)
ei_barid_nodes <- ei_nodes[lengths(nodes_near) > 0, ]

#Find matching nodes between ItAnt and EI barid
#Get reciprocal nearest neighbours
ei_to_itant <- st_nearest_feature(ei_barid_nodes, itAnt_nodes)
itant_to_ei <- st_nearest_feature(itAnt_nodes, ei_barid_nodes)

ei_idx <- seq_len(nrow(ei_barid_nodes))
keep <- itant_to_ei[ei_to_itant] == ei_idx

#Calculate distances and keep only matches below 10 km
distance <- st_distance(
  ei_barid_nodes[keep, ],
  itAnt_nodes[ei_to_itant[keep], ],
  by_element = TRUE
)

keep2 <- units::drop_units(distance) <= 5000

matches <- data.frame(
  ei_id = ei_barid_nodes$Id[keep][keep2],
  roman_id = itAnt_nodes$id[ei_to_itant[keep][keep2]])

itant_common_nodes_ei <- itAnt_nodes[itAnt_nodes$id %in% matches$roman_id, ]

ei_common_nodes_itant <- ei_barid_nodes[ei_barid_nodes$Id %in% matches$ei_id, ]

#Filter id, names, feature type, normalised edge betweenness
itant_common_nodes_ei_df <- itant_common_nodes_ei %>%
  select(id, name, featureTyp, conn, betTimeN) %>%
  rename(type = featureTyp, betRom = betTimeN) %>%
  st_drop_geometry()

ei_common_nodes_itant_df <- ei_common_nodes_itant %>%
  select(Id, name, type, conn, betTimeN) %>%
  rename(betEI = betTimeN) %>%
  st_drop_geometry()

#Match and merge
itant_ei_matched <- matches %>%
  left_join(itant_common_nodes_ei_df, by = c("roman_id" = "id"))

itant_ei_matched <- itant_ei_matched %>%
  left_join(
    ei_common_nodes_itant_df,
    by = c("ei_id" = "Id"),
    suffix = c("_roman", "_ei")
  )

itant_ei_matched <- itant_ei_matched %>%
  mutate(name = paste(name_roman, name_ei, sep = "/")) %>%
  mutate(diff = betEI - betRom, diff_conn = conn_ei - conn_roman) 

#Overview table
itant_ei_betweenness_table <- itant_ei_matched %>%
  select(name_roman, type_roman, name_ei, type_ei, betRom, betEI, diff, conn_roman, conn_ei, diff_conn)
write.csv2(itant_ei_betweenness_table, file = "outputs/itant_ei_betweenness.csv")

#Pivot to prepare data for plotting a dumbbell plot (betweenness)
itant_ei_bet <- itant_ei_matched  %>%
  pivot_longer(cols = c(betRom, betEI), names_to = "period", values_to = "betweenness")

#Pivot to prepare data for plotting a dumbbell plot (weighted degree)
itant_ei_deg <- itant_ei_matched  %>%
  pivot_longer(cols = c(conn_roman, conn_ei), names_to = "period", values_to = "degree")

#Betweenness
bet_itant <- itant_ei_bet %>%
  filter(period == "betRom")

#Order by descending betweenness
bet_itantOrd <- bet_itant %>%
  arrange(desc(betweenness)) %>%
  pull(name)

bet_itant_ei_ord <- itant_ei_bet %>%
  mutate(name = factor(name, levels = rev(bet_itantOrd)))

betItantOrd <- bet_itant_ei_ord %>%
  filter(period == "betRom")

betEIOrd <- bet_itant_ei_ord %>%
  filter(period == "betEI")

betItAntEIPlot <- ggplot(bet_itant_ei_ord)+
  geom_segment(data = betItantOrd,
               aes(x = betweenness, y = name,
                   yend = betEIOrd$name, xend = betEIOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betRom = "Roman", betEI = "Early Islamic"))+
  theme_bw()

ggsave(filename = "itAnt_ei_barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Weighted degree
deg_itant <- itant_ei_deg %>%
  filter(period == "conn_roman")

#Order by descending betweenness
deg_itantOrd <- deg_itant %>%
  arrange(desc(degree)) %>%
  pull(name)

deg_itant_ei_ord <- itant_ei_deg %>%
  mutate(name = factor(name, levels = rev(deg_itantOrd)))

degItantOrd <- deg_itant_ei_ord %>%
  filter(period == "conn_roman")

degEIOrd <- deg_itant_ei_ord %>%
  filter(period == "conn_ei")

degItAntEIPlot <- ggplot(deg_itant_ei_ord)+
  geom_segment(data = degItantOrd,
               aes(x = degree, y = name,
                   yend = degEIOrd$name, xend = degEIOrd$degree),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = degree, y = name, color = period), size = 1.5, show.legend = TRUE)+
  scale_x_continuous(breaks = scales::breaks_width(1))+
  labs(y = NULL, x = "Weighted degree", color = "Period")+
  scale_color_discrete(labels = c(conn_roman = "Roman", conn_ei = "Early Islamic"))+
  theme_bw()

ggsave(filename = "itAnt_ei_barid_degree.tiff", path = "outputs/", device = "tiff", dpi = 300)

##ItANt and Early Mamluk barid
#Get nodes associated with EI barid
em_barid <- em_edges %>%
  filter(barid == "yes")

nodes_near <- st_is_within_distance(em_nodes, em_barid, dist = 1)
em_barid_nodes <- em_nodes[lengths(nodes_near) > 0, ]

#Find matching nodes between ItAnt and EM barid
#Get reciprocal nearest neighbours
em_to_itant <- st_nearest_feature(em_barid_nodes, itAnt_nodes)
itant_to_em <- st_nearest_feature(itAnt_nodes, em_barid_nodes)

em_idx <- seq_len(nrow(em_barid_nodes))
keep <- itant_to_em[em_to_itant] == em_idx

#Calculate distances and keep only matches below 10 km
distance <- st_distance(
  em_barid_nodes[keep, ],
  itAnt_nodes[em_to_itant[keep], ],
  by_element = TRUE
)

keep2 <- units::drop_units(distance) <= 5000

matches <- data.frame(
  em_id = em_barid_nodes$Id[keep][keep2],
  roman_id = itAnt_nodes$id[em_to_itant[keep][keep2]])

itant_common_nodes_em <- itAnt_nodes[itAnt_nodes$id %in% matches$roman_id, ]

em_common_nodes_itant <- em_barid_nodes[em_barid_nodes$Id %in% matches$em_id, ]

#Filter id, names, feature type, normalised edge betweenness
itant_common_nodes_em_df <- itant_common_nodes_em %>%
  select(id, name, featureTyp, conn, betTimeN) %>%
  rename(type = featureTyp, betRom = betTimeN) %>%
  st_drop_geometry()

em_common_nodes_itant_df <- em_common_nodes_itant %>%
  select(Id, name, type, conn, betTimeN) %>%
  rename(betEM = betTimeN) %>%
  st_drop_geometry()

#Match and merge
itant_em_matched <- matches %>%
  left_join(itant_common_nodes_em_df, by = c("roman_id" = "id"))

itant_em_matched <- itant_em_matched %>%
  left_join(
    em_common_nodes_itant_df,
    by = c("em_id" = "Id"),
    suffix = c("_roman", "_em")
  )

itant_em_matched <- itant_em_matched %>%
  mutate(name = paste(name_roman, name_em, sep = "/")) %>%
  mutate(diff = betEM - betRom, diff_conn = conn_em - conn_roman) 

#Overview table
itant_em_betweenness_table <- itant_em_matched %>%
  select(name_roman, type_roman, name_em, type_em, betRom, betEM, diff, conn_roman, conn_em, diff_conn)
write.csv2(itant_em_betweenness_table, file = "outputs/itant_em_betweenness.csv")

#Pivot to prepare data for plotting a dumbbell plot (betweenness)
itant_em_bet <- itant_em_matched  %>%
  pivot_longer(cols = c(betRom, betEM), names_to = "period", values_to = "betweenness")

#Pivot to prepare data for plotting a dumbbell plot (weighted degree)
itant_em_deg <- itant_em_matched  %>%
  pivot_longer(cols = c(conn_roman, conn_em), names_to = "period", values_to = "degree")

#Betweenness
bet_itant <- itant_em_bet %>%
  filter(period == "betRom")

#Order by descending betweenness
bet_itantOrd <- bet_itant %>%
  arrange(desc(betweenness)) %>%
  pull(name)

bet_itant_em_ord <- itant_em_bet %>%
  mutate(name = factor(name, levels = rev(bet_itantOrd)))

betItantOrd <- bet_itant_em_ord %>%
  filter(period == "betRom")

betEMOrd <- bet_itant_em_ord %>%
  filter(period == "betEM")

betItAntEMPlot <- ggplot(bet_itant_em_ord)+
  geom_segment(data = betItantOrd,
               aes(x = betweenness, y = name,
                   yend = betEMOrd$name, xend = betEMOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betRom = "Roman", betEM = "Early Mamluk"))+
  theme_bw()

ggsave(filename = "itAnt_em_barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Weighted degree
deg_itant <- itant_em_deg %>%
  filter(period == "conn_roman")

#Order by descending betweenness
deg_itantOrd <- deg_itant %>%
  arrange(desc(degree)) %>%
  pull(name)

deg_itant_em_ord <- itant_em_deg %>%
  mutate(name = factor(name, levels = rev(deg_itantOrd)))

degItantOrd <- deg_itant_em_ord %>%
  filter(period == "conn_roman")

degEMOrd <- deg_itant_em_ord %>%
  filter(period == "conn_em")

degItAntEMPlot <- ggplot(deg_itant_em_ord)+
  geom_segment(data = degItantOrd,
               aes(x = degree, y = name,
                   yend = degEMOrd$name, xend = degEMOrd$degree),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = degree, y = name, color = period), size = 1.5, show.legend = TRUE)+
  scale_x_continuous(breaks = scales::breaks_width(1))+
  labs(y = NULL, x = "Weighted degree", color = "Period")+
  scale_color_discrete(labels = c(conn_roman = "Roman", conn_em = "Early Mamluk"))+
  theme_bw()

ggsave(filename = "itAnt_em_barid_degree.tiff", path = "outputs/", device = "tiff", dpi = 300)

##ItANt and Late Mamluk barid
#Get nodes associated with EI barid
lm_barid <- lm_edges %>%
  filter(barid == "yes")

nodes_near <- st_is_within_distance(lm_nodes, lm_barid, dist = 1)
lm_barid_nodes <- lm_nodes[lengths(nodes_near) > 0, ]

#Find matching nodes between ItAnt and EM barid
#Get reciprocal nearest neighbours
lm_to_itant <- st_nearest_feature(lm_barid_nodes, itAnt_nodes)
itant_to_lm <- st_nearest_feature(itAnt_nodes, lm_barid_nodes)

lm_idx <- seq_len(nrow(lm_barid_nodes))
keep <- itant_to_lm[lm_to_itant] == lm_idx

#Calculate distances and keep only matches below 10 km
distance <- st_distance(
  lm_barid_nodes[keep, ],
  itAnt_nodes[lm_to_itant[keep], ],
  by_element = TRUE
)

keep2 <- units::drop_units(distance) <= 5000

matches <- data.frame(
  lm_id = lm_barid_nodes$Id[keep][keep2],
  roman_id = itAnt_nodes$id[lm_to_itant[keep][keep2]])

itant_common_nodes_lm <- itAnt_nodes[itAnt_nodes$id %in% matches$roman_id, ]

lm_common_nodes_itant <- lm_barid_nodes[lm_barid_nodes$Id %in% matches$lm_id, ]

#Filter id, names, feature type, normalised edge betweenness
itant_common_nodes_lm_df <- itant_common_nodes_lm %>%
  select(id, name, featureTyp, conn, betTimeN) %>%
  rename(type = featureTyp, betRom = betTimeN) %>%
  st_drop_geometry()

lm_common_nodes_itant_df <- lm_common_nodes_itant %>%
  select(Id, name, type, conn, betTimeN) %>%
  rename(betLM = betTimeN) %>%
  st_drop_geometry()

#Match and merge
itant_lm_matched <- matches %>%
  left_join(itant_common_nodes_lm_df, by = c("roman_id" = "id"))

itant_lm_matched <- itant_lm_matched %>%
  left_join(
    lm_common_nodes_itant_df,
    by = c("lm_id" = "Id"),
    suffix = c("_roman", "_lm")
  )

itant_lm_matched <- itant_lm_matched %>%
  mutate(name = paste(name_roman, name_lm, sep = "/")) %>%
  mutate(diff = betLM - betRom, diff_conn = conn_lm - conn_roman) 

#Overview table
itant_lm_betweenness_table <- itant_lm_matched %>%
  select(name_roman, type_roman, name_lm, type_lm, betRom, betLM, diff, conn_roman, conn_lm, diff_conn)
write.csv2(itant_lm_betweenness_table, file = "outputs/itant_lm_betweenness.csv")

#Pivot to prepare data for plotting a dumbbell plot (betweenness)
itant_lm_bet <- itant_lm_matched  %>%
  pivot_longer(cols = c(betRom, betLM), names_to = "period", values_to = "betweenness")

#Pivot to prepare data for plotting a dumbbell plot (weighted degree)
itant_lm_deg <- itant_lm_matched  %>%
  pivot_longer(cols = c(conn_roman, conn_lm), names_to = "period", values_to = "degree")

#Betweenness
bet_itant <- itant_lm_bet %>%
  filter(period == "betRom")

#Order by descending betweenness
bet_itantOrd <- bet_itant %>%
  arrange(desc(betweenness)) %>%
  pull(name)

bet_itant_lm_ord <- itant_lm_bet %>%
  mutate(name = factor(name, levels = rev(bet_itantOrd)))

betItantOrd <- bet_itant_lm_ord %>%
  filter(period == "betRom")

betLMOrd <- bet_itant_lm_ord %>%
  filter(period == "betLM")

betItAntLMPlot <- ggplot(bet_itant_lm_ord)+
  geom_segment(data = betItantOrd,
               aes(x = betweenness, y = name,
                   yend = betLMOrd$name, xend = betLMOrd$betweenness),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = betweenness, y = name, color = period), size = 1.5, show.legend = TRUE)+
  labs(y = NULL, x = "Normalised betweenness", color = "Period")+
  scale_color_discrete(labels = c(betRom = "Roman", betLM = "Late Mamluk"))+
  theme_bw()

ggsave(filename = "itAnt_lm_barid_betweenness.tiff", path = "outputs/", device = "tiff", dpi = 300)

#Weighted degree
deg_itant <- itant_lm_deg %>%
  filter(period == "conn_roman")

#Order by descending betweenness
deg_itantOrd <- deg_itant %>%
  arrange(desc(degree)) %>%
  pull(name)

deg_itant_lm_ord <- itant_lm_deg %>%
  mutate(name = factor(name, levels = rev(deg_itantOrd)))

degItantOrd <- deg_itant_lm_ord %>%
  filter(period == "conn_roman")

degLMOrd <- deg_itant_lm_ord %>%
  filter(period == "conn_lm")

degItAntLMPlot <- ggplot(deg_itant_lm_ord)+
  geom_segment(data = degItantOrd,
               aes(x = degree, y = name,
                   yend = degLMOrd$name, xend = degLMOrd$degree),
               color = "darkgrey",
               linewidth = 1,
               alpha = .5)+
  geom_point(aes(x = degree, y = name, color = period), size = 1.5, show.legend = TRUE)+
  scale_x_continuous(breaks = scales::breaks_width(1))+
  labs(y = NULL, x = "Weighted degree", color = "Period")+
  scale_color_discrete(labels = c(conn_roman = "Roman", conn_lm = "Late Mamluk"))+
  theme_bw()

ggsave(filename = "itAnt_lm_barid_degree.tiff", path = "outputs/", device = "tiff", dpi = 300)

#########################################################################################
###Calculate distance to the nearest node from Itinerarium Antonini and EI, EM, LM barid nodes
#EI
nearest_idx <- st_nearest_feature(ei_barid_nodes, itAnt_nodes)

nearest_dist <- st_distance(ei_barid_nodes, itAnt_nodes[nearest_idx, ], by_element = TRUE)

ei_barid_itant_nearest <- ei_barid_nodes
ei_barid_itant_nearest$near_id <- itAnt_nodes$id[nearest_idx]
ei_barid_itant_nearest$near_dist <- units::drop_units(nearest_dist)

#EM
nearest_idx <- st_nearest_feature(em_barid_nodes, itAnt_nodes)

nearest_dist <- st_distance(em_barid_nodes, itAnt_nodes[nearest_idx, ], by_element = TRUE)

em_barid_itant_nearest <- em_barid_nodes
em_barid_itant_nearest$near_id <- itAnt_nodes$id[nearest_idx]
em_barid_itant_nearest$near_dist <- units::drop_units(nearest_dist)

#LM
nearest_idx <- st_nearest_feature(lm_barid_nodes, itAnt_nodes)

nearest_dist <- st_distance(lm_barid_nodes, itAnt_nodes[nearest_idx, ], by_element = TRUE)

lm_barid_itant_nearest <- lm_barid_nodes
lm_barid_itant_nearest$near_id <- itAnt_nodes$id[nearest_idx]
lm_barid_itant_nearest$near_dist <- units::drop_units(nearest_dist)

##Plot histograms
#EI
ei_barid_itant_nearest <- ei_barid_itant_nearest %>%
  mutate(
    dist_class = cut(
      near_dist,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", ">20 km"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

dist_counts <- ei_barid_itant_nearest %>%
  count(dist_class)

ei_itant_nearest_plot <- ggplot(dist_counts, aes(x = dist_class, y = n)) +
  geom_col(fill = "grey70", colour = "black") +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4) +
  labs(x = "Distance to the nearest Itinerarium Antonini site", y = "Number of barid stations") +
  theme_bw()

ggsave(filename = "itAnt_ei_barid_distance.tiff", path = "outputs/", device = "tiff", dpi = 300)

#EM
em_barid_itant_nearest <- em_barid_itant_nearest %>%
  mutate(
    dist_class = cut(
      near_dist,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", ">20 km"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

dist_counts <- em_barid_itant_nearest %>%
  count(dist_class)

em_itant_nearest_plot <- ggplot(dist_counts, aes(x = dist_class, y = n)) +
  geom_col(fill = "grey70", colour = "black") +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4) +
  labs(x = "Distance to the nearest Itinerarium Antonini site", y = "Number of barid stations") +
  theme_bw()

ggsave(filename = "itAnt_em_barid_distance.tiff", path = "outputs/", device = "tiff", dpi = 300)

#LM
lm_barid_itant_nearest <- lm_barid_itant_nearest %>%
  mutate(
    dist_class = cut(
      near_dist,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", ">20 km"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

dist_counts <- lm_barid_itant_nearest %>%
  count(dist_class)

lm_itant_nearest_plot <- ggplot(dist_counts, aes(x = dist_class, y = n)) +
  geom_col(fill = "grey70", colour = "black") +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4) +
  labs(x = "Distance to the nearest Itinerarium Antonini site", y = "Number of barid stations") +
  theme_bw()

ggsave(filename = "itAnt_lm_barid_distance.tiff", path = "outputs/", device = "tiff", dpi = 300)

#########################################################################################
###Calculate distance to the nearest Roman node and EI, EM, LM barid nodes
#EI
nearest_idx <- st_nearest_feature(ei_barid_nodes, roman_nodes)

nearest_dist <- st_distance(ei_barid_nodes, roman_nodes[nearest_idx, ], by_element = TRUE)

ei_barid_roman_nearest <- ei_barid_nodes
ei_barid_roman_nearest$near_id <- roman_nodes$id[nearest_idx]
ei_barid_roman_nearest$near_dist <- units::drop_units(nearest_dist)

#EM
nearest_idx <- st_nearest_feature(em_barid_nodes, roman_nodes)

nearest_dist <- st_distance(em_barid_nodes, roman_nodes[nearest_idx, ], by_element = TRUE)

em_barid_roman_nearest <- em_barid_nodes
em_barid_roman_nearest$near_id <- roman_nodes$id[nearest_idx]
em_barid_roman_nearest$near_dist <- units::drop_units(nearest_dist)

#LM
nearest_idx <- st_nearest_feature(lm_barid_nodes, roman_nodes)

nearest_dist <- st_distance(lm_barid_nodes, roman_nodes[nearest_idx, ], by_element = TRUE)

lm_barid_roman_nearest <- lm_barid_nodes
lm_barid_roman_nearest$near_id <- roman_nodes$id[nearest_idx]
lm_barid_roman_nearest$near_dist <- units::drop_units(nearest_dist)

##Plot histograms
#EI
ei_barid_roman_nearest <- ei_barid_roman_nearest %>%
  mutate(
    dist_class = cut(
      near_dist,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", ">20 km"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

dist_counts <- ei_barid_roman_nearest %>%
  count(dist_class)

ei_roman_nearest_plot <- ggplot(dist_counts, aes(x = dist_class, y = n)) +
  geom_col(fill = "grey70", colour = "black") +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4) +
  labs(x = "Distance to the nearest Roman site", y = "Number of barid stations") +
  theme_bw()

ggsave(filename = "roman_ei_barid_distance.tiff", path = "outputs/", device = "tiff", dpi = 300)

#EM
em_barid_roman_nearest <- em_barid_roman_nearest %>%
  mutate(
    dist_class = cut(
      near_dist,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", ">20 km"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

dist_counts <- em_barid_roman_nearest %>%
  count(dist_class)

em_roman_nearest_plot <- ggplot(dist_counts, aes(x = dist_class, y = n)) +
  geom_col(fill = "grey70", colour = "black") +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4) +
  labs(x = "Distance to the nearest Roman site", y = "Number of barid stations") +
  theme_bw()

ggsave(filename = "roman_em_barid_distance.tiff", path = "outputs/", device = "tiff", dpi = 300)

#LM
lm_barid_roman_nearest <- lm_barid_roman_nearest %>%
  mutate(
    dist_class = cut(
      near_dist,
      breaks = c(0, 5000, 10000, 15000, 20000, Inf),
      labels = c("0–5 km", "5–10 km", "10–15 km", "15–20 km", ">20 km"),
      include.lowest = TRUE,
      right = TRUE
    )
  )

dist_counts <- lm_barid_roman_nearest %>%
  count(dist_class)

lm_roman_nearest_plot <- ggplot(dist_counts, aes(x = dist_class, y = n)) +
  geom_col(fill = "grey70", colour = "black") +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4) +
  labs(x = "Distance to the nearest Roman site", y = "Number of barid stations") +
  theme_bw()

ggsave(filename = "roman_lm_barid_distance.tiff", path = "outputs/", device = "tiff", dpi = 300)

# WAf/CAf covid survey
library(googledrive)
library(googlesheets4)
googledrive::drive_auth()
sheets_auth(token = drive_token())
data_WAf <- read_sheet("https://docs.google.com/spreadsheets/d/1KExrokkD2CztTA0ByFf1v6xpIH5SWxr9S92XTAmCtJA", sheet = "data_WAf")
data_CAf <- read_sheet("https://docs.google.com/spreadsheets/d/1KExrokkD2CztTA0ByFf1v6xpIH5SWxr9S92XTAmCtJA", sheet = "data_CAf")


library(tidyverse)
bar_plot_fn <- function(data, item, title, ...){
  p <- ggplot(data %>% filter(Fields %in% item)) + 
    geom_bar(aes(x = Category, ...)) + 
    scale_x_discrete(limits = data %>% 
                       filter(Fields %in% item) %>%
                       count(Category) %>%
                       arrange(n) %>%
                       select(Category) %>%
                       unlist()
    ) +
    scale_fill_viridis_d() +
    coord_flip() + 
    labs(title = title)
  return(p)
}
p_challenge_WAf <- bar_plot_fn(data_WAf, "Challenge", "Challenges in West Africa")
p_challenge_WAf

p_action_WAf <- bar_plot_fn(data_WAf, "Action", "Actions in West Africa")
p_action_WAf

p_challenge_CAf <- bar_plot_fn(data_CAf, "Challenge", "Challenges in Central Africa")
p_challenge_CAf

p_action_CAf <- bar_plot_fn(data_CAf, "Action", "Actions in Central Africa")
p_action_CAf

p_challenge_WAf_CAf <- bar_plot_fn(
  data = bind_rows(data_WAf,data_CAf), 
  item = "Challenge", 
  title = "Challenges in West and Central Africa", 
  fill = Pays)
p_challenge_WAf_CAf
ggsave(filename = "./fig/p_challenge_WAf_CAf.png",p_challenge_WAf_CAf,
       width = 15, height = 10, units = "cm")

p_action_WAf_CAf <- bar_plot_fn(
  data = bind_rows(data_WAf,data_CAf), 
  item = "Action", 
  title = "Actions in West and Central Africa",
  fill = Pays)
p_action_WAf_CAf
ggsave(filename = "./fig/p_action_WAf_CAf.png",p_action_WAf_CAf,
       width = 15, height = 10, units = "cm")

# link actions to challenges

# make copy of data and add \n where challenge/action >20 char
data_CAf_copy <- data_CAf
data_WAf_copy <- data_WAf

devtools::install_github("fbreitwieser/sankeyD3")
library(sankeyD3)

biopama_cols = c("#90c14f", "#41ad53", "#61ab79", "#71a6a1","#91b090", 
                 "#a7d1e5","#cf785e", "#a25b28", "#70b6d1", "#679b95",
                 "#36aa49", "#90c04e", "#e65025", "#70b6d1","#cc775d" )

WAf_nodes <- data_WAf %>% 
  select(Category) %>%
  group_by(Category) %>%
  count() %>%
  tibble::rowid_to_column("ID") %>%
  select(Category, ID, n) %>%
  add_column(col = biopama_cols[c(1,2,3,3,2,3,4,5,4,6,7,8,6,5,8,6,7)]) %>%
  as.data.frame()

CAf_nodes <- data_CAf %>% 
  filter(Fields %in% c("Challenge", "Action")) %>%
  select(Category) %>%
  group_by(Category) %>%
  count() %>%
  tibble::rowid_to_column("ID") %>%
  select(Category, ID, n) %>%
  add_column(col = biopama_cols[c(1,2,3,3,2,3,4,5,4,6,7,8,6,5,8,6,7, 9, 10)]) %>%
  as.data.frame()

WAf_CAf_nodes <- bind_rows(data_WAf, data_CAf) %>% 
  filter(Fields %in% c("Challenge", "Action")) %>%
  select(Category) %>%
  group_by(Category) %>%
  count() %>%
  tibble::rowid_to_column("ID") %>%
  rename(value = n) %>%
  select(Category, ID, value) %>%
  add_column(col = biopama_cols[c(9,1,10,2,9,3,3,2,3,4,5,4,6,7,11,9,8,6,6,5,8,6,12,7)]) %>%
  mutate(weight = 0.5) %>%
  as.data.frame()

links_WAf <- data_WAf %>%
  mutate(id = paste0(ISO3, IDR, IDC)) %>%
  pivot_wider(id_cols = id, names_from = Fields, values_from = Category, values_fn = list(Category = list)) %>%
  unnest(cols = c(Challenge, Response, Action)) %>%
  mutate(Challenge_id = match(Challenge, WAf_nodes$Category)-1) %>%
  mutate(Action_id = match(Action, WAf_nodes$Category)-1) %>%
  group_by(Challenge) %>%
  mutate(value = n()) %>%
  ungroup()

links_CAf <- data_CAf %>%
  mutate(id = paste0(ISO3, IDR, IDC)) %>%
  filter(Fields %in% c("Challenge", "Action")) %>%
  pivot_wider(id_cols = id, names_from = Fields, values_from = Category, values_fn = list(Category = list)) %>%
  unnest(cols = c(Challenge, Action)) %>%
  mutate(Challenge_id = match(Challenge, CAf_nodes$Category)-1) %>%
  mutate(Action_id = match(Action, CAf_nodes$Category)-1) %>%
  group_by(Challenge) %>%
  mutate(value = n()) %>%
  ungroup()

links <- bind_rows(data_WAf, data_CAf) %>%
  mutate(id = paste0(ISO3, IDR, IDC)) %>%
  filter(Fields %in% c("Challenge", "Action")) %>%
  pivot_wider(id_cols = id, names_from = Fields, values_from = Category, values_fn = list(Category = list)) %>%
  unnest(cols = c(Challenge, Action)) %>%
  mutate(Challenge_id = match(Challenge, WAf_CAf_nodes$Category)-1) %>%
  mutate(Action_id = match(Action, WAf_CAf_nodes$Category)-1) %>%
  group_by(Challenge) %>%
  mutate(value = n()) %>%
  ungroup() %>%
  drop_na()

sankeyNetwork(Links = as.data.frame(WAf_links), Nodes = as.data.frame(WAf_nodes), Source = "Challenge_id",
              Target = "Action_id", NodeID = "Category", NodeColor = "col", fontSize = 12, numberFormat = "d")
sankeyNetwork(Links = as.data.frame(links), Nodes = WAf_CAf_nodes, Source = "Challenge_id",
              Target = "Action_id", NodeID = "Category", NodeColor = "col", fontSize = 18, numberFormat = "d",
              #title = "Challenges and Actions linked to Covid-19 in protected areas of Central West Africa",
              showNodeValues = FALSE, nodeWidth = 40,
              #yOrderComparator = "function(nodeA, nodeB) {if (nodeA.name < nodeB.name) return -1; if (nodeA.name > nodeB.name) return 1; return 0;}",
              dragY = TRUE
              )
# other package?
library(sankey)
WAf_edges <- WAf_links %>% 
  select(Challenge, Action) %>%
  mutate(colorstyle = "gradient") %>%
  as.data.frame()
sankey(make_sankey(nodes = WAf_nodes, edges = WAf_edges), title = "Challenges and actions linked to protected areas management in West Africa")

WAf_CAf_edges <- links %>%
  select(Challenge, Action) %>%
  mutate(colorstyle = "gradient") %>%
  mutate(weight = 0.5) %>%
  mutate(lty = 0) %>%
  as.data.frame()

png("./fig/WAf_CAf_sankey.png", 800, 700, res = 150)
dev.new()
sankey(make_sankey(nodes = WAf_CAf_nodes, edges = WAf_CAf_edges), title = "Challenges and actions linked to protected areas management in West and Central Africa")
dev.off()
# other package
library(ggalluvial)
ggplot(links,
       aes(y = value,
           axis1 = Challenge, axis2 = Action)) +
  geom_alluvium(aes(fill = Challenge),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/12, reverse = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)),
            reverse = FALSE) +
  scale_x_continuous(breaks = 1:2, labels = c("Challenge", "Action"))


tmp <- data_WAf %>% 
  filter(Fields %in% c("Challenge", "Action")) %>%
  mutate(id = paste0(ISO3, IDR, IDC)) %>%
  select(id,Fields,Category) %>%
  mutate(freq = 1)
tmp <- to
           
ggplot(tmp,
       aes(x = Fields, stratum = Category, alluvium = id,
           y = 1,
           fill = Category, label = Category)) +
  scale_x_discrete(expand = c(.1, .1)) +
  geom_flow() +
  geom_stratum(alpha = .5) +
  geom_text(stat = "stratum", size = 3) +
  theme(legend.position = "none") +
  ggtitle("Challenges and priorities for action in West and Central Africa")

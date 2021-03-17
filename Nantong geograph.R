# --------------------------------------------------------------------------------------------- #
# ------------------------------------- PAPMed project map ------------------------------------ #
# programmer: Zhengting He
# date: December 5-8, 2020
# --------------------------------------------------------------------------------------------- #

# Map is produced by R package ¡°ggplot2¡± (Hadley Wickham et al., version: 3.3.2)
# Original dataset is provided by diva-gis.org
# Location is provided by Baidu Map Open Platform
# https://www.zhihu.com/question/49899908

require(dplyr)
require(ggplot2)
require(maptools)
require(raster)

setwd("F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/ChinaProvinceCityJsonData-master/CHN_adm/adm3")
shape_data <- readShapeSpatial('CHN_adm3.shp')
shape_data_taiwan <- readShapeSpatial('TWN_adm2.shp')
shape_data_china <- union(shape_data, shape_data_taiwan)

# map of China
pdf(file = "F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/China geograph.pdf", width = 18.05, height = 12.25)
ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = id), data = shape_data_china) +
  annotate(geom = "point", x = 120.866798, y = 32.074365, colour = "red", size = 4) +
  ggtitle("Map of China") +
  theme_classic() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = -4)) +
  guides(fill = FALSE)
dev.off()

# map of nantong
id_nantong <- shape_data$NAME_2 %>% 
  `==`('Nantong') %>% 
  which() %>% 
  `-`(1) %>% 
  as.character()

shape_nantong <- shape_data %>% 
  fortify() %>% 
  filter(id %in% id_nantong)

pdf(file = "F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/Nantong geograph.pdf", width = 18.05, height = 12.25)
ggplot() + 
  geom_polygon(aes(x = long, y = lat, fill = id), data = shape_nantong) +
  scale_fill_manual(values = c("#1cc7d0", "#b84592", "#8e43e7", "#ff4f81", "#2dde98", "#ffc168", "#ff6c5f", "#ff6c5f")) +
  geom_text(aes(label = "Tongzhou", x = 121.07331, y = 32.06588), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Chongchuan", x = 120.85496, y = 31.96233), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Gangzha", x = 120.81650, y = 32.03329), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Haimen", x = 121.31296, y = 31.97289), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Qidong", x = 121.68100, y = 31.87965), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Rudong", x = 121.07199, y = 32.36618), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Rugao", x = 120.58965, y = 32.26743), check_overlap = TRUE, size = 7, fontface = "bold") +
  geom_text(aes(label = "Hai'an", x = 120.46460, y = 32.52962), check_overlap = TRUE, size = 7, fontface = "bold") +
  ggtitle("Map of Nantong city") +
  theme_classic() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = -4)) +
  guides(fill = FALSE)
dev.off()

# map of study villages: group by location
location <- read.csv("F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/pingchao and liuqiao2.csv")
shape_tongzhou <- shape_data %>% 
  fortify() %>% 
  filter(id %in% 1140)

pdf(file = "F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/site geograph1.pdf", width = 18.05, height = 12.25)
ggplot() + 
  geom_polygon(aes(x = long, y = lat), data = shape_tongzhou, fill = "#f0f0f0") +
  geom_point(data = location, aes(x = long, y = lat, colour = group), size = 2.5) +
  scale_color_manual(values = c("#4257b2", "#00a9a9")) +
  geom_text(aes(label = "Pingchao", x = 120.75808, y = 32.14120), check_overlap = TRUE, size = 5, fontface = "bold") +
  geom_text(aes(label = "Liuqiao", x = 120.86361, y = 32.19931), check_overlap = TRUE, size = 5, fontface = "bold") +
  geom_text(aes(label = "Tongzhou", x = 121.07331, y = 32.06588), check_overlap = TRUE, size = 8, fontface = "bold") +
  ggtitle("Geographic location of study villages") +
  theme_classic() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        legend.title = element_blank(), legend.text = element_text(size = 15, face = "bold"), legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = -4))
dev.off()

# map of study villages: group by randomization
pdf(file = "F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/site geograph3.pdf", width = 18.05, height = 12.25)
ggplot() + 
  geom_polygon(aes(x = long, y = lat), data = shape_tongzhou, fill = "#f4f5f6") +
  geom_point(data = location, aes(x = long, y = lat, colour = random), size = 2.5) +
  scale_color_manual(values = c("#606c76", "#9b4dca")) +
  geom_text(aes(label = "Pingchao", x = 120.75808, y = 32.14120), check_overlap = TRUE, size = 5, fontface = "bold") +
  geom_text(aes(label = "Liuqiao", x = 120.86361, y = 32.19931), check_overlap = TRUE, size = 5, fontface = "bold") +
  geom_text(aes(label = "Tongzhou", x = 121.07331, y = 32.06588), check_overlap = TRUE, size = 8, fontface = "bold") +
  ggtitle("Geographic location of study villages") +
  theme_classic() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        legend.title = element_blank(), legend.text = element_text(size = 15, face = "bold"), legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = -4))
dev.off()

# part of the map of study villages: group by location
shape_tongzhou <- subset(shape_tongzhou, long < 120.95 & lat > 32.05)
pdf(file = "F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/site geograph2.pdf", width = 18.05, height = 12.25)
ggplot() + 
  geom_polygon(aes(x = long, y = lat), data = shape_tongzhou, fill = "#f0f0f0") +
  geom_point(data = location, aes(x = long, y = lat, colour = group), size = 1.5) +
  geom_text(data = location, aes(label = village, x = long, y = lat + 0.002, colour = group), size = 4, 
            fontface = "bold") +
  scale_color_manual(values = c("#4257b2", "#00a9a9")) +
  geom_point(aes(x = 120.846943, y = 32.160863), size = 2) +
  geom_text(aes(label = "The 7th People's Hospital of Tongzhou", x = 120.846943, y = 32.163863), size = 5, fontface = "bold") +
  geom_point(aes(x = 120.76025, y = 32.105976), size = 2) +
  geom_text(aes(label = "The 8th People's Hospital of Tongzhou", x = 120.76025, y = 32.102976), size = 5, fontface = "bold") +
  geom_text(aes(label = "Pingchao", x = 120.75808, y = 32.14120), check_overlap = TRUE, size = 6, fontface = "bold") +
  geom_text(aes(label = "Liuqiao", x = 120.86361, y = 32.19431), check_overlap = TRUE, size = 6, fontface = "bold") +
  ggtitle("Geographic location of study villages") +
  theme_classic() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        legend.title = element_blank(), legend.text = element_text(size = 15, face = "bold"), legend.position = "bottom", 
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = -4))
dev.off()

# part of the map of study villages: group by randomization
location <- subset(location, random != "drop")
pdf(file = "F:/Box Sync/Duke Kunshan University Intern/2 CCL/admin/network meeting/geograph/site geograph4.pdf", width = 18.05, height = 12.25)
ggplot() + 
  geom_polygon(aes(x = long, y = lat), data = shape_tongzhou, fill = "#f4f5f6") +
  geom_point(data = location, aes(x = long, y = lat, colour = random), size = 1.5) +
  geom_text(data = location, aes(label = village, x = long, y = lat + 0.002, colour = random), size = 4, 
            fontface = "bold") +
  scale_color_manual(values = c("#606c76", "#9b4dca")) +
  geom_point(aes(x = 120.846943, y = 32.160863), size = 2) +
  geom_text(aes(label = "The 7th People's Hospital of Tongzhou", x = 120.846943, y = 32.163863), size = 4, fontface = "bold") +
  geom_point(aes(x = 120.76025, y = 32.105976), size = 2) +
  geom_text(aes(label = "The 8th People's Hospital of Tongzhou", x = 120.76025, y = 32.102976), size = 4, fontface = "bold") +
  geom_text(aes(label = "Pingchao", x = 120.75808, y = 32.14120), check_overlap = TRUE, size = 6, fontface = "bold") +
  geom_text(aes(label = "Liuqiao", x = 120.86361, y = 32.19431), check_overlap = TRUE, size = 6, fontface = "bold") +
  ggtitle("Randomization result of study villages") +
  theme_classic() +
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(),
        legend.title = element_blank(), legend.text = element_text(size = 15, face = "bold"), legend.position = "bottom", 
        plot.title = element_text(face = "bold", size = 30, hjust = 0.5, vjust = -4))
dev.off()
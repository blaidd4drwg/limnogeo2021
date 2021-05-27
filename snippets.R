library(tidyverse)
library(corrplot)
library(plotly)

xrf_pollution_limno21 <- xrf.df %>%
  select(CoreID = UID, Depth = CoreDepth, Element = elements, Counts = area)

save(icpoes_pollution_limno21, file = "icpoesdata_pollution_limno21.RData")

icpoes_pollution_limno21 <- icpoes.df %>%
  mutate(CoreID = as.factor(UID), Depth = depth * 10) %>%
  select(CoreID, Depth, Concentration = concentration, Element = element)



### Tutorial code snippets with Lake Zug data

# Steps for data analysis:
# 1. Importing data, tidying and cleaning data, removing duplicates, outliers, deal with missing values
# 2. Explore your data visually! At this stage you don't need to worry about labels, nice colours or publishing. Have a look at cores that look visually interesting or where you have prior knowledge about what you expect to find
# 3. If necessary, do some statistical tests
# 4. Think about what your data tells you and how you could visualise this the best way. Get inspired by scientific articles, blog, google, books, journals!
# 5. Make plots that you want to use for publication. Think how you can pack all the necessary information for your story into a plot. It is possible that you will have to do some post-processing after using your visualisation program. E.g. you might want to plot your data next to a linescan or to your sediment core description.
# 6. If you're using code make sure that your scripts run without errors, ideally pack your analysis in a self-contained project, make comments and make sure that you can still reconstruct your work after a while! If you use excel for your analysis, be generous with comments or use a spreadsheet where you detail data handling steps or important information.


xrfwide <- xrfdata_lakezug %>%
  select(CoreID, Depth, Element, cps) %>%
  pivot_wider(names_from = Element, values_from = cps)

xrfwide_zug18_7 <- cor(xrfwide %>% filter(CoreID == "ZUG18-7") %>% select(-CoreID, -Depth))
corrplot(xrfwide_zug18_7, type = "upper", order = "hclust", tl.col="black", mar=c(0,0,1,0), tl.srt=45, title = "Correlation plot for XRF data of ZUG18-7")

xrfwide_zug18_8 <- cor(xrfwide %>% filter(CoreID == "ZUG18-8") %>% select(-CoreID, -Depth))
corrplot(xrfwide_zug18_8, type = "upper", order = "hclust", tl.col="black", mar=c(0,0,1,0), tl.srt=45, title = "Correlation plot for XRF data of ZUG18-8")

xrfwide_zug18_9 <- cor(xrfwide %>% filter(CoreID == "ZUG18-9") %>% select(-CoreID, -Depth))
corrplot(xrfwide_zug18_9, type = "upper", order = "hclust", tl.col="black", mar=c(0,0,1,0), tl.srt=45, title = "Correlation plot for XRF data of ZUG18-9")

plotobj <- ggplot(xrfdata_lakezug, aes(x = Depth, y = cps, colour = Element)) + geom_line() + coord_flip() + scale_x_reverse() + facet_wrap(Element ~ CoreID, scales = "free")

ggplotly(plotobj)

library(tidyverse)
library(pracma)
library(slider)

test <- xrfdata_lakezug %>%
  select(CoreID, Depth, Element, cps) %>%
  group_by(CoreID, Element) %>%
  mutate(WindowGroup = seq(1, nrow(.), by = 1))

ggplot(xrfdata_lakezug %>% filter(Element %in% c("Pb", "Zn")), aes(x = Depth, y = cps, colour = Element)) + geom_line(alpha = 0.2) + coord_flip() + scale_x_reverse() + facet_wrap( ~ CoreID, scales = "free") + geom_smooth(method = "gam")




library(leaflet)

wms_swisstopo <- "https://wms.geo.admin.ch/"
coords_richterswil <- read_csv("coordinates_richterswil.csv") %>%
  mutate(Date = ymd(Date)) %>%
  filter(str_detect(Sample, "AW|ZH"))

leaflet(coords_richterswil) %>%
  setView(lng = 8.67868, lat = 47.24122, zoom = 10) %>%
  addWMSTiles(
    wms_swisstopo,
    layers = c("ch.swisstopo.pixelkarte-farbe", "ch.swisstopo.swissbathy3d-reliefschattierung"),
    options = WMSTileOptions(format = "image/jpeg")
  ) %>%
  addCircleMarkers(lng = ~Long, lat = ~lat, label = ~Sample)


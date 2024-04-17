biomass_scr_graph <- read.csv("C:/Users/lewiske/Documents/CAFE/projects/restoration/kristin/CS_Estimates/biomass_scr_graph.csv")

####Adult brook trout biomass

ggplot(data=biomass_scr_graph, aes(x=factor(year), y=adut_bt, fill=type))+
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  xlab("Year") + ylab("Biomass (g/100 sq meters)") +
  theme(axis.title.y = element_text(size=18), axis.title.y=element_text(size=18)) +
  scale_fill_grey() +
  theme_bw()

#######YOY Brook Trout Biomass

ggplot(data=biomass_scr_graph, aes(x=factor(year), y=yoy_bt, fill=type))+
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  xlab("Year") + ylab("Biomass (g/100 sq meters)") +
  theme(axis.title.y = element_text(size=18), axis.title.y=element_text(size=18)) +
  scale_fill_grey() +
  theme_bw()

######Adult atlantic salmon
ggplot(data=biomass_scr_graph, aes(x=factor(year), y=adult_as, fill=type))+
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  xlab("Year") + ylab("Biomass (g/100 sq meters)") +
  theme(axis.title.y = element_text(size=18), axis.title.y=element_text(size=18)) +
  scale_fill_grey() +
  theme_bw()

#####Young of Year AS
ggplot(data=biomass_scr_graph, aes(x=factor(year), y=yoy_as, fill=type))+
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  xlab("Year") + ylab("Biomass (g/100 sq meters)") +
  theme(axis.title.y = element_text(size=18), axis.title.y=element_text(size=18)) +
  scale_fill_grey() +
  theme_bw()


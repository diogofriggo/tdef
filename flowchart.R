library(tidyverse)
library(DiagrammeR)
library(DiagrammeRsvg)
library(svglite)
library(rsvg)
mermaid("
graph TB
A(<center>1 Faça um gráfico dos dados<br/>Identifique comportamentos atípicos<br/>Identifique padrões</center>)-->D{<center>2. A variância parece ser<br/>constante ao longo da série?</center>}
D-->E(<center>2.1 Aplique uma transformação<br/>Box-Cox para estabilizar a variância<br/>até que a série aparente ser estacionária</center>)
E-->F
D-->F(<center>3. Analise os gráficos ACF e PACF<br/>para identificar prováveis modelos</center>)
F-->G(<center>3.1 Teste os modelos comparando<br/>seus valores de AIC</center>)
G-->H(<center>3.2 Verifique os resíduos<br/>por ACF e por Ljung-Box</center>)
H-->I{<center>4. Os resíduos se aproximam<br/>de ruído branco?</center>}
I-->|Sim| J(5.Faça previsões)
I--> |Não| F
") -> m
m
attributes(m)
attr(m, 'package')
semplot<-grViz("
graph TB
A(<center>1 Faça um gráfico dos dados<br/>Identifique comportamentos atípicos<br/>Identifique padrões</center>)-->D{<center>2. A variância parece ser<br/>constante ao longo da série?</center>}
D-->E(<center>2.1 Aplique uma transformação<br/>Box-Cox para estabilizar a variância<br/>até que a série aparente ser estacionária</center>)
E-->F
D-->F(<center>3. Analise os gráficos ACF e PACF<br/>para identificar prováveis modelos</center>)
F-->G(<center>3.1 Teste os modelos comparando<br/>seus valores de AIC</center>)
G-->H(<center>3.2 Verifique os resíduos<br/>por ACF e por Ljung-Box</center>)
H-->I{<center>4. Os resíduos se aproximam<br/>de ruído branco?</center>}
I-->|Sim| J(5.Faça previsões)
I--> |Não| F
")
semplot %>% export_svg %>% charToRaw %>% rsvg %>% png::writePNG("semplot.png")

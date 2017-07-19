
install.packages("rJava","http://rforge.net/",type="source")
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")


initCoreNLP(type= "arabic", mem = "4g")


catInHat = c("بسم الله الرحمن الرحيم",
             "الحمدلله رب العالمين",
             "الرحمن الرحيم")

output = annotateString(catInHat)

output$token
output$collapsedProcDep
output$basicDep


write.xlsx(x, file = "output.xlsx")
write.table(output$token,file = "output")

#       args <- commandArgs(TRUE)

#ALTERNATIVE <- args[1]
#      CROWN <- args[2]

#	ALTERNATIVE=1          # 1: removed   2: relocated			# moved to Impressionistic.R
#	CROWN=FALSE            # F: stem      T: crown

source("fun/eval_NN.R")		# pre-compiled function

Spec <- c("SHORFALL","SHORJOHO","SHORPARF","SHORPAUC","SHORPILO","PARAMALA","MALLWRAY","DIMOMURI","FORDSPLE","ARDISANG",
          "BARRLANC","LITHNIEW","PENTLAXI","NEOSPHIL","DIPTKERR","DRYPLONG","LITHGRAC","LITHLEPT","MICRRETI","OCHAAMEN",
          "SCORBORN","SYZYLINE","SYZYTAWA","APORFALC","BACCTETR","CHISSARA","CLEICONT","DACRROST","DEHAGIGA","DYSOCYRT",
          "GONYKEIT","HYDNBORN","KNEMLATE","LITSCAUL","LITSOCHR","LOPHBECC","MADHKORT","MALLPENA","MALLSTIP","MASCCORY",
          "POLYCAUL","POLYRUMP","POLYSUMA","POLYXANT","REINHUMI","SYZYELOP","XANTVITE","VATIDULI")

Spec <- c("BEILGLAU", "MALLWRAY", "SHORJOHO", "SYZYELOP"); cat("\n")

for (sp in Spec[2]) {

	Period <- 'P1'; source("2_NN.R")

}

CHECK <- out.NN[order(out.NN$Tag), c(1, 3, 4, 5, 8:20)]; cat("\n")

CHECK$la_c <- round(CHECK$la_c, 1)
CHECK$la_C <- round(CHECK$la_C, 1)
CHECK$la_h <- round(CHECK$la_h, 1)
CHECK$la_H <- round(CHECK$la_H, 1)

names(CHECK)[6:17] <- c('n_c', 'n_C', 'n_h', 'n_H', 'a_c', 'o_C', 'o_h', 'o_H', 'l_c', 'l_C', 'l_h', 'l_H')

print(CHECK[CHECK$r == 1 | CHECK$r == 5 | CHECK$r == 10 | CHECK$r == 15 | CHECK$r == 20, ])

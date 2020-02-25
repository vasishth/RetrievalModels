Rscript -e "library(knitr); knit('VasishthEngelmannSCCP.Rnw')"

pdflatex VasishthEngelmannSCCP.tex; bibtex VasishthEngelmannSCCP ;  pdflatex VasishthEngelmannSCCP.tex; pdflatex VasishthEngelmannSCCP.tex

#rm *.{aux,bbl,blg,idx,log,out,toc}
cd $1
latex -shell-escape main.tex
bibtex main.aux
makeindex main.aux
pdflatex -shell-escape main.tex
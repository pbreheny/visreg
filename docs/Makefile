all: src/*.Rmd img/rgl.html css/main.css
	knitall -k --only-newer src/*.Rmd
	sed -i '/chunk fake/d' *.md
	rm -f img/*-fake-*
	jekyll b

img/rgl.html: src/rgl.R
	R CMD BATCH --vanilla src/rgl.R
	sed -i 's/"padding":15/"padding":0/g' rgl.html
	sed -i 's/"padding":40/"padding":0/g' rgl.html
	sed -i 's/width:480px;height:480px;/width:480px;height:480px;margin:auto;/g' rgl.html
	mv rgl.html img
	rm rgl.Rout

clean:
	mv README.md README.bak
	rm *.md
	rm img/*
	mv README.bak README.md

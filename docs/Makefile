SHA := $(shell git rev-parse --short HEAD)
Version := $(shell Rscript -e "cat(as.character(packageVersion('visreg')))")

all:	
	rm -f img/*
	rm -rf _site
	rm -rf cache
	rm -f *.md
	knitall src/*.Rmd
	make img/rgl.html
	sed -i '/chunk fake/d' *.md
	sed -i "s/version:.*/version: $(Version)/" _config.yml
	rm -f img/*-fake-*
	jekyll b
	echo 'HTML source code for http://pbreheny.github.io/visreg' > _site/README.md

img/rgl.html: src/rgl.R
	R CMD BATCH --vanilla src/rgl.R
	sed -i 's/"padding":15/"padding":0/g' rgl.html
	sed -i 's/"padding":40/"padding":0/g' rgl.html
	sed -i 's/width:480px;height:480px;/width:480px;height:480px;margin:auto;/g' rgl.html
	mv rgl.html img
	rm rgl.Rout

push:
	cd _site; \
	git init; \
	git add .; \
	git commit -m "Master build: $(SHA)"; \
	git push --force --quiet github:pbreheny/visreg.git master:gh-pages

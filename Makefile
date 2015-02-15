.phony: slides
slides:
	pandoc slides.md -t slidy -o slides.html \
               --smart --standalone --mathml --css=custom.css -o slides.html

.phony: slides
slides:
	pandoc slides.md --smart --standalone -t slidy --mathml >slides.html

.phony: slides
slides:
	pandoc slides.md -s -t slidy --mathml >slides.html

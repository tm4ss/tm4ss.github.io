# tm4ss - Text Mining for Social Scientists and Digital Humanists

This course consists of 8 tutorials written in R-markdown and further described in [this paper](http://gscl2017.dfki.de/proceedings.php). 

You can use *knitr* to create the tutorial sheets as HTML notebooks from the [R-markdown source code](https://github.com/tm4ss/tm4ss.github.io).

In the `/docs` folder, you have access to the **[rendered tutorials](https://tm4ss.github.io/docs)**.

## Tutorials

1. Web crawling and scraping
2. Text data import in R
3. Frequency analysis
4. Key term extraction
5. Co-occurrence analysis
6. Topic models (LDA)
7. Text classification
8. Part-of-Speech tagging / Named Entity Recognition

Click **[here for the rendered tutorials](https://tm4ss.github.io/docs)**.

## Render from source

Clone the repository

```
git clone https://github.com/tm4ss/tm4ss.github.io.git
```

Open the `Tutorials.Rproj` R-project file and run

```
rmarkdown::render_site(output_format = "html_document")
```

## License & Citation

This course was created by Gregor Wiedemann and Andreas Niekler. It was freely released under GPLv3 in September 2017. If you use (parts of) it for your own teaching or analysis, please cite

```
Wiedemann, Gregor; Niekler, Andreas (2017): [Hands-on: a five day text mining course for humanists and social scientists in R](http://ceur-ws.org/Vol-1918/wiedemann.pdf). Proceedings of the 1st Workshop Teaching NLP for Digital Humanities (Teach4DH@GSCL 2017), Berlin.
```
PDF

[Download paper](http://ceur-ws.org/Vol-1918/wiedemann.pdf)

Bibtex

```
@inproceedings{WN17,
  author    = {Gregor Wiedemann and Andreas Niekler},
  title     = {Hands-On: {A} Five Day Text Mining Course for Humanists and Social Scientists in {R}},
  booktitle = {Proceedings of the Workshop on Teaching {NLP} for Digital Humanities
               ({Teach4DH@GSCL 2017}), Berlin, Germany, September 12, 2017.},
  pages     = {57--65},
  year      = {2017},
  crossref  = {DBLP:conf/gldv/2017teach4dh},
  url       = {http://ceur-ws.org/Vol-1918/wiedemann.pdf},
}
```

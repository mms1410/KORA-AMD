SOURCE = R/report.Rmd
TARGET = R/report.html

all: $(TARGET)

$(TARGET): $(SOURCE)
	Rscript -e 'rmarkdown::render("R/report.Rmd")'

clean:
	rm $(TARGET)

SOURCE = R/report.Rmd
TARGET = assets/report.pdf

all: $(TARGET)

$(TARGET) : $(SOURCE)
	Rscript -e  "rmarkdown::render('R/report.Rmd')" # $< -> first prerequisite
	mv R/report.pdf assets/report.pdf

clean:
	rm $(TARGET)

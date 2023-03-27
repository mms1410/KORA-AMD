SOURCE = R/report.Rmd
TARGET = assets/report.html

all: $(TARGET)

$(TARGET) : $(SOURCE)
	Rscript -e 'rmarkdown::render("$<")' # $< -> first prerequisite
	mv R/report.html $(TARGET)

clean:
	rm $(TARGET)

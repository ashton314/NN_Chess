test:

docs:
	pod2text README.pod README

install:

run:

clean:
	rm *~

commit: clean
	git add .
	git commit

push: commit
	git push

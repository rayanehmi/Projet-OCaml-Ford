
build:
	@echo "\n==== COMPILING ====\n"
	ocamlbuild ftest.native

format:
	ocp-indent --inplace src/*

edit:
	code . -n

demo: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native graphs/graph1.txt 0 5 outfile
	@echo "\n==== RESULT ==== (content of result) \n"
	@cat outfile

demo2: build
	@echo "\n==== EXECUTING ====\n"
	./ftest.native src/paymentfile.txt -1 -2 outfile2
	@echo "\n==== RESULT ==== (content of result) \n"
	@cat outfile2

clean:
	-rm -rf _build/
	-rm ftest.native

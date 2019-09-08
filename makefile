compiler:
	lex grammar
	gcc lex.yy.c
clean:
	rm -rf a.out
	rm -rf lex.yy.c
	rm -rf *.txt

team:
	@echo "Team : team_alpha"
	@echo "Joseph Thomas"
	@echo "josthoma"

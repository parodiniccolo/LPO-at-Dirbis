package project.parser;

public enum TokenType { // important: SKIP, IDENT, and NUM must have ordinals 1, 2 and 3
	EOF, SKIP, IDENT, NUM, PRINT, VAR, PLUS, TIMES, EQ, MIN, ASSIGN, OPEN_PAR,
	CLOSE_PAR, START_PAIR, END_PAIR, STMT_SEP, EXP_SEP, OPEN_BLOCK, CLOSE_BLOCK, MINUS, NOT, AND, BOOL,
	IF, ELSE, FOR, TO, FST, SND, SEASON, SEASONOF, ORDINAL_SEASON, LOWER }
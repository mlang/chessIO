test: perftsuite.epd
	stack test

perftsuite.epd:
	wget https://github.com/mishoo/queen.lisp/raw/master/perftsuite.epd

.PHONY: test

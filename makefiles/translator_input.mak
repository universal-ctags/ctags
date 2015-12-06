.PHONY: list-translator-input
list-translator-input:
	@for x in $(TRANSLATOR_INPUT); do echo $$x; done

TRANSLATOR_INPUT = \
	$(NULL)

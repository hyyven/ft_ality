NAME = ft_ality
DUNE = dune		# ca c'est utile ptn
SRCS = $(shell find . -name "*.mli" -o -name "*.ml" -o -name "dune") # to avoid relink / find every .ml, .mli, dune files

all: $(NAME)

$(NAME): $(SRCS)
	$(DUNE) build
	@rm -f $(NAME)
	@cp _build/install/default/bin/ft_ality $(NAME)

run: $(NAME)
	dune exec $(NAME)

clean:
	$(DUNE) clean

fclean: clean
	rm -rf $(NAME)

re: fclean all
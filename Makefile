NAME = ft_ality
DUNE = dune		# ca c'est utile ptn
SRCS = $(shell find lib bin -type f \( -name "*.ml" -o -name "*.mli" -o -name "dune" \)) # to avoid relink | find every .ml, .mli, dune files

all: $(NAME)

$(NAME): $(SRCS)
	$(DUNE) build
	@rm -f $(NAME)
	@cp _build/install/default/bin/ft_ality $(NAME)

run: $(NAME)
	dune exec $(NAME) grammar/grammar.gmr

clean:
	$(DUNE) clean

fclean: clean
	@rm -rf $(NAME)

re: fclean all

# dune exec ft_ality -- <path/to/grammar> --debug
# ./ft_ality grammar/grammar.gmr --debug

# Makefile pour ft_ality_v2
# Compilation OCaml

# Compilateur
OCAMLC = ocamlc
OCAMLOPT = ocamlopt

# Fichiers sources (dans l'ordre de dépendance)
SOURCES = types.ml main.ml

# Fichiers objets
OBJS = $(SOURCES:.ml=.cmo)
OBJS_OPT = $(SOURCES:.ml=.cmx)

# Nom de l'exécutable
EXEC = ft_ality

# Règle par défaut: compilation en bytecode
all: $(EXEC)

# Compilation bytecode
$(EXEC): $(OBJS)
	$(OCAMLC) -o $(EXEC) $(OBJS)

%.cmo: %.ml
	$(OCAMLC) -c $<

# Compilation native (optimisée)
opt: $(EXEC).opt

$(EXEC).opt: $(OBJS_OPT)
	$(OCAMLOPT) -o $(EXEC) $(OBJS_OPT)

%.cmx: %.ml
	$(OCAMLOPT) -c $<

# Nettoyage
clean:
	rm -f *.cmi *.cmo *.cmx *.o $(EXEC) $(EXEC).opt

.PHONY: all opt clean

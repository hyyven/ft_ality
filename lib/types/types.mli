type input = string

type etat = int

type transition = etat * input * etat

type automate = {
  etats : etat list;
  lexique : input list;
  etat_initial : etat;
  etats_finaux : etat list;
  transitions : transition list;
}

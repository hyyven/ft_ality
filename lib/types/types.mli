type input = string

type etat = int

type transition = etat * input * etat

type key_dict = (input * string)

type automate = 
{
    etats : etat list;
    lexique : key_dict list ;
    etat_initial : etat;
    etats_finaux : etat list;
    transitions : transition list;
}

type move =
{
    nom : string;
    perso : string;
    sequence : input list;
}

type grammaire = 
{
    moves : move list;
}
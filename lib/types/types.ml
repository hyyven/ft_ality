type etat = int

type input = string

type transition = etat * input * etat

type key_dict = (input * string)

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

type automate = 
{
    etats : etat list;
    lexique : key_dict list ;
    etat_initial : etat;
    etats_finaux : etat list;
    transitions : transition list;
    grammar : grammaire;
}

(*ce serait bien qu'etat finaux deviennent (etat * move list) [etat de fin, move X] on verra + tard

et peut etre rajouter dans l'automate un truc pour print debug pour le bonus ? 
*)
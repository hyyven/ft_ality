type input = string
type etat = int
type transition = etat * input * etat

type automate = 
{
    etats : etat list;
    lexique : input list ;
    etat_initial : etat;
    etats_finaux : etat list;
    transitions : transition list;
}
    
(*types move 
{
    nom : string;
    perso : string;
    sequence : input list;
} 

type grammaire = 
{
    moves : move list;
}
ce serait bien qu'etat finaux deviennent (etat * move list) [etat de fin, move X] on verra + tard

et peut etre rajouter dans l'automate un truc pour print debug pour le bonus ? 
*)
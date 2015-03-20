(****************************************)

(**  Nom : ATTIA                       **)
(**  Prenom : Anthony                  **)
(**  Année : 2014 - 2015               **)

(***************************************)





(*** Début du projet ***)

let _ = Random.self_init();;

(*************** 5.2 Listes ***************)

(* Question 1 *)
(* Insere un élément à une position donnée dans une liste *)
let rec insert_at_pos  x liste  n =
  match liste with 
  | [] -> [x]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at_pos  x t (n-1) ;;

(* Question 2 *)
(* Insere un élémenet à une position aléatoire dans une liste *)
let  insert_at_random  x liste =
  let pos = Random.int (( List.length liste) +1)
  in  insert_at_pos x  liste pos ;;

(* Question 3 *)
(* Mélange une liste en prenant ses éléments un par un et en les insérant à une position aléatoire  *)
let  shuffle  listeInit  = 
  let rec melange l1 l2 = match l1 with 
    |[] -> l2
    |hd::t -> melange(t ) (insert_at_random hd l2 )
  in melange (List.tl listeInit) [(List.hd listeInit)]
  

(* Question 4 *)
(* Extrait au hasard un élément de la liste, l'indice de la position d'un élément dans la liste est choisit au hasard *)
let  random_get  liste =
  match liste with 
  | [] -> None
  | l -> let pos = Random.int ( List.length l)  
	 in Some (List.nth l pos);;

random_get  ["a";"b";"c"];;


(* Question 5 *)
(* renvoi la liste des éléments qui ne sont pas None *)
let  clean_list l =
  let rec aux lb lr =
    match lr with
    |[] -> lb
    |(Some x)::l  -> aux (lb @ [x]) l
    |None ::l -> aux lb l
  in aux [] l;;

(*************** 5.3 Age et sexe  ***************)

(* Question 6 *) 

type sexe = Feminin|Masculin;;

type age = Bebe | Enfant | Adulte;;

(* Question 7 *)

(* Retourne le sexe d'un invidu au hasard lors de la création d'un nouvel individu  *)
let  random_sex () =  
  let search = Random.int 2 in
  if search = 0 then Feminin 
  else Masculin;;

(* Retourne l'age d'un invidu au hasard lors de la création d'un nouvel individu  *)
let random_age () = 
  let rand = Random.int 3 in 
  match rand with
  |0 -> Bebe
  |1 -> Enfant 
  |_ -> Adulte;;


(* Question 8 *)

(* fonction qui transforme un element de type sexe en string *)
let string_of_sex sexe = 
  match sexe with 
  |Feminin -> Printf.sprintf("Feminin")
  |Masculin -> Printf.sprintf("Masculin");;

(* fonction qui transforme un element de type age en string *)
let string_of_age age = 
  match age with
  | Bebe -> Printf.sprintf("Bebe")
  | Enfant -> Printf.sprintf("Enfant")
  | Adulte -> Printf.sprintf("Adulte");;



(*************** 6 La planète Symbioz   ***************)
(*************** 6.1 Signature  ***********************)

(* Question 9 *)

(* Déclaration du module planete *)
module type PLANETE = 
sig
  type pos
  val ouest : pos->pos  
  val est : pos -> pos
  val nord : pos -> pos
  val sud : pos -> pos
  val at_pos : ('a -> pos ) -> pos -> 'a list -> 'a list
  val sort_by_pos : ('a -> pos ) -> 'a list -> 'a list list
  val random_pos : unit -> pos
  val display : (int->int->unit) -> pos -> unit
  val clear : unit-> unit
end;;

(*************** 6.2 Module    ***************)

(* Question 10 *)

(* Implémentation  du module Symbioz:PLANETE, la planète est de taille size_x et size_y  *)  

module Symbioz:PLANETE = 
struct

  let size_x = 10;;
  let size_y = 10;;

  type pos = int * int;;

  let ouest (x,y)  =
    if x = 0  then  size_x-1 ,y  else (x - 1), y;;
  
  let est (x, y)  =
    if x = size_x  then 1,y else (x + 1) ,y;;

  let nord (x, y) =
    if y = size_y then x, 1 else x , (y+1);;

  let sud (x, y) =
    match y with
    |0 -> (x,size_y -1)
    |_ -> (x, (y-1));;
  
  let  random_pos() = (Random.int size_x) , (Random.int size_y);;
  
  (* Fonction récurvise qui prend une fonction d'évaluation une position et une liste, renvoyant la liste des éléments présents à la position donnée *)    
  let rec  at_pos f pos l  =
    match l with
    |[] -> []
    |hd::t -> if pos = f hd then hd::at_pos f pos t else at_pos f pos t;;

  (* Fonction qui renvoie une liste de listes, chacune de ces listes étant l'ensemble  de tous les éléments à une position donnée *)
  let rec sort_by_pos f list = 
    let rec search_x x = if x = size_x then [] 
      else 
	let rec search_y  y = if y = size_y then []
	  else 
	    at_pos f (x,y)list::search_y (y +1) 
	in 
        search_y 0 @ search_x (x+ 1)
    in
    search_x 0;;
  
  let display f (x,y) = f x y ;;
  
  let clear() = ();;

end;;

(*************** 7 Génétique élémentaire   ***************)
(*************** 7.1 SIGNATURE ***************************)

module type INDIVIDU = 
sig
  type pos
  type individu 
  val egalite : individu  -> individu -> bool
  val create_random : unit -> individu
  val sex_individu : individu -> sexe
  val age_individu : individu -> age
  val reproduire : int -> individu -> individu ->  individu list
  val manger : int -> individu -> individu option
  val bouger : (pos->int) -> individu -> individu 
  val vieillir : individu -> individu option
  val afficher  : individu -> unit
  val position_individu : individu -> pos 
end;;


(* Question  12 *)
module type MAKE_INDIVIDU = functor( P:PLANETE) -> INDIVIDU with type pos = P.pos;;  


(***************** 7.2 zherbs *********************)

(* Question 13 *)

module Make_Zherb : MAKE_INDIVIDU = 
  functor (P:PLANETE) ->
struct 

  
  type pos = P.pos;;
  type individu = {age: age ; id_individu:int; pos:pos};;
  
  let last_id =  ref 0;;
  
  exception Sexe_error;;
  
  (* Compare les id des deux individus pour savoir si ils sont égaux *)
  let egalite ind_1 ind_2 = if ind_1.id_individu  = ind_2.id_individu then true else false;;

  (* Crée aléatoirement un nouvel enregistrement d'une zherbs avec des valeurs aléatoires *)    
  let create_random () = 
    let age =  random_age() in  
    let position = P.random_pos() in
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age ; id_individu = id_individu; pos = position};;
  
  (* Crée  un nouvel enregistrement d'une zherbs avec des valeurs données *) 
  let create_zherbs age  position = 
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age ; id_individu = id_individu; pos = position};;
  
  (* L'appel de cette fonction lève une erreur car une zherbs n'a pas de sexe *)
  let sex_individu ind = raise Sexe_error;; 
  let age_individu ind = ind.age;;
  let position_individu ind = ind.pos;;

  (* Une zherb née qu'avec une seule autre zherb *)
  let reproduire e p m = 
    let new_pos = match Random.int 10 with
      |0 -> P.est m.pos
      |1 -> P.nord m.pos
      |2 -> P.sud m.pos
      |3 -> P.ouest m.pos  
      |_ -> m.pos 
    in 
    let new_ind () = create_zherbs Bebe new_pos in 
    let rec create_list_zherbs e new_ind  = match e with
      |0 -> []
      |_ -> (new_ind ()) ::(create_list_zherbs (e-1) new_ind)
    in 
    create_list_zherbs e new_ind;;

  (* Renvoi individu option car une zherbs ne mange pas *)
  let manger e ind = 
    Some ind
      
  (* Renvoi l'individu initial change mise à jour de la position car une zherbs ne bouge pas *)    
  let bouger fe ind = 
    ind;;
  
  (* Une zherbs passe un tour bébé, un tour enfant et un tour adulte *)
  let vieillir ind = 
    let age_ind = ind.age in 
    match age_ind  with 
    |Bebe -> Some {ind with age = Enfant}
    |Enfant ->Some { ind with age =  Adulte}
    |Adulte -> None;;

  let afficher ind =
    print_string (string_of_age(ind.age) )

end;;

(***************** 7.3 Krapits ****************)
(* Question 14 *)
module Make_Krhapit : MAKE_INDIVIDU = 
  functor (P:PLANETE) ->
struct 
  
  

  type pos = P.pos;;
  type individu = {age: age ; nb_tour:int; sexe:sexe ; id_individu:int; pos:pos; life:int};;
  
  exception Sexe_error;;

  let last_id =  ref 0;;
  let life_max =  6;;

  (* Compare les id des deux individus pour savoir si ils sont égaux *)
  let egalite ind_1 ind_2 = if ind_1.id_individu  = ind_2.id_individu then true else false;;

  (* Crée aléatoirement un nouvel enregistrement d'un krapits avec des valeurs aléatoires *)
  let create_random () = 
    let age =  random_age() in  
    let sexe = random_sex() in 
    let position = P.random_pos() in
    let nb_tour = 0 in
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age; sexe = sexe ; id_individu = id_individu; pos = position ; life =  life_max; nb_tour = nb_tour };;

  (* Crée  un nouvel enregistrement d'un krapits avec des valeurs données *)
  let create_krapits age sexe position = 
    last_id := !last_id +1; 
    {age = age; sexe = sexe ; id_individu = !last_id; pos = position; life = 6; nb_tour = 0 };;
  
  (* Récupération des attributs d'un krapits *)
  let sex_individu ind = ind.sexe ;;
  let age_individu ind = ind.age;;
  let life_individu ind = ind.life;;
  let position_individu ind = ind.pos;;
  
  (* Fonction de reproduction entre 2 krapits, ils peuvent faire de 1 à 5 enfants *)
  let reproduire e p m = 
    let new_pos = match Random.int 5  with
      |0 -> P.est m.pos
      |1 -> P.nord m.pos
      |2 -> P.sud m.pos
      |3 -> P.ouest m.pos  
      |_ -> m.pos 
    in
    let new_ind () = create_krapits  Bebe (random_sex())  new_pos in
    let e = (Random.int 5) + 1 in
    let rec create_list_krapits e new_ind  = match e with
      |0 -> []
      |_ -> (new_ind ()) ::(create_list_krapits (e-1) new_ind)
    in 
    create_list_krapits e new_ind;;
  
  (* Fonction qui fait manger un krapits, si ce dernier ne mange pas il perd un point de vie à chaque tour, si il mange, sa vie revient au max *)
  let manger e ind =
    if e <>  0
    then
      Some { ind with life = life_max}
    else if ind.life <= -1
    then
      None  
    else
      Some {ind with life = ind.life -1};;

  (* Un krapits peut bouger dans une direction aléatoirement *)
  let bouger fe ind = 
    if ind.life <= 3 
    then
      let position_hasard = Random.int 4  in
      match position_hasard with 
      |0 -> {ind with pos = (P.est (position_individu ind))}
      |1 -> {ind with pos = (P.sud (position_individu ind))}
      |2 -> {ind with pos = (P.nord (position_individu ind))} 
      |_ -> {ind with pos = (P.ouest (position_individu ind))}  
    else
      ind;;

  (* Un krapits vit un tour enfant, un tour bébé, et de 1 à 4 tours adulte puis meurt *)
  let vieillir ind = 
    
    let get_age = ind.age in 
    let nb_tour_rand =  (Random.int 4) + 1 in
    match get_age  with 
    |Bebe -> Some {ind with age = Enfant }
    |Enfant ->Some { ind with age = Adulte; nb_tour = nb_tour_rand} 
    |Adulte -> if ind.nb_tour = 0 then None else Some {ind with nb_tour = ind.nb_tour -1} ;;
  
  let afficher ind =
    print_string (string_of_age(ind.age));;

end;;

(***************** 7.3 Krapits ****************)
(** Question 15 **)
module Make_Krogul : MAKE_INDIVIDU = 
  functor (P:PLANETE) ->
struct 
  
  
  type pos = P.pos;;
  type individu = {age: age ; nb_tour:int; sexe:sexe ; id_individu:int; pos:pos; life:int};;
  
  exception Sexe_error;;

  (* Variables globales *)
  let last_id =  ref 0;;
  let life_max =  6;;
  let nb_max_enfant = 2;;
  
  (* Compare les id des deux individus pour savoir si ils sont égaux *)
  let egalite ind_1 ind_2 = if ind_1.id_individu  = ind_2.id_individu then true else false;;
  
  (* Crée aléatoirement un nouvel enregistrement d'un kroguls avec des valeurs aléatoires *)
  let create_random () = 
    let age =  random_age() in  
    let sexe = random_sex() in 
    let position = P.random_pos() in
    let nb_tour = 0 in
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    
    {age = age; sexe = sexe ; id_individu = id_individu; pos = position ; life =  life_max ; nb_tour = nb_tour };;
  
  (* Crée un nouvel individu avec des valeurs données *)
  let create_krogul age sexe position = 
    last_id := !last_id +1; 
    {age = age; sexe = sexe ; id_individu = !last_id; pos = position; life = life_max; nb_tour = 0 };;
  
  (* Récupéarations des données de l'individu *)
  let sex_individu ind = ind.sexe ;;
  let age_individu ind = ind.age;;
  let life_individu ind = ind.life;;
  let position_individu ind = ind.pos;;
  
  (* Reproduction entre 2 kroguls *)
  let reproduire e p m = 
    let new_pos = match Random.int 20  with
      |0 -> P.est m.pos
      |1 -> P.nord m.pos
      |2 -> P.sud m.pos
      |3 -> P.ouest m.pos  
      |_ -> m.pos 
    in
    let new_ind () = create_krogul  Bebe (random_sex())  new_pos in
    let e = (Random.int nb_max_enfant)  in
    let rec create_list_krogul e new_ind  = match e with
      |0 -> []
      |_ -> (new_ind ()) ::(create_list_krogul (e-1) new_ind)
    in 
    create_list_krogul e new_ind;;
  
  (* Fait manger un krogul *)
  let manger e ind =
    if e <>  0  && ind.life <= 6 
    then
      Some { ind with life = ind.life + 5}
    else if ind.life <= -1
    then 
      None
    else
      Some { ind with life = ind.life -2};;

  (* On récupère le maximum d'une liste *)
  let max_list l = 
    List.fold_right (fun x -> fun y -> if x > y then x else y ) l 0;;

  (* On récupere grace à la fonction d'évaluation, la quantité de nourriture à chaque case adjacente;; en cas d'égalité on choisit 
     au hasard la direction parmis celle qui présente le plus de nourriture *)
  let bouger fe ind = 
    if ind.life <= 4
    then
      let recup_positions  = 
	List.map (fun f -> f ind.pos) [P.est ; P.ouest ; P.nord ; P.sud] 
      in 
      let eval = 
	List.map  fe recup_positions 
      in 
      let max_eval =  List.filter(fun e -> (max_list (eval))= fe(e)) recup_positions
      in
      let au_hasard = Random.int(List.length max_eval) in 
      {ind with pos = List.nth recup_positions au_hasard }
    else 
      ind;;
  
  (* En fonction d 'un nombre de tour défini, un krogul vit la durée du nombre de tour lors de sa création et passe de 2 à 5 tours adulte *)	
  let vieillir ind = 
    let get_age = ind.age 
    in 
    let nb_tour_adulte =  (Random.int 5) + 2 
    in
    match get_age  with 
    |Bebe -> if ind.nb_tour = 0 then Some{ind with age = Enfant; nb_tour = 2} 
      else Some {ind with age = Bebe ;  nb_tour = ind.nb_tour -1  }

    |Enfant -> if ind.nb_tour = 0 then Some{ind with age = Adulte ; nb_tour = nb_tour_adulte} 
      else Some { ind with age = Enfant; nb_tour = ind.nb_tour - 1 }

    |Adulte -> if ind.nb_tour = 0 then None 
      else Some {ind with nb_tour = ind.nb_tour -1} ;;
  
  let afficher ind =
    print_string (string_of_age(ind.age));;


end;;



(* Question 16 *)
(*** Implémentation  de la signature population  ***)

module type POPULATION = 
sig
  
  type pos
  type individu 
  type population
  type nourriture
  
  val create_pop : int -> population
  val reduce : (individu -> 'a ->'a) -> population -> 'a -> 'a 
  val map :('a -> 'b) -> 'a list -> 'b list
  val iter: ('a -> unit ) -> 'a list -> unit 
  val vieillissement : population -> population
  val mouvement :  nourriture -> population -> population 
  val kill_individu : individu -> population -> population
  val nourriture :nourriture -> population -> (nourriture * population)
  val sous_pop : pos -> population -> population
    
end;;


(* Question 17 *)

(*Declaration de la signature MAKE_PLANTES, elle prend en paramètre un individu et un make_individu renvoyant une population *)
module type MAKE_PLANTES = functor(P:PLANETE ) 
    -> functor(MI:MAKE_INDIVIDU) 
	-> POPULATION with type pos = P.pos 
		      and type nourriture = unit 
		      and type individu = MI(P).individu;;




(* Question 18 *)

(* Foncteurs qui prennent en argument une PLANETE, une POPULATION , et un MAKE_INDIVIDU et renvoient une POPULATION *)
module type MAKE_ANIMAUX = functor(P:PLANETE) 
    -> functor(Proies:POPULATION with type pos = P.pos) 
	-> functor(MI:MAKE_INDIVIDU ) 
	    -> POPULATION with type pos = P.pos and type nourriture = unit;;  




(* Question 19*)

(*** Foncteur Make_zherbs : MAKE_PLANTES  ***)
module Make_Zherbs : MAKE_PLANTES =
  functor(P:PLANETE) -> functor(MI:MAKE_INDIVIDU) -> 
struct
  
  module MIP = MI(P);;
  
  type pos = P.pos
  type individu = MIP.individu;;
  type population = individu list;;  
  type nourriture = unit;;

  (* fonction récurvise, insérant chaque nouvel individu crée dans la population *)
  let insert_ind_population_list nombre_individu new_individu =
    let rec insert_ind_population_list e new_ind  = match e with
      |0 -> []
      |_ -> (new_ind ()) ::(insert_ind_population_list (e-1) new_ind)
    in 
    insert_ind_population_list nombre_individu new_individu;;

  (* Chaque individu est crée aléatoirement et y est ajouté  à la population pour créer la nouvelle population *)
  let create_pop nbre_individu = insert_ind_population_list nbre_individu  MIP.create_random;;
  
  (* fonctions sur les listes *)
  let map = List.map
  let reduce = List.fold_right
  let iter = List.iter
  let filter = List.filter

  (* En cas de mort l'age de l'individu passe à none, clean_list enlève ces individus morts de la liste population *)
  let vieillissement pop = clean_list(map MIP.vieillir pop);;

  (* Une plante ne bouge pas, donc on renvoit que la population avec toutes les plantes à leurs positions initiale *)
  let mouvement nou pop =  pop;; 
  
  (* On filtre l'individu donnée en argument avec celui dans la population, si ces deux correspondent, on recrée une nouvelle liste privée de l'individu trouvé *)
  let kill_individu individu pop = filter (fun ind_list ->not ( MIP.egalite ind_list individu)) pop;;
  
  (* Prend aléatoirement un élément de type individu dans la liste population, renvoie None si population est vide *)
  let random_get_ind population = random_get population;;
  
  (* Les plantes ne se nourrissent pas *)
  let nourriture nour population  = ((),population);; 
  
  (* Renvoie la population dans la case donnée, on se sert de la fonction at_pos qui récupère la liste des éléments à une pos donnée *)
  let sous_pop  pos population = P.at_pos (MIP.position_individu ) pos population 

end;;

(*
  let sous_population pos pop = P.at_pos P.pos pop 
*)


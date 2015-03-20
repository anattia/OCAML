let _ = Random.self_init();;

(*************** 5.2 Listes ***************)

(* Question 1 *)
(* Insere un élément à une position donnée dans une liste *)
let rec insert_at_pos  x liste  n =
  match liste with 
  | [] -> [x]
  | h :: t as l -> if n = 0 then x :: l else h :: insert_at_pos  x t (n-1) ;;

(* Quetion  2 *)
(* Insere un élémenet à une position aléatoire dans une liste *)
let  insert_at_random  x liste =
  let pos = Random.int (( List.length liste) +1)
  in  insert_at_pos x  liste pos ;;

(* Question 3 *)
(* Mélange une liste en prenant ses éléments un par un et en les insérant à une position aléatoire  *)
let shuffle d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let randomize l =
  let random_cmp x y =
    if x = y then 0
    else if Random.bool() then 1 else -1
  in
  List.sort random_cmp l

(* Question 4 *)
(* *)
let  random_get  = function
  | [] -> None
  | l -> let pos = Random.int ( List.length l)  
	 in Some (List.nth l pos);;

random_get  ["a";"b";"c"];;


(* Question 5 *)
(* renvoi la liste des éléments qui ne sont pas none *)
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
  let rann = Random.int 2 in 
  match rann with
  |0 -> Bebe
  |1 -> Enfant 
  |2 -> Adulte;;


(* 8 *)
(* fonction qui transforme un element de type sexe en string *)
let string_of_sex sexe = match sexe with 
  |Feminin -> Printf.sprintf("Feminin")
  |Masculin -> Printf.sprintf("Masculin");;

(* fonction qui transforme un element de type age en string *)
let string_of_age age = match age with
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

  type pos = int * int

  let ouest (x,y)  =
    if x = 0  then  size_x-1 ,y  else (x - 1), y
      
  let est (x, y)  =
    if x = size_x  then 1,y else (x + 1) ,y

  let nord (x, y) =
    if y = size_y then x, 1 else x , (y+1)  

  let sud (x, y) =
    match y with
    |0 -> (x,size_y -1)
    |_ -> (x, (y+1))
      
  let  random_pos() =
    (Random.int size_x) , (Random.int size_y)  
      
  let rec  at_pos f pos l  =
    match l with
    |[] -> []
    |hd::t -> if pos = f hd then hd::at_pos f pos t else at_pos f pos t 

  let rec sort_by_pos f list = 
    let rec search_x x = if x = size_x then [] else 
	let rec search_y  y = if y = size_y then []
	  else at_pos f (x,y)list::search_y (y +1) 
	in 
        search_y 0 @ search_x (x+ 1)
    in
    search_x 0

  let display f (x,y) = f x y 
    
  let clear() = ()
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
end;;
(*
  
  
  
  val age_individu : individu ->  age
  val manger : individu -> int -> individu option
  val bouger : (pos->int) -> individu -> individu 
  
  val vieillir : individu -> individu option
  
  
*)
(* 12 *)
module type MAKE_INDIVIDU = functor( P:PLANETE) -> INDIVIDU with type pos = P.pos;;  


(***************** 7.2 zherbs *********************)
module Make_Zherb : MAKE_INDIVIDU = 
  functor (P:PLANETE) ->
struct 
  let last_id =  ref 0
  type pos = P.pos
  type individu = {age: age ; id_individu:int; pos:pos}
  exception Sexe_error
  let egalite ind_1 ind_2 = if ind_1.id_individu  = ind_2.id_individu then true else false
      
  let create_random () = 
    let age =  random_age() in  
    let position = P.random_pos() in
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age ; id_individu = id_individu; pos = position}
      
  let create_zherbs age  position = 
    let age = age in 
    let position = position in 
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age ; id_individu = id_individu; pos = position}

  let sex_individu ind = raise Sexe_error 
  
  let age_individu ind = ind.age

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
  
  let manger e ind = 
    Some ind
      
  let bouger fe ind = 
    ind
      
  let vieillir ind = 
    let get_age = ind.age in 
    match get_age  with 
    |Bebe -> Some {ind with age = Enfant}
    |Enfant ->Some { ind with age =  Adulte}
    |Adulte -> None 

  let afficher ind =
    print_string (string_of_age(ind.age))

end;;

(***************** 7.3 Krapits ****************)
module Make_Krhapit : MAKE_INDIVIDU = 
  functor (P:PLANETE) ->
struct 
  
  let last_id =  ref 0;;
  
  let life_max =  6;;

  type pos = P.pos;;
  
  type individu = {age: age ; nb_tour:int; sexe:sexe ; id_individu:int; pos:pos; life:int};;
  
  exception Sexe_error;;

  let egalite ind_1 ind_2 = if ind_1.id_individu  = ind_2.id_individu then true else false;;
  
  let create_random () = 
    let age =  random_age() in  
    let sexe = random_sex() in 
    let position = P.random_pos() in
    let nb_tour = 0 in
    let lifek = 6 in
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age; sexe = sexe ; id_individu = id_individu; pos = position ; life =  lifek ; nb_tour = nb_tour }
      
  let create_krapits age sexe position = 
    last_id := !last_id +1; 
    {age = age; sexe = sexe ; id_individu = !last_id; pos = position; life = 6; nb_tour = 0 };;
  
  let sex_individu ind = ind.sexe ;;
  
  let age_individu ind = ind.age;;

  let life_individu ind = ind.life;;

  let position_individu ind = ind.pos;;
  
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
  
  
  
  
  let manger e ind =
    if e <>  0
    then
      Some { ind with life = life_max}
    else if ind.life <= -1
    then
      None  
    else
      Some {ind with life = ind.life -1}

  ;;
  let bouger fe ind = 
    if ind.life <= 3 
    then
      let position_hasard = Random.int 3 in
      match position_hasard with 
      |0 -> {ind with pos = (P.est (position_individu ind))}
      |1 -> {ind with pos = (P.sud (position_individu ind))}
      |2 -> {ind with pos = (P.nord (position_individu ind))} 
      |3 -> {ind with pos = (P.ouest (position_individu ind))}  
    else
      ind;;

  
  let vieillir ind = 
    let age_adulte = 3 in
    let get_age = ind.age in 
    let nb_tour_rand =  (Random.int 4) + 1 in
    match get_age  with 
    |Bebe -> Some {ind with age = Enfant }
    |Enfant ->Some { ind with age = Adulte; nb_tour = nb_tour_rand} 
    |Adulte -> if ind.nb_tour = 0 then None else Some {ind with nb_tour = ind.nb_tour -1} ;;
  
  let afficher ind =
    print_string (string_of_age(ind.age))

end;;


module Make_Krogul : MAKE_INDIVIDU = 
  functor (P:PLANETE) ->
struct 
  
  let last_id =  ref 0;;
  
  let life_max =  6;;

  type pos = P.pos;;
  
  type individu = {age: age ; nb_tour:int; sexe:sexe ; id_individu:int; pos:pos; life:int};;
  
  exception Sexe_error;;

  let egalite ind_1 ind_2 = if ind_1.id_individu  = ind_2.id_individu then true else false;;
  
  let create_random () = 
    let age =  random_age() in  
    let sexe = random_sex() in 
    let position = P.random_pos() in
    let nb_tour = 0 in
    let lifek = 6 in
    last_id := !last_id +1; 
    let id_individu = !last_id in 
    {age = age; sexe = sexe ; id_individu = id_individu; pos = position ; life =  lifek ; nb_tour = nb_tour }
      
  let create_krogul age sexe position = 
    last_id := !last_id +1; 
    {age = age; sexe = sexe ; id_individu = !last_id; pos = position; life = 6; nb_tour = 0 };;
  
  let sex_individu ind = ind.sexe ;;
  
  let age_individu ind = ind.age;;

  let life_individu ind = ind.life;;

  let position_individu ind = ind.pos;;
  
  let reproduire e p m = 
    let new_pos = match Random.int 20  with
      |0 -> P.est m.pos
      |1 -> P.nord m.pos
      |2 -> P.sud m.pos
      |3 -> P.ouest m.pos  
      |_ -> m.pos 
    in
    
    let new_ind () = create_krogul  Bebe (random_sex())  new_pos in
    let e = (Random.int 2)  in
    let rec create_list_krogul e new_ind  = match e with
      |0 -> []
      |_ -> (new_ind ()) ::(create_list_krogul (e-1) new_ind)
    in 
    create_list_krogul e new_ind;;
  
  let manger e ind =
    if e <>  0  && ind.life <= 6 
    then
      Some { ind with life = ind.life + 5}
    else if ind.life <= -1
    then 
      None
    else
      Some { ind with life = ind.life -2};;

  let bouger fe ind = 
    ind

  let vieillir ind = 
    let get_age = ind.age in 
    let nb_tour_adulte =  (Random.int 5) + 2 in
    match get_age  with 
    |Bebe -> if ind.nb_tour = 0 then Some{ind with age = Enfant; nb_tour = 2} 
      else Some {ind with age = Bebe ;  nb_tour = ind.nb_tour -1  }
    |Enfant -> if ind.nb_tour = 0 then Some{ind with age = Adulte ; nb_tour = nb_tour_adulte} 
      else Some { ind with age = Enfant; nb_tour = ind.nb_tour - 1 }
    |Adulte -> if ind.nb_tour = 0 then None else Some {ind with nb_tour = ind.nb_tour -1} ;;
  
  let afficher ind =
    print_string (string_of_age(ind.age))


end;;


let random_test i =
  let v = Random.int i in
  print_int v;;

let test_unit i = 
  print_string i;;

(* 8 *)
(*if ind.life <= 4
  then
  if fe (P.ouest ind.pos) < fe (P.est ind.pos) then
  if fe (P.est ind.pos) < fe (P.nord ind.pos) then
  if fe(P.nord ind.pos) < fe (P.sud ind.pos) then
  P.sud ind.pos
  else
  P.nord ind.pos
  else if fe (P.est ind.pos) > fe (P.sud ind.pos)
  then
  P.est ind.pos
  else
  P.sud ind.pos
  else if fe (P.ouest ind.pos) > fe (P.nord ind.pos)
  then 
  P.ouest ind.pos
  else
  P.nord ind.pos
  else
  fe(P.est ind.pos)
  else
  P.ouest ind.pos 
  else
  ind;;
*)

(* Question 16 *)
(*** Implémentation  de la signature population  ***)

module type POPULATION = 
sig
  
  type pos
  
  type individu 
  
  type population
  
  type nourriture

  val create_pop : int  -> population  
  
  val reduce : (individu -> 'a ->'a) -> population -> 'a -> 'a 
  
  val map : (individu -> 'a) -> population -> 'a list
  
  val iter: (individu -> unit ) -> 'a list -> unit 
  
  val sous_population : pos-> population -> population 
  
  val kill_individu : individu -> population -> population
  
  val vieillissement : population -> population
  
  val reproduction  : population -> population
  
  val nourriture :nourriture -> population -> (nourriture * population)

end;;

(* Question 17 *)

(*Declaration de la signature MAKE_PLANTES, elle prend en paramètre un individu et un make_individu renvoyant une population *)
module type MAKE_PLANTES = functor(P:PLANETE ) 
    -> functor(MI:MAKE_INDIVIDU) 
	-> POPULATION with type pos = P.pos and type nourriture = unit;;


(* Question 18 *)

(* Foncteurs qui prennent en argument une PLANETE, une POPULATION , et un MAKE_INDIVIDU et renvoient une POPULATION *)
module type MAKE_ANIMAUX = functor(P:PLANETE) 
    -> functor(Proies:POPULATION with type pos = P.pos) 
	-> functor(MI:MAKE_INDIVIDU ) 
	    -> POPULATION with type pos = P.pos and type nourriture = unit;;  

(* Question 19*)

(*  Foncteur Make_zherbs : MAKE_PLANTES *)
module Make_Zherbs : MAKE_PLANTES =
  functor(P:PLANETE) -> functor(MI:MAKE_INDIVIDU) -> 
struct
  
  module MIP = MI(P)
  
  type pos = P.pos
  
  type individu = MIP.individu
  
  type population = individu list 
  
  type nourriture = unit

  let insert_ind_population_list nombre_individu new_individu =
    let rec insert_ind_population_list nombre_individu l =
      if nombre_individu = 0
      then l
      else
	insert_ind_population_list(nombre_individu-1) (new_individu())::l ;;
    in 
    insert_ind_population_list nombre_individu []

  let create_pop entier = insert_ind_population_list nbre_individu  MIP.create_random;;
  
  let map = List.map
  
  let reduce = List.fold_right
  
  let iter = List.iter
    
  let sous_population pos pop = P.at_pos MIP.pos pop 
  
  let vieillissement pop = clean_list(map MIP.veillir pop);;
  
  let bouger pop = map MIP.bouger pop;;
  
  let nourriture nou pop  = (), clean_list( map MIP.manger pop);; 
  
  let kill_individu individu population = List.filter (fun l -> individu.pos) pop

end;;

let pos x = x >= 0;;
let id = 2;;
List.filter (fun x -> (List.exists  pos [1;2;3;4;5])) [1;2;3;4;5];;


let random i = 
  let x = (Random.int i) +1  in
  print_int x ;;

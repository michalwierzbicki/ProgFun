let sort_lex l = List.sort compare l;;

let sign x =
   if x > 0 then 1
   else if x < 0 then -1
   else 0;;
 
 let il_wekt (x0, y0) (x1, y1) (x2, y2) =
   sign ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0));;

 let rec pol_otoczki_rec stos l = 
   match stos with
      h1::h2::ts ->
        (match l with
	   h::t -> if il_wekt h2 h1 h = (-1) then pol_otoczki_rec (h2::ts) l
		   else pol_otoczki_rec (h::stos) t
           |  []   -> stos)
   | _ ->
        (match l with
           h::t -> pol_otoczki_rec (h::stos) t
         | [] -> stos);;
      

 let pol_otoczki l1 = pol_otoczki_rec [] l1;;
   
   
   let otoczka l1 = 
       let l = sort_lex l1 in
          let _::pierwsze_pol = pol_otoczki l in
              let _::drugie_pol = pol_otoczki (List.rev l) in
                 pierwsze_pol@drugie_pol;;
   
   
 let rec trojkaty_z_otoczki_2 otoczka x =
    match otoczka with
       h1::h2::t -> (h1, h2, x)::(trojkaty_z_otoczki_2 (h2::t) x)
    | _ -> [];;
 
 let triangulacja_otoczki otoczka =
    match otoczka with h1::tail -> trojkaty_z_otoczki_2 tail h1;;
 
 
 let czy_w_trojkacie (x, y, z) w =
    il_wekt x y w = il_wekt x y z && il_wekt y z w = il_wekt y z x && il_wekt z x w = il_wekt z x y;;
    
 let rec dodaj_wierzcholek lista_trojkatow punkt =
   match lista_trojkatow with
     (w1, w2, w3)::t -> if czy_w_trojkacie (w1, w2, w3) punkt then ((w1, w2, punkt)::((w2, w3, punkt)::((w1, punkt, w3)::(dodaj_wierzcholek t punkt))))
                        else ((w1, w2, w3)::(dodaj_wierzcholek t punkt))
   | []              -> [];;
 
 let rec uzupelnij totoczka lista =
    match lista with
      h::t -> uzupelnij (dodaj_wierzcholek totoczka h) t
    | []   -> totoczka;;
 
 let przyklad = [(100, 300); (111, 555); (10, 673); (312, 222); (100, 500); (200, 200); (200, 400); (300, 600); (400, 300); (500, 100); (500, 500); ( 600, 200); (700, 700); (800, 200); (800, 500); (444, 132); (892, 111); (789, 742)];;

 
 
 
 let rec udr h t =
   match t with
      x::l -> if x == h then (udr h l)
              else h::(udr x l)
   | []    -> h::[];;
 
 let usundup l =
   udr (List.hd l) (List.tl l);;
 
 
 let cwk (x, y, z) (xp, yp, zp) = (* czy wspolna krawedz *)
   List.length (usundup (sort_lex [x; y; z; xp; yp; zp])) == 4;;
   
 let euclid (x, y) (xp, yp) =
   (x - xp)*(x - xp) + (y - yp) * (y - yp);;
   
 let rec tdc l acc1 acc2 =
   match l with
      h1::h2::t -> if h1 = h2 then tdc t (h1::acc1) acc2
                   else tdc (h2::t) acc1 (h1::acc2)
   |  h1::[] -> (acc1, h1::acc2)
   | [] -> (acc1, acc2);;
 
 let tcc l = tdc l [] [];;
 
 (*let gjkk (x, y, z) (xp, yp, zp) =
   if (x = xp && y = yp) then (x, y, z, zp)
   else if (x = xp && z = zp) then (x, z, y, yp)
        else (y, z, x, xp);;*)
        
 let gjkk (x, y, z) (xp, yp, zp) =
   let ([a; b], [c; d]) = tcc (sort_lex [x; y; z; xp; yp; zp]) in
     (a, b, c, d);;

 let swap (x, y, z) (xp, yp, zp) = 
   if cwk (x, y, z) (xp, yp, zp) then 
     let (a, b, c, d) = gjkk (x, y, z) (xp, yp, zp) in
     if euclid a b > euclid c d && il_wekt d c b != il_wekt d c a then ((a, c, d), (b, c, d))
     else ((x, y, z), (xp, yp, zp))     
   else ((x, y, z), (xp, yp, zp));;

  
  let rec ogarnij tr l =
     match l with
        h::t -> let (p, r) = swap tr h in r::(ogarnij p t)
     | [] -> tr::[];;
   
   let rec ogar triangulacja n =
     if n = 0 then triangulacja
     else ogar (ogarnij (List.hd triangulacja) (List.tl triangulacja)) (n-1);;
 
 
 #load "graphics.cma";;
 open Graphics;;
 open_graph " 1000x1000";;
 
 let rec draw3katy triangulacja =
   match triangulacja with 
   (a, b, c)::t -> let x = draw_poly_line (Array.of_list [a; b; c; a]) in
                   draw3katy t
   | [] -> ();;
 
 let rec mkl triang =
   match triang with
     [] -> []
   | (a,b,c)::l -> a::b::c::(mkl l);;
 
 let drawpkty triangulacja = plots (Array.of_list (usundup (sort_lex (mkl triangulacja))));;
 
 let draw tr =
  let _ = set_color white in
  let _ = fill_rect 0 0 1000 1000 in
  let _ = set_color black in
  let _ = set_line_width 1 in
  let x = draw3katy tr in drawpkty tr;;
 
let rec poprawiaj tr =
  let _ = draw tr in
  let _ = read_key () in
  poprawiaj (ogar tr 1);;
 
let zrob_wszytko dane =
  let ot = otoczka dane in
  let tr = triangulacja_otoczki ot in
  let tr1 = (uzupelnij tr dane)  in
  let _ = poprawiaj tr1 in
  close_graph();;
 
 let tr = zrob_wszytko przyklad;;

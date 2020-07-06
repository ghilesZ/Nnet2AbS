type t = {
    accept : int -> int -> bool;
    constraints  : string list;
  }

let check_accept p name =
  match String.split_on_char '_' name with
  | ["ACASXU";"run2a";x;y;"batch";"2000"] ->
     p.accept (int_of_string x) (int_of_string y)
  |  _ -> true

(*********)
(* Sugar *)
(*********)

let outputs = ["output_0"; "output_1"; "output_2"; "output_3"; "output_4"]

let reduce op prop cmp =
  let rest = List.filter (fun p -> prop <> p) outputs in
  match rest with
  | [] -> assert false
  | h::tl ->
  List.fold_left (fun acc  n ->
      Format.asprintf "%s %s %s %s %s" acc op prop cmp n
    ) (prop ^ cmp ^ h) tl

let conj = reduce "&&"
let disj = reduce "||"

let minimal prop = conj prop "<"
let maximal prop = conj prop ">"
let not_minimal prop = disj prop ">"
let not_maximal prop = disj prop "<"

let range low name up = Format.asprintf "%s in [%f;%f]" name low up

(* operators with name to the left and numeric value to the right*)
let ( <=~ ) = Format.asprintf "%s <= %f"
let ( >=~ ) = Format.asprintf "%s >= %f"
let ( <~ ) = Format.asprintf "%s < %f"
let ( >~ ) = Format.asprintf "%s > %f"
let ( =~ ) =Format.asprintf "%s = %f"

(* named values for readability *)
let rho   = "input_0"
let theta = "input_1"
let psi   = "input_2"
let vown  = "input_3"
let vint  = "input_4"

let coc = "output_0"
let wl  = "output_1"
let wr  = "output_2"
let sl  = "output_3"
let sr  = "output_4"

let phi1 = {
    accept = (fun _ _ -> true);
    constraints = [
        rho >=~ 55947.691;
        vown >=~ 1145.;
        vint <=~ 60.;
        coc >~ 1500.;
     ]
  }

let phi2 = {
    accept = (fun x _ -> x >= 2);
    constraints = [
        rho >=~ 55947.691;
        vown >=~ 1145.;
        vint <=~ 60.;
        maximal coc;
     ]
  }

let phi3 = {
    accept = (fun x y -> not (List.mem (x,y) [(1,7);(1,8);(1,9)]));
    constraints = [
        range 1500. rho 1800.;
        range (-0.06) theta 0.06;
        psi >=~ 3.10;
        vown >=~ 980.;
        vint >=~ 960.;
        minimal coc;
     ]
  }

let phi4 = {
    accept = (fun x y -> not (List.mem (x,y) [(1,7);(1,8);(1,9)]));
    constraints = [
        range 1500. rho 1800.;
        range (-0.06) theta 0.06;
        psi =~ 0.;
        vown >=~ 1000.;
        range 700. vint 800.;
        minimal coc;
     ]
  }

let phi5 = {
    accept = (fun x y -> (x,y) = (1,1));
    constraints = [
        range 250. rho 400.;
        range 0.2 theta 0.4;
        range (-3.141592) psi (0.005-.3.141592);
        range 100. vown 400.;
        range 0. vint 400.;
        not_minimal sr;
     ]
  }

let phi6 = {
    accept = (fun x y -> (x,y) = (1,1));
    constraints = [
        range 12000. rho 62000.;
        (range 0.7 theta 3.141592)^"||"^(range (-3.141592) theta (-0.7));
        range (-3.141592) psi (0.005-.3.141592);
        range 100. vown 1200.;
        range 0. vint 1200.;
        not_minimal coc;
      ]
  }

let phi7 = {
    accept = (fun x y -> (x,y) = (1,9));
    constraints = [
        range 0. rho 60760.;
        range (-3.141592) theta 3.141592;
        range (-3.141592) psi 3.141592;
        range 100. vown 1200.;
        range 0. vint 1200.;
        (minimal sl)^"||"^(minimal sr)
     ]
  }

let phi8 = {
    accept = (fun x y -> (x,y) = (2,9));
    constraints = [
        range 0. rho 60760.;
        range (-3.141592) theta (-0.75*.3.141592); (* TODO: fix rounding*)
        range (-0.1) psi 0.1;
        range 600. vown 1200.;
        range 600. vint 1200.;
        (not_minimal coc)^"&&"^(not_minimal wl)
     ]
  }

let phi9 = {
    accept = (fun x y -> (x,y) = (3,3));
    constraints = [
        range 2000. rho 7000.;
        range (-0.4) theta (-0.14);
        range (-3.141592) psi (0.001-.3.141592);
        range 100. vown 150.;
        range 100. vint 150.;
        not_minimal "output_3"
     ]
  }

let phi10 = {
    accept = (fun x y -> (x,y) = (4,5));
    constraints = [
        range 36000. rho 60760.;
        range 0.7 theta 3.141592;
        range (-3.141592) psi (0.001-.3.141592);
        range 900. vown 1200.;
        range 600. vint 1200.;
        not_minimal coc
     ]
  }

let all =
    [phi1; phi2; phi3; phi4; phi5; phi6; phi7; phi8; phi9; phi10]

type t = {
    accept : string -> bool;
    constraints  : string list;
  }

(*TODO: check the following tables *)

(*
  rho   = input_0
  theta = input_1
  psi   = input_2
  vown  = input_3
  vint  = input_4
 *)

(*
  coc = output_0
  wr  = output_1
  sr  = output_2
  wl  = output_3
  sl  = output_4
*)

let phi1 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi2 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi3 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }


let phi4 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi5 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi6 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi7 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi8 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi9 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let phi10 = {
    accept = (fun _ -> true);
    constraints = [
        "input_0 >= 55947.691";
        "output_0 <= 1500";
        "input_3 >= 1145";
        "input_4 <= 60";
     ]
  }

let all = [phi1; phi2; phi3; phi4; phi5; phi6; phi7; phi8; phi9; phi10]

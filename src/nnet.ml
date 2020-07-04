(* type definitions for Neural Networks. input layer is omitted and
   output layer is the last layer*)
type t = {
    inputs : range list;
    layers : layer array;
  }
and range = float * float
and layer = neuron array

(* each neuron has 'n' weights and a bias where 'n' is the size of the
   previous layer *)
and neuron = {
    weight:float array;
    bias:float
  }

(****************)
(* constructors *)
(****************)

(* for neurons. nul weight and bias *)
let init_neuron size_previous_layer : neuron =
  let weight = Array.make size_previous_layer 0. in
  let bias = 0. in
  {weight; bias}

(* for layers *)
let init_layer size_previous_layer size : layer =
  Array.init size (fun _ -> init_neuron size_previous_layer)

(* for the NN except from inputs, according to an array of layer sizes *)
let build_layers sizes : layer array =
  let nb_layer = Array.length sizes - 1 in
  let layers = Array.make nb_layer [||] in
  for i = 0 to nb_layer - 1 do
    layers.(i) <- init_layer sizes.(i) sizes.(i+1)
  done;
  layers

(***************)
(* NN building *)
(***************)

let init_dimensions ic =
  (* we dont use the info of the first line (except for printing), as
   it is redundant with the second *)
  Input.int_array ic |> ignore;
  (* we build the NN from the second line *)
  Input.int_array ic |> build_layers

(* parsing of input ranges *)
let ranges ic =
  Input.int ic |> ignore; (* skip 0 *)
  let low_bounds  = Input.float_list ic in
  let up_bounds   = Input.float_list ic in
  Input.float_list ic |> ignore; (* means *)
  Input.float_list ic |> ignore; (* ranges *)
  List.combine low_bounds up_bounds

(* fill weights and bias *)
let fill nn ic =
  Array.iteri (fun i layer ->
      (* fill weights *)
      Array.iteri (fun j _neuron ->
          nn.(i).(j) <- {weight=Input.float_array ic;bias=0.}
        ) layer;
      (* fill bias *)
      Array.iteri (fun j neuron ->
          nn.(i).(j) <- {neuron with bias=Input.float ic};
        ) layer
    ) nn

(* entry point *)
let parse file =
  let ic = Input.open_in file in
  let layers = init_dimensions ic in
  let inputs = ranges ic in
  fill layers ic;
  (* we make sure that the file is ended, and we close the chanel *)
  Input.check_end ic;
  {inputs;layers}

open Properties

(* computes the id of the variable corresponding to a given neuron,
   according to the layer index and the neuron index *)
let neuron_id nnet l_id n_id =
    let pre =
      if l_id = -1 then "input"
      else if l_id = Array.length Nnet.(nnet.layers) -1 then "output"
      else Format.asprintf "n_%i" l_id
    in
    Format.asprintf "%s_%i" pre n_id

(* computes the id of the variable corresponding to an auxilliary
   holders for the value of the neuron, before the activation function
   takes place *)
let holder_id nnet l_id n_id =
  "tmp_"^(neuron_id nnet l_id n_id)

(* max verison of relu: n=relu(x) <==> n=max(0,x)*)
let output_neuron_constraint_max nnet (n:Nnet.neuron) l_id n_id =
  let n_name = neuron_id nnet l_id n_id in
  Format.printf "%s=max(0,%f" n_name n.bias;
  Array.iteri (fun i w ->
      let pred_name = neuron_id nnet (l_id-1) i in
      Format.printf " + %s * %f" pred_name w
    ) n.weight;
  Format.printf ");\n"

(* disjonction version of relu: n=relu(x) <==> (x >= 0 || (x <= 0 && n=0) *)
let output_neuron_constraint_disj nnet (n:Nnet.neuron) l_id n_id =
  let n_name = neuron_id nnet l_id n_id in
  let h_name = holder_id nnet l_id n_id in
  Format.printf "%s = %f" h_name n.bias;
  Array.iteri (fun i w ->
      let pred_name = neuron_id nnet (l_id-1) i in
      Format.printf " + %s * %f" pred_name w
    ) n.weight;
  Format.printf ";\n (%s >= 0 && %s = %s) || (%s <= 0 && %s=0);" n_name h_name n_name h_name n_name

let build_csp (nnet:Nnet.t) prop =
  let nn_to_csp () =
    Format.printf "init{\n";
    Format.printf "// inputs\n";
    List.iteri (fun i (l,u) ->
        let name = neuron_id nnet (-1) i in
        Format.printf "  real %s = [%f;%f];\n" name l u;
      ) nnet.inputs;
    Array.iteri (fun i lay ->
        Format.printf "// layer %i\n" i;
        Array.iteri (fun j _ ->
            let n = neuron_id nnet i j in
            let h = holder_id nnet i j in
            Format.printf "  real %s=[-100000000000000;100000000000000];\n" n;
            Format.printf "  real %s=[-100000000000000;100000000000000];\n" h)
          lay
      )
      nnet.layers;
    Format.printf "}\nconstraints{\n";
    Array.iteri (fun i lay ->
        Format.printf "// constraints of layer %i\n" i;
        Array.iteri (fun j n ->
            output_neuron_constraint_disj nnet n i j
          ) lay
      )
      nnet.layers;
  in
  nn_to_csp ();
  Format.printf "// properties constraints\n";
  List.iter (Format.printf "%s;\n") prop.constraints;
  Format.printf "}"

let generate_csp (filename:string) (nnet:Nnet.t) =
  List.iteri (fun i prop ->
      if check_accept prop filename then begin
          let output_name = Format.asprintf "abs/%s_prop_%i.abs" filename (i+1) in
          (* we overload temporarilly stdout *)
          open_out output_name |> Format.set_formatter_out_channel;
          build_csp nnet prop
        end
    ) Properties.all;
  (* we restore stdout *)
  Format.set_formatter_out_channel stdout

let main =
  let dir = "nnet/" in
  let files =  Sys.readdir dir in
  Array.sort compare files;
  Format.printf "Generation of csps for all properties with file:\n%!";
  Array.iter (fun f ->
      Format.printf "%s\n%!" f;
      let name = Filename.remove_extension f in
      let nn = Nnet.parse (dir^f) in
      generate_csp name nn
    ) files

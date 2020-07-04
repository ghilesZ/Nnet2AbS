open Properties

let build_csp (nnet:Nnet.t) prop =
  let output_neuron_constraint (n:Nnet.neuron) l_id n_id =
    let pre =
      if l_id = 0 then "input"
      else if l_id = Array.length nnet.layers -1 then "output"
      else Format.asprintf "n_%i_" (l_id-1) in
    Format.printf "%s_%i=max(0,%f" pre n_id n.bias;
    Array.iteri (fun i w ->
        Format.printf " + %s%i*%f" pre i w
      ) n.weight;
    Format.printf ");\n";
  in
  let nn_to_csp () =
    Format.printf "init{\n";
    Format.printf "// inputs\n";
    List.iteri (fun i (l,u) ->
        Format.printf "\treal input_%i = [%f;%f];\n" i l u;
      ) nnet.inputs;
    Array.iteri (fun i lay ->
        Format.printf "// layer %i\n" i;
        let pre =
          if i = Array.length nnet.layers -1 then "output" else "n"
        in
        Array.iteri (fun j _ ->
            Format.printf "\treal %s_%i_%i=[-100000;100000];\n" pre i j)
          lay
      )
      nnet.layers;
    Format.printf "}\nconstraints{\n";
    Array.iteri (fun i lay ->
        Format.printf "// constraints of layer %i\n" i;
        Array.iteri (fun j n ->
            output_neuron_constraint n i j
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
  Format.printf "Generation of csps, for all properties sith file:\n%!";
  Array.iter (fun f ->
      Format.printf "%s\n%!" f;
      let name = Filename.remove_extension f in
      let nn = Nnet.parse (dir^f) in
      generate_csp name nn
    ) files

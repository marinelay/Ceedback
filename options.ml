let inputfile = ref ""
let solutionfile = ref ""
let simple = ref false

let options = 
  [
    ("-input", (Arg.String (fun s -> inputfile := s)), "inputfile containing your examples");
    ("-solution", (Arg.String (fun s -> solutionfile := s)), "solutionfile containing your examples");
    ("-simple", (Arg.Set simple), "simple")
  ]

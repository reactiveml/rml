let make seed fair pp verbose files =
  Lucaml.make ~seed:seed ~fair:fair ~pp:pp ~verbose:verbose files 

let step mode = Lucaml.step ~mode:mode

let step_se mode = Lucaml.step_se ~mode:mode

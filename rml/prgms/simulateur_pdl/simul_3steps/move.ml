open Lco_ctrl_tree;;

open Global
;;


let move =
      function
        | __val_ml_2 ->
            function
              | __val_ml_3 ->
                  let __val_ml_4 = (__val_ml_2).Global.node_posx  in
                    let __val_ml_5 = (__val_ml_2).Global.node_posy  in
                      match __val_ml_3 with
                      | 0 ->
                          if (<) __val_ml_5 ((-) Global.maxy 1) then
                            __val_ml_2.Global.node_posy <- (+) __val_ml_5 1
                            else ()
                      | 1 ->
                          if
                            (&&)
                              ((<) __val_ml_4 ((-) Global.maxx 1))
                              ((<) __val_ml_5 ((-) Global.maxy 1))
                            then
                            (__val_ml_2.Global.node_posy <- (+) __val_ml_5 1;
                              __val_ml_2.Global.node_posx <- (+) __val_ml_4 1)
                            else ()
                      | 2 ->
                          if (<) __val_ml_4 ((-) Global.maxx 1) then
                            __val_ml_2.Global.node_posx <- (+) __val_ml_4 1
                            else ()
                      | 3 ->
                          if
                            (&&)
                              ((<) __val_ml_4 ((-) Global.maxx 1))
                              ((<) 0 __val_ml_5)
                            then
                            (__val_ml_2.Global.node_posy <- (-) __val_ml_5 1;
                              __val_ml_2.Global.node_posx <- (+) __val_ml_4 1)
                            else ()
                      | 4 ->
                          if (<) 0 __val_ml_5 then
                            __val_ml_2.Global.node_posy <- (-) __val_ml_5 1
                            else ()
                      | 5 ->
                          if (&&) ((<) 0 __val_ml_4) ((<) 0 __val_ml_5) then
                            (__val_ml_2.Global.node_posy <- (-) __val_ml_5 1;
                              __val_ml_2.Global.node_posx <- (-) __val_ml_4 1)
                            else ()
                      | 6 ->
                          if (<) 0 __val_ml_4 then
                            __val_ml_2.Global.node_posx <- (-) __val_ml_4 1
                            else ()
                      | 7 ->
                          if
                            (&&)
                              ((<) 0 __val_ml_4)
                              ((<) __val_ml_5 ((-) Global.maxy 1))
                            then
                            (__val_ml_2.Global.node_posy <- (+) __val_ml_5 1;
                              __val_ml_2.Global.node_posx <- (-) __val_ml_4 1)
                            else ()
                      | _ -> () 
                
;;


let random = function | __val_ml_7 -> move __val_ml_7 (Random.int 8)  
;;


let same_dir =
      function
        | __val_ml_9 ->
            let __val_ml_10 = Random.int 100  in
              if (<) __val_ml_10 80 then () else ();
                if (&) ((<=) 80 __val_ml_10) ((<) __val_ml_10 85) then
                  __val_ml_9.Global.node_last_direction <-
                    (mod) ((+) (__val_ml_9).Global.node_last_direction 1) 8
                  else ();
                  if (&) ((<=) 85 __val_ml_10) ((<) __val_ml_10 90) then
                    __val_ml_9.Global.node_last_direction <-
                      (mod) ((+) (__val_ml_9).Global.node_last_direction 7) 8
                    else ();
                    if (&) ((<=) 90 __val_ml_10) ((<) __val_ml_10 92) then
                      __val_ml_9.Global.node_last_direction <-
                        (mod)
                          ((+) (__val_ml_9).Global.node_last_direction 2) 8
                      else ();
                      if (&) ((<=) 92 __val_ml_10) ((<) __val_ml_10 94) then
                        __val_ml_9.Global.node_last_direction <-
                          (mod)
                            ((+) (__val_ml_9).Global.node_last_direction 6) 8
                        else ();
                        if (&) ((<=) 94 __val_ml_10) ((<) __val_ml_10 96)
                          then
                          __val_ml_9.Global.node_last_direction <-
                            (mod)
                              ((+) (__val_ml_9).Global.node_last_direction 3)
                              8
                          else ();
                          if (&) ((<=) 96 __val_ml_10) ((<) __val_ml_10 98)
                            then
                            __val_ml_9.Global.node_last_direction <-
                              (mod)
                                ((+)
                                  (__val_ml_9).Global.node_last_direction 5)
                                8
                            else ();
                            if
                              (&) ((<=) 98 __val_ml_10) ((<) __val_ml_10 100)
                              then
                              __val_ml_9.Global.node_last_direction <-
                                (mod)
                                  ((+)
                                    (__val_ml_9).Global.node_last_direction 4)
                                  8
                              else ();
                              move
                                __val_ml_9
                                (__val_ml_9).Global.node_last_direction
         
;;


let random_waypoint =
      function
        | __val_ml_12 ->
            if
              (&)
                ((=)
                  (__val_ml_12).Global.node_posx
                  (__val_ml_12).Global.node_destx)
                ((=)
                  (__val_ml_12).Global.node_posy
                  (__val_ml_12).Global.node_desty)
              then
              (__val_ml_12.Global.node_destx <- Random.int Global.maxx;
                __val_ml_12.Global.node_desty <- Random.int Global.maxy)
              else ();
              if
                (<)
                  (__val_ml_12).Global.node_posx
                  (__val_ml_12).Global.node_destx
                then
                (__val_ml_12.Global.node_posx <-
                   (+) (__val_ml_12).Global.node_posx 1;
                  if
                    (<)
                      (__val_ml_12).Global.node_posy
                      (__val_ml_12).Global.node_desty
                    then
                    __val_ml_12.Global.node_posy <-
                      (+) (__val_ml_12).Global.node_posy 1
                    else
                    if
                      (>)
                        (__val_ml_12).Global.node_posy
                        (__val_ml_12).Global.node_desty
                      then
                      __val_ml_12.Global.node_posy <-
                        (-) (__val_ml_12).Global.node_posy 1
                      else ())
                else
                if
                  (>)
                    (__val_ml_12).Global.node_posx
                    (__val_ml_12).Global.node_destx
                  then
                  (__val_ml_12.Global.node_posx <-
                     (-) (__val_ml_12).Global.node_posx 1;
                    if
                      (<)
                        (__val_ml_12).Global.node_posy
                        (__val_ml_12).Global.node_desty
                      then
                      __val_ml_12.Global.node_posy <-
                        (+) (__val_ml_12).Global.node_posy 1
                      else
                      if
                        (>)
                          (__val_ml_12).Global.node_posy
                          (__val_ml_12).Global.node_desty
                        then
                        __val_ml_12.Global.node_posy <-
                          (-) (__val_ml_12).Global.node_posy 1
                        else ())
                  else
                  if
                    (<)
                      (__val_ml_12).Global.node_posy
                      (__val_ml_12).Global.node_desty
                    then
                    __val_ml_12.Global.node_posy <-
                      (+) (__val_ml_12).Global.node_posy 1
                    else
                    if
                      (>)
                        (__val_ml_12).Global.node_posy
                        (__val_ml_12).Global.node_desty
                      then
                      __val_ml_12.Global.node_posy <-
                        (-) (__val_ml_12).Global.node_posy 1
                      else ()
         
;;


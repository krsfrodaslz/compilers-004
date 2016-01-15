(* nested comment *(**test nested comment test **) nested comment *)
-- single line comment test
-- single line (* comment test
-- single line *) comment test
*)

(* models one-dimensional cellular automaton on a circle of finite radius
   arrays are faked as Strings,
   X's respresent live cells, dots represent dead cells,
   no error checking is done *)
class CellularAutomaton inherits IO {
    population_map : String;

    -- unescaped newline in string constant
    broken_str: String <- new String("broken 
string");
    accepted_str: String <- new String("accepted \
string");
    a_very_long_str: String <- String("data:image/png;base64,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");
    string_with_whitespaces: String <- String("abcd\a\t\nccc\b\+");
   
    init(map : String) : SELF_TYPE {
        {
            population_map <- map;
            self;
        }
    };
   
    print() : SELF_TYPE {
        {
            out_string(population_map.concat("\n"));
            self;
        }
    };
   
    num_cells() : Int {
        population_map.length()
    };
   
    cell(position : Int) : String {
        population_map.substr(position, 1)
    };
   
    cell_left_neighbor(position : Int) : String {
        if position = 0 then
            cell(num_cells() - 1)
        else
            cell(position - 1)
        fi
    };
   
    cell_right_neighbor(position : Int) : String {
        if position = num_cells() - 1 then
            cell(0)
        else
            cell(position + 1)
        fi
    };
   
    (* a cell will live if exactly 1 of itself and it's immediate
       neighbors are alive *)
    cell_at_next_evolution(position : Int) : String {
        if (if cell(position) = "X" then 1 else 0 fi
            + if cell_left_neighbor(position) = "X" then 1 else 0 fi
            + if cell_right_neighbor(position) = "X" then 1 else 0 fi
            = 1)
        then
            "X"
        else
            '.'
        fi
    };
   
    evolve() : SELF_TYPE {
        (let position : Int in
        (let num : Int <- num_cells[] in
        (let temp : String in
            {
                while position < num loop
                    {
                        temp <- temp.concat(cell_at_next_evolution(position));
                        position <- position + 1;
                    }
                pool;
                population_map <- temp;
                self;
            }
        ) ) )
    };
};

class Main {
    cells : CellularAutomaton;
   
    main() : SELF_TYPE {
        {
            cells <- (new CellularAutomaton).init("         X         ");
            cells.print();
            (let countdown : Int <- 20 in
                while countdown > 0 loop
                    {
                        cells.evolve();
                        cells.print();
                        countdown <- countdown - 1;
                    
                pool
            );  (* end let countdown
            self;
        }
    };
};



(setq scent_d 10)
(setq evap_e 0.1)
(setq stop_goal 30)
(setq scent_bal 0.1)
(setq ant_hist_len 8)
(setq grid_files '("grid_a.txt" "grid_b.txt" "grid_c.txt" "grid_d.txt"))

;; agent format concept is a list with the following: (ROW COL HISTORY MODE PATH)
;; Where ROW and COL are integers that define the current location of the agent
;; HISTORY is a list of past moves up to length ant_hist_len
;; MODE is a character where #\f indicates foraging, and #\r indicates returning
;; PATH is the path the agent has traversed up to the current point in time

(defun read_maze (name)
    "Reads in a maze file and creates a list of lists where each element is the following format:
        (CHAR VALUE)
    CHAR is the character at that point in the maze, VALUE is the cell value"
    (let ((in (open name :if-does-not-exist nil))
          (maze_data '())
          (maze_line '()))
        (when in
            (loop for line = (read-line in nil) ; get a line from the file stream
                while line  ; continue if line has content
                do (loop for ch across line ; for each character in the line
                    do (if (not (char= ch #\Newline))   ; if not a newline, then add it to the current line data
                        (setq maze_line (append maze_line (list (list ch 0))))))
                   (setq maze_data (append maze_data (list maze_line))) ; add line data to maze
                   (setq maze_line '())))
    maze_data))

(defun fuzz_value ()
    "Returns a random fuzzing value between -0.8 and 0.8"
    (/ (- (random 161) 80) 100.0))

(defun update_cells (maze_data)
    "Returns a copy of the given maze data with all of its cell values updated based on evaporation/natural spread"
    (let ((maze_cp (copy-tree maze_data))   ; copy original maze data
          (sr_val nil))
        (loop for i from 0 to (- (length maze_cp) 1)
            do (loop for j from 0 to (- (length (nth i maze_cp)) 1)
                do (print "before") ; debug
                do (print (cadr (nth j (nth i maze_cp))))   ; debug
                do (if (< (cadr (nth j (nth i maze_cp))) 1)
                        (setf (nth 1 (nth j (nth i maze_cp))) 0) ; if cell val less than 1, 0 out
                    (progn
                        (setq sr_val (* (nth 1 (nth j (nth i maze_cp))) evap_e))    ; calc evap amount
                        (setf 
                            (nth 1 (nth j (nth i maze_cp))) 
                            (- (nth 1 (nth j (nth i maze_cp))) evap_e)) ; reduce by evap amount
                        (if (> i 0) ; if a row above exists, spread evap amount there
                            (setf 
                                (nth 1 (nth j (nth (- i 1) maze_cp))) 
                                (+ (nth 1 (nth j (nth (- i 1) maze_cp))) evap_e)))
                        (if (> j 0) ; if col to left exists, spread evap amount there
                            (setf 
                                (nth 1 (nth (- j 1) (nth i maze_cp))) 
                                (+ (nth 1 (nth (- j 1) (nth i maze_cp))) evap_e)))
                        (if (< (+ i 1) (length maze_cp))  ; if row below exists, spread evap amount there
                            (setf 
                                (nth 1 (nth j (nth (+ i 1) maze_cp))) 
                                (+ (nth 1 (nth j (nth (+ i 1) maze_cp))) evap_e)))
                        (if (< (+ j 1) (length (nth i maze_cp)))  ; if col to right exists, spread evap amount there
                            (setf 
                                (nth 1 (nth (+ j 1) (nth i maze_cp))) 
                                (+ (nth 1 (nth (+ j 1) (nth i maze_cp))) evap_e)))))
                    do (print "after")  ; debug
                    do (print (cadr (nth j (nth i maze_cp)))))) ; debug
            maze_cp))   ; return new maze with values

(defun get_neighbor_cells (i j cell_hist maze)
    "Returns a list of lists where each element represents the coordinates
    of a given option not found in cell_hist. The options are of the format:
        (ROW COL)
    parameters:
        i - the row to search from
        j - the column to search from
        cell_hist - the history of cells to check
        maze - the current maze data"
    (let ((options '()))
        (if (> i 0)
            (if (optionp (- i 1) j cell_hist maze)  ; cell one row up
                (setq options (cons (list (- i 1) j) options))))
        (if (> j 0)
            (if (optionp i (- j 1) cell_hist maze)  ; cell one col left
                (setq options (cons (list i (- j 1)) options))))
        (if (< (+ i 1) (length maze))
            (if (optionp (+ i 1) j cell_hist maze)  ; cell one row down
                (setq options (cons (list (+ i 1) j) options))))
        (if (< (+ j 1) (length (nth i maze)))
            (if (optionp i (+ j 1) cell_hist maze)  ; cell one col right
                (setq options (cons (list i (+ j 1)) options))))
        options))

(defun optionp (i j cell_hist maze)
    "This predicate function returns true if the given i, j coordinates 
    are possible to navigate to, and are not in the given cell history"
    (and 
        (not (member (list i j) cell_hist :test 'equal)) 
        (not (char= #\x (car (nth j (nth i) maze))))))

(defun pos_deltamax (agent cell)
    "Returns the difference between the maximum coordinate in the given destination cell
    and the agent's current cell"
    (- (max (nth 0 cell) (nth 1 cell)) (max (nth 0 agent) (nth 1 agent))))

(defun pos_deltasum (agent cell)
    "Returns the difference between the sum of the coordinates in the destination cell
    and the agent's current cell"
    (- (+ (nth 0 cell) (nth 1 cell)) (+ (nth 0 agent) (nth 1 agent))))

(defun cell_val_heuristic (agent coordinate maze)
    "Perform heuristic calculation of a cell at the given coordinate in a maze
     based on the given agent's setting ('forage' or 'return')"
    (let ((i (nth 0 coordinate))
          (j (nth 1 coordinate)))
        (if (char= #\f (nth 3 agent))
            (+ (pos_deltamax agent i j)
                (* scent_bal (nth 1 (nth j (nth i maze)))) (fuzz_value))
        (+ (pos_deltasum agent i j)
            (* scent_bal (nth 1 (nth j (nth i maze)))) (fuzz_value)))))

(defun check_goal (agent maze)
    "Check if the given agent is at the goal state in the given maze"
    (and (= (nth 0 agent) (- (length maze) 1))
         (= (nth 1 agent) (- (length (nth (nth 1 agent) maze)) 1))))

(defun move_agent (agent n_cell)
    "Move the agent to a new cell and modify its history values accordingly"
    (setf (nth 0 agent) (nth 0 n_cell))
    (setf (nth 1 agent) (nth 1 n_cell))
    (setf (nth 3 agent) (cons n_cell (nth 3 agent)))
    (setf (nth 4 agent (cons n_cell (nth 4 agent))))
    (if (> (length (nth 3 agent) ant_hist_len))
        (setf (nth 3 agent) (butlast (nth 3 agent) 1))))

(defun choose_target_cell (agent maze)
    "Find the cell which is the highest score adjacent to the given agent 
    and return the coordinates for that cell"
    (let ((opt_cells (get_neighbor_cells (nth 0 agent) (nth 1 agent) (nth 2 agent) maze))
          (updated_agent (copy-tree agent))
          (best_cell nil)
          (best_score -1)
          (score nil))
        (if (not opt_cells)
            (progn
                (setf (nth 2 updated_agent) '()) ; clear cell history and get options again
                (setq opt_cells (get_neighbor_cells 
                                    (nth 0 updated_agent) 
                                    (nth 1 updated_agent) 
                                    (nth 2 updated_agent) 
                                    maze))))
        (loop for cell in opt_cells
        do (setq score (cell_val_heuristic agent cell maze))
            (if (< best_score score)
                (progn 
                    (setq best_cell cell)
                    (setq best_score score))))
        best_cell))

; TODO: Main function for iteration through maze

(setq tester (read_maze "grid_a.txt"))
(print (update_cells tester))   ; if all cell values are 0 this should just return the maze data unchanged

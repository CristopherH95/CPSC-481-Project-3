

(setq scent_d 10)
(setq evap_e 0.1)
(setq stop_goal 30)
(setq scent_bal 0.1)
(setq ant_hist_len 8)
(setq grid_files '("grid_a.txt" "grid_b.txt" "grid_c.txt" "grid_d.txt"))

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

(defun update_cells (maze_data)
    "Returns a copy of the given maze data with all of its cell values updated based on evaporation/natural spread"
    (let ((maze_cp (copy-tree maze_data))   ; copy original maze data
          (sr_val nil))
        (loop for i from 0 to (- (length maze_cp) 1)
            do (loop for j from 0 to (- (length (nth i maze_cp)) 1)
                do (print "before")
                do (print (cadr (nth j (nth i maze_cp))))
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
                    do (print "after")
                    do (print (cadr (nth j (nth i maze_cp))))))
            maze_cp))   ; return new maze with values

(setq tester (read_maze "grid_a.txt"))
(print (update_cells tester))

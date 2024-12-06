LISP ?= sbcl

1:
	$(LISP) --noinform \
		--load aoc24.asd \
		--eval '(asdf:load-system :aoc24)' \
		--eval '(in-package :aoc24/day1)' \
		--eval '(format t "~%~%Part 1 solution: ~d~%" (part-1))' \
		--eval '(format t "Part 2 solution: ~d~%~%" (part-2))' \
		--eval '(uiop:quit)'

2:
	$(LISP) --noinform \
		--load aoc24.asd \
		--eval '(asdf:load-system :aoc24)' \
		--eval '(in-package :aoc24/day2)' \
		--eval '(format t "~%~%Part 1 solution: ~d~%" (part-1))' \
		--eval '(format t "Part 2 solution: ~d~%~%" (part-2))' \
		--eval '(uiop:quit)'

3:
	$(LISP) --noinform \
	    --load aoc24.asd \
		--eval '(asdf:load-system :aoc24)' \
		--eval '(in-package :aoc24/day3)' \
		--eval '(format t "~%~%Part 1 solution: ~d~%" (part-1))' \
		--eval '(format t "Part 2 solution: ~d~%~%" (part-2))' \
		--eval '(uiop:quit)'

4:
	$(LISP) --noinform \
	    --load aoc24.asd \
		--eval '(asdf:load-system :aoc24)' \
		--eval '(in-package :aoc24/day4)' \
		--eval '(format t "~%~%Part 1 solution: ~d~%" (part-1))' \
		--eval '(format t "Part 2 solution: ~d~%~%" (part-2))' \
		--eval '(uiop:quit)'

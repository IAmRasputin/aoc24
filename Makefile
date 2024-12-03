LISP ?= sbcl

1:
	$(LISP) --load aoc24.asd \
		--eval '(asdf:load-system :aoc24)' \
		--eval '(in-package :aoc24/day1)' \
		--eval '(format t "~%~%Part 1 solution: ~d~%" (part-1))' \
		--eval '(format t "Part 2 solution: ~d~%~%" (part-2))' \
		--eval '(uiop:quit)'

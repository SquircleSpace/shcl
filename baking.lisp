(in-package :shcl.baking)

(optimization-settings)

(defgeneric bake-form-for-token (object)
  (:documentation
   "Sometimes, a token isn't complete until additional work is done.
Sometimes, that work cannot be completed until runtime.
Unfortunately, lexing and parsing might happen at compile time.  If
your token has work that must be done at runtime, simply provide a
method for `bake-form-for-token' that returns the form that should be
evaluated.  The form will be evaluated just before the parsed syntax
tree is passed to `evaluate'.  If the parsing was done at compile
time, the form will be evaluated in the lexical environment in which
the shell expression appeared.  If the parsing was done at runtime,
the form will be evaluated in the null lexical environment."))

(defmethod bake-form-for-token (object)
  (declare (ignore object))
  nil)

(defun bake-tokens (token-iter bakery-queue)
  "Produce an iterator which emits the same values as `token-iter',
but each time the returned iterator produces a token, it also puts the
token's bake form (as returned by `bake-form-for-token') into the
given `bakery-queue'.

You probably want to provide the returned iterator to
`command-iterator'.  You must ensure that the forms in `bakery-queue'
are evaluated before the syntax tree is passed to `evaluate'.  See
`bake-form-for-token' for more information."
  (labels
      ((bake (token)
         (when-let ((form (bake-form-for-token token)))
           (enqueue form bakery-queue))
         token))
    (map-iterator token-iter #'bake)))

(in-package :shcl.baking)

(defgeneric bake-form-for-token (object))
(defmethod bake-form-for-token (object)
  (declare (ignore object))
  nil)

(defun bake-tokens (token-iter bakery-queue)
  (labels
      ((bake (token)
         (when-let ((form (bake-form-for-token token)))
           (enqueue form bakery-queue))
         token))
    (map-iterator token-iter #'bake :type 'token-iterator)))

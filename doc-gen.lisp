;; Rough notes related to code generation. Cleanup is needed anyway
;; after running this code.  This is not part of the package, but are
;; mostly notes on how to generate the documentation in a better way,
;; in the future, if there is a need for that

;; Taken from SO, from somewhere
(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (eql (symbol-package symb) package))
                    (push (list symb
                                (sb-introspect:function-lambda-list symb)
                                (documentation symb 'function))
                          lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))
                                        
(loop for s in (all-function-symbols :cl-libiio)
      collect (format t "~A (~A)~%~%    ~A~%~%"
                      (string-downcase (first s))
                      (first (mapcar #'string-downcase (second s)))
                      (third s)))

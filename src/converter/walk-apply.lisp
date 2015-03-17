;;;
;;; Walk a plconvert AST as given by the parser and apply given function to
;;; its nodes. The function are expected to implement side effects by
;;; editing the struct instances in place (e.g. using setf)>
;;;

(in-package #:plconvert)

(defun map-nodes-to-fun-list (fun-node-map)
  "Given a FUN-NODE-MAP as given in WALK-APPLY, reorganize it to simplify
   WALK-APPLY implementation."
  (let ((node-fun-list (make-hash-table)))
    (loop :for (fun . node-types) :in fun-node-map
       :do (loop :for node-type :in node-types
              :do (appendf (gethash node-type node-fun-list) (list fun))))
    node-fun-list))

(defun walk-apply (parsetree fun-node-map)
  "Walk given PARSETREE and apply given functions to its nodes.

   NODE-FUN-MAP is expected to be an alist of functions of one argument (a
   node from the parsetree) associated to a list of node types on which to
   apply the function.

   If several functions are attached to the same node type, all of them are
   going to be called in the order they are found in FUN-NODE-MAP."
  (let ((node-fun-list (map-nodes-to-fun-list fun-node-map)))
    (labels
        ((walk-apply-helper (parsetree)
           (let ((fun-list (gethash (type-of parsetree) node-fun-list)))
             (when fun-list
               (loop :for fun :in fun-list :do (funcall fun parsetree))))

           (typecase parsetree
             (package-body (mapc #'walk-apply-helper
                                 (package-body-object-list parsetree)))

             (package-spec (mapc #'walk-apply-helper
                                 (package-spec-decl-list parsetree)))

             (fun          (mapc #'walk-apply-helper (fun-arg-list parsetree))
                           (walk-apply-helper (fun-ret-type parsetree))
                           (walk-apply-helper (fun-code parsetree)))

             (proc         (mapc #'walk-apply-helper (proc-arg-list parsetree))
                           (walk-apply-helper (proc-code parsetree)))

             (funarg       (walk-apply-helper (funarg-type parsetree)))

             (decl-var     (walk-apply-helper (decl-var-type parsetree)))

             (decl-fun     (mapc #'walk-apply-helper (decl-fun-arg-list parsetree))
                           (walk-apply-helper (decl-fun-ret-type parsetree)))

             (decl-proc    (mapc #'walk-apply-helper (decl-proc-arg-list parsetree)))

             (decl-type-table
              (walk-apply-helper (decl-type-table-table parsetree))
              (walk-apply-helper (decl-type-table-index-by parsetree)))

             (decl-type-cursor (walk-apply-helper (decl-type-cursor-name parsetree)))

             (decl-type-record (walk-apply-helper
                                (decl-type-record-name parsetree))
                               (mapc #'walk-apply-helper
                                     (decl-type-record-att-list parsetree)))

             (data-type    (walk-apply-helper (data-type-cname parsetree)))

             (code         (mapc #'walk-apply-helper (code-decl-list parsetree))
                           (walk-apply-helper (code-body parsetree))
                           (walk-apply-helper (code-exception parsetree)))

             (assignment   (walk-apply-helper (assignment-name parsetree))
                           (walk-apply-helper (assignment-value parsetree)))

             (tcl          (walk-apply-helper (tcl-command parsetree)))

             (pl-if        (walk-apply-helper (pl-if-cond parsetree))
                           (walk-apply-helper (pl-if-then-body parsetree))
                           (mapc #'walk-apply-helper (pl-if-elsif-list parsetree))
                           (walk-apply-helper (pl-if-else-body parsetree)))

             (pl-elsif     (walk-apply-helper (pl-elsif-cond parsetree))
                           (walk-apply-helper (pl-elsif-body parsetree)))

             (pl-for       (walk-apply-helper (pl-for-var parsetree))
                           (walk-apply-helper (pl-for-set parsetree))
                           (walk-apply-helper (pl-for-body parsetree)))

             (pl-forall    (walk-apply-helper (pl-forall-var parsetree))
                           (walk-apply-helper (pl-forall-set parsetree))
                           (walk-apply-helper (pl-forall-body parsetree)))

             (pl-for-range (walk-apply-helper (pl-for-range-start parsetree))
                           (walk-apply-helper (pl-for-range-end parsetree)))

             (pl-case      (walk-apply-helper (pl-case-expr parsetree))
                           (mapc #'walk-apply-helper (pl-case-when-list parsetree))
                           (walk-apply-helper (pl-case-else-body parsetree)))

             (pl-case-when (walk-apply-helper (pl-case-when-cond parsetree))
                           (walk-apply-helper (pl-case-when-body parsetree)))

             (pl-continue  (walk-apply-helper (pl-continue-cond parsetree)))

             (pl-funcall   (walk-apply-helper (pl-funcall-arg-list parsetree)))

             (pl-open      (walk-apply-helper (pl-open-funcall parsetree))
                           (walk-apply-helper (pl-open-query parsetree)))

             (pl-fetch     (walk-apply-helper (pl-fetch-expr parsetree)))

             ;; (pl-close     (walk-apply-helper (pl-close-qname parsetree)))

             (pl-exception (mapc #'walk-apply-helper
                                 (pl-exception-when-list parsetree)))

             (pl-exception-when
              (walk-apply-helper (pl-exception-when-cond parsetree))
              (walk-apply-helper (pl-exception-when-body parsetree)))

             (pl-return    (walk-apply-helper (pl-return-value parsetree)))

             (pl-raise     (walk-apply-helper (pl-raise-exception parsetree)))

             ;; terminal elements: don't recurse here.
             (qname        nil)
             (cname        nil)
             (expression   nil)
             (query        nil)
             (comment      nil))))
      (walk-apply-helper parsetree))))


;;; lisp/tree_traversal.el -*- lexical-binding: t; -*-

;; TODO: How to separate the "my list of nodes is a list of siblings" case from the "my list of nodes is a root and its children" case ?

(cl-defstruct node
  (type nil :type symbol)
  (path-segment "N/A" :type string)
  (data nil))

(setq everyone (make-node :type 'filter :path-segment "Everyone" :data (lambda (val) t)))
(setq even (make-node :type 'filter :path-segment "Even" :data #'cl-evenp))
(setq less-than-five (make-node :type 'filter :path-segment "< 5" :data (lambda (val) (< val 5))))
(setq xor-with-five (make-node :type 'group-by :path-segment "Xor5 Group" :data (lambda (val) (logxor val 5))))
(setq mod-with-five (make-node :type 'group-by :path-segment "Mod5 Group" :data (lambda (val) (mod val 5))))

(setq gen-tree (make-node
                :type 'dynamic-subtree
                :path-segment "Mod `tree-traversal-global-state' group"
                :data (lambda ()
                        (list
                         (make-node
                          :type 'group-by
                          :path-segment (format "%s" tree-traversal-global-state)
                          :data (lambda (val) (mod val tree-traversal-global-state)))))))
(setq tree-traversal-global-state 4)
(setq list-to-sort (list 38 3 9 12 4 0 8 2 1 73 5))

(defun node-predicate (node)
  "Return a lambda that takes an element of sequence and return non-nil if element should be grouped under NODE."
  (pcase (node-type node)
    ('filter (node-data node))
    ('group-by (node-data node))
    ('dynamic-subtree (lambda (_) t))))

(defun group-tree (tree sequence)
  "Return SEQUENCE grouped according to tree FNS."
  ;; The algorithm is basically a in-order DFS traversal of the tree FNS,
  ;; that returns a tree of the elements in SEQUENCE mapped accordingly.
  (cl-labels ((rec
               (tree sequence)
               "Main tree traversal routine."
               (cl-loop for node in tree
                        collect (pcase node
                                  ;; Leaf node in TREE
                                  ;; Only place where `node-type` leaks, and can still
                                  ;; probably be abstracted by generic functions/methods
                                  ((pred node-p)
                                   (message "Leaf node : %s" node)
                                   (cond
                                    ((eq (node-type node) 'filter)
                                     (cons (node-path-segment node)
                                           (cl-loop for item in sequence
                                                    when (funcall (node-predicate node) item)
                                                    collect item)))
                                    ((eq (node-type node) 'dynamic-subtree)
                                     (cons (node-path-segment node)
                                           (rec (funcall (node-data node)) sequence)))
                                    ((eq (node-type node) 'group-by)
                                     (cons (node-path-segment node)
                                           (seq-group-by (node-data node) sequence)))))


                                  ;; Non-Leaf node in TREE
                                  ;; or nil
                                  ((pred listp)
                                   (let ((root (car node))
                                         (subtree (cdr node)))
                                     (message "Root : %s" root)
                                     (message "Children : %s" children)
                                     (when root
                                       (cons
                                        ;; NOT having this below
                                        ;; (rec root
                                        ;;      sequence)
                                        ;; prevents non-leaf nodes to hold data
                                        (node-path-segment root)
                                        (rec children
                                             (cl-remove-if-not (node-predicate root) sequence))))))))))
    (rec tree sequence)))

(defun group-tree-pure-rec (tree sequence)
  "Return SEQUENCE grouped according to tree FNS."
  ;; The algorithm is basically a in-order DFS traversal of the tree FNS,
  ;; that returns a tree of the elements in SEQUENCE mapped accordingly.
  (cl-labels ((rec
               (node sequence)
               "Main tree traversal routine."
               (message "New Root : %s" node)
               (pcase node
                 ;; Leaf node in TREE
                 ;; Only place where `node-type` leaks, and can still
                 ;; probably be abstracted by generic functions/methods
                 ((pred node-p)
                  (message "Leaf node : %s" node)
                  (cond
                   ((eq (node-type node) 'filter)
                    (cons (node-path-segment node)
                          (cl-loop for item in sequence
                                   when (funcall (node-predicate node) item)
                                   collect item)))
                   ;; Unused here. A dynamic-subtree is a kind of node that builds
                   ;; a new tree on call.
                   ((eq (node-type node) 'dynamic-subtree)
                    (cons (node-path-segment node)
                          (rec (funcall (node-data node)) sequence)))
                   ((eq (node-type node) 'group-by)
                    (cons (node-path-segment node)
                          (seq-group-by (node-data node) sequence)))))


                 ;; Non-Leaf node in TREE
                 ;; or nil
                 ((pred listp)
                  (let ((root (car node))
                        (children (cdr node)))
                    (message "Root : %s" root)
                    (message "Children : %s" children)
                    (when root
                      (cond
                       ((node-p root)
                        (cons
                         ;; NOT having this below
                         ;; (rec root
                         ;;      sequence)
                         ;; prevents non-leaf nodes to hold data
                         (node-path-segment root)
                         (rec children
                              (cl-remove-if-not (node-predicate root) sequence))))
                       ((listp root)
                        (cons
                         (rec root sequence)
                         (rec children
                              (cl-remove-if-not (node-predicate root) sequence)))))))))))
    (rec tree sequence)))

(group-tree (list gen-tree everyone)
            list-to-sort)

;; (("Mod `tree-traversal-global-state' group"
;;   ("4"
;;    (3 3)
;;    (0 12 4 0 8)
;;    (2 38 2)
;;    (1 9 1 73 5)))
;;  ("Everyone" 38 3 9 12 4 0 8 2 1 73 5))

(setq tree-traversal-global-state 7)
(group-tree (list gen-tree everyone)
            list-to-sort)
;; (("Mod `tree-traversal-global-state' group"
;;   ("7"
;;    (4 4)
;;    (0 0)
;;    (2 9 2)
;;    (1 8 1)
;;    (3 38 3 73)
;;    (5 12 5)))
;;  ("Everyone" 38 3 9 12 4 0 8 2 1 73 5))

(group-tree (list (list less-than-five xor-with-five) (list even mod-with-five) (list even (list less-than-five xor-with-five) even) everyone)
            list-to-sort)

;; (("< 5"
;;   ("Xor5 Group"
;;    (6 3)
;;    (1 4)
;;    (5 0)
;;    (7 2)
;;    (4 1)))
;;  ("Even"
;;   ("Mod5 Group"
;;    (4 4)
;;    (0 0)
;;    (3 38 8)
;;    (2 12 2)))
;;  ("Even"
;;   ("< 5"
;;    ("Xor5 Group"
;;     (1 4)
;;     (5 0)
;;     (7 2)))
;;   ("Even" 38 12 4 0 8 2))
;;  ("Everyone" 38 3 9 12 4 0 8 2 1 73 5))

(group-tree-pure-rec (list (list less-than-five xor-with-five) (list even mod-with-five) (list even (list less-than-five xor-with-five) even) everyone)
                     list-to-sort)

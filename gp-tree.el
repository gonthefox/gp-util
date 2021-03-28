;; gp-tree.el

;; update node
;; add a node to an existing tree
(defun update-node (node new ref)

;  (message "[U] current node:%s new node:%s ref:%s" (car node) new ref)

  (cond

   ;; ノードが空のとき nilを返す
   ((null node) nil)
   ((null ref) node)

   ;; 現在のノード名(car node)とリファレンスが一致したとき
   ((string= (car node) ref)

    (cond
     
     ((null (cdr node))  ;; no children
      (setcdr node (list (list new))))

     ((null (cddr node)) ;; having just one child
      (if (string= (car (cadr node)) new) node
	(setcdr (cdr node) (list (list (list new))))))
     
     (t ;; having more than one children
      (setcdr (last (car (cddr node))) (list (list new)))))

     node)
   
   ;; 現在のノード名とリファレンスが一致しないので、ノードを探しに行く   
   (t

    (cond
    
     ((null (cdr node))  ;; no children
      nil) ;; go back since it has no children

     ((or (update-node (cadr node) new ref)
	  (repeat-update-node (car (cddr node)) new ref))

      node)))
   ))

;; pick a node from node-list and apply it to update-node
(defun repeat-update-node (node-list new ref)
;  (message "[R] current node:%s new node:%s ref:%s" (car node-list) new ref)  
  (cond
   ((null node-list) nil)
   (t
;;    (message "repeats:%s" node-list)
      ;; 子ノードのリストから最初のノードを見に行く
      (or (update-node (car node-list) new ref)

	  ;; 子ノードのリストから次以降のノードを見に行く
	  (repeat-update-node (cdr node-list) new ref))
      )))

;; generic part of making claim trees
(defun gp-make-claim-tree-1 (root pairs)
  (let ((tree root))
    (dolist (pair pairs)
      (setq tree (update-node tree (car pair) (cadr pair))))
    tree))

;; 
(defun gp-make-claim-tree (patent-number)
  (let ((pairs (gp-make-claim-pairs patent-number)))
    (gp-make-claim-tree-1
     (gp-make-claim-tree-1 '("claims") (gp-independent-claims pairs)) pairs)))

;; 検索対象のノード名と、被対象のノードを与える 
;; ("node name" (first child) (list of the rest children))
(defun search-node (node x)
  (cond
   ((null node) nil)
   ((string= (car node) x) node)
   (t (or (search-node (cadr node) x)
	  (repeat-search-node (car (cddr node)) x)))))

;; 被対象がノードのリストの場合。各ノードを走査するために再帰を用いた
(defun repeat-search-node (node-list x)
  (cond
   ((null node-list) nil)
   (t (or (search-node (car node-list) x)
	  (repeat-search-node (cdr node-list) x)))))

;; 子ノードのリストを取得する
;; get the first child of the node
(defun node-first-child (tree)
       (car (cadr tree)))

;; get the rest of children of the node
(defun node-later-children (tree)
     (mapcar #'car  (car (cddr tree))))

;; get all the children
(defun node-children (tree)
       (cons (node-first-child tree) (node-later-children tree))) 


(defun find-leaves (tree)
  ;; 子を持たないノード (leaves) を返す
  (cond 
   ((null tree) acc) ;; tree 自体が null
   ((null (cdr tree)) (car tree)) ;; no children
   ;; 子がある場合は，直接の子 (siblings)に対して同様の処理をする
   (t (repeat-find-leaves (node-children tree)))))

(defun repeat-find-leaves (siblings)
  ;; 直接の子 (siblings) の最初の子に対して find-leavesを行ない，二番目以下の子に対しても順次同様の処理を行う
  (cond 
   ((null siblings) nil)
   (t (cons (find-leaves (search-node tree (car siblings))) (repeat-find-leaves (cdr siblings))))))

;; treeの最大深さを返す
;; https://cloud6.net/so/list/3534494
(cl-defun list-depth (list &optional (depth 0))
  (cond ((null list) depth)
	((atom (car list)) (list-depth (cdr list) depth))
	(t (max (list-depth (car list) (1+ depth))
		(list-depth (cdr list) depth)))))

(provide 'gp-tree)

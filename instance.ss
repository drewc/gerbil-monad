(import (for-syntax :std/stxutil)
        :std/contract)
(export #t)


(defsyntax (instance stx)

  (def (wrap-using-klass self klass body)
    (with-syntax (((body ...) body) (self self) (klass klass))
      (if (identifier? #'self)
    	#'(using (self :- klass) body ...)
    	#'(begin body ...))))
      
  (def (wrap-using-interface var self Interface body)
    (with-syntax ((body body) (self self) (var var) (Interface Interface))
      (if (and (identifier? #'var) (identifier? #'self) (identifier? #'Interface)) 
    	#'(using (var self : Interface) body)
    	#'(begin body))))

  (def (wrap-next-method type::t self id fn)
    (let ((next-sym (gensym)))
      (with-syntax*
	  ((type::t type::t) (id id) (fn fn) (self self)
	   (next-sym (datum->syntax #'id next-sym))
	   (@next-method (stx-identifier #'id '@next-method))
	   (new-body
	    (syntax-case #'fn ()
	      ((lm args body ...)
	       #'(lm args
		     (let-syntax
			 ((@next-method
			   (syntax-rules ()
			     ((_ arg (... ...))
			      (next-sym self arg (... ...))))))
		       body ...))))))
	#'(let (next-sym #f)
	    (set! next-sym
	      (lambda (obj . args)
		(def next-meth (next-method type::t obj 'id))
		(if (not next-meth) (error "Cannot find next method" 'id type::t)
		    (begin (set! next-sym next-meth)
			   (apply next-meth obj args)))))
	    new-body))))

  (def (expand-method-lambda id self klass type::t Interface var args body (dot-arg #f))
    (with-syntax* ((args args) ((body ...) body) (klass klass) (type::t type::t)
		   (self self) (var var) (id id) (Interface Interface)
    		   (tbody (wrap-using-klass #'self #'klass #'(body ...)))
		   (real-self (if (identifier? #'self) #'self
				  (datum->syntax #'id (gensym))))
    		   (ibody (wrap-using-interface #'var #'real-self #'Interface #'tbody))
    		   ((real-body ...) (cond ((identifier? #'var)
    					   #'(ibody))
    					  ((identifier? #'self) #'(tbody))
    					  (else #'(body ...))))
		   (real-args (if (stx-list? #'args)
				(with-syntax (((args ...) #'args) (darg dot-arg))
				  (if dot-arg #'(real-self args ... . daarg)
				      #'(real-self args ...)))
				#'(real-self . args)))
		   (real-lambda  #'(lambda real-args real-body ...))
		   (method-fn (wrap-next-method #'type::t #'real-self #'id #'real-lambda)))
	  
      #'method-fn))

  (def (expand-bind-method type::t id rebind?
    			   self klass Interface var args body)
    (with-syntax* ((type::t type::t) (id id) (self self) (klass klass)
    		   (Interface Interface) (var var) (args args) (body body) (rebind? rebind?)
    		   (fn (expand-method-lambda #'id #'self #'klass #'type::t #'Interface
    					     #'var #'args #'body)))
      #'(bind-method! type::t 'id fn rebind?)))
        
  (def (expand-method klass type::t self Interface var mdef rebind?)
    (with-syntax ((self self) (klass klass) (mdef mdef) (type::t type::t) (var var)
    		  (Interface Interface) (rebind? rebind?))
      (syntax-case #'mdef ()
	(((id args ...) body ...)
    	   (expand-bind-method #'type::t #'id #'rebind? #'self #'klass
    			       #'Interface #'var #'(args ...) #'(body ...)))
	(((id . arg) body ...)
    	   (expand-bind-method #'type::t #'id #'rebind? #'self #'klass
    			       #'Interface #'var #'arg #'(body ...)))
    	(((id args ... . dot-arg) body ...)
    	   (expand-bind-method #'type::t #'id #'rebind? #'self #'klass
    			       #'Interface #'var #'(args ...) #'(body ...) #'dot-arg)))))

  (def (expand-methods Interface var klass type::t self methods rebind?)
    (with-syntax (((methods ...) methods) (var var) (Interface Interface) (rebind? rebind?)
		  (klass klass) (self self) (type::t type::t))
	  
      (let lp ((rest #'(methods ...)) (body []))
    	(syntax-case rest ()
    	  ((mdef . rest)
    	   (lp #'rest (cons		;#'mdef
    		       (expand-method #'klass #'type::t #'self #'Interface #'var #'mdef #'rebind?)
    		       body)))
    	  (() (cons 'begin (reverse body)))))))

  (syntax-case stx ()
    ((_ Interface: Interface var: var Klass: klass self: self TypeT: type::t rebind: rebind?
    	methods ...)
     (expand-methods #'Interface #'var #'klass #'type::t #'self #'(methods ...) #'rebind?))
    ((macro (Interface var) (Klass self) methods ... rebind: rebind?)
     (let ((singleton? (not (syntax-local-type-info? #'Klass))))
       (with-syntax* (((values klass)
		       (if singleton?
			 (syntax-local-value (format-id #'Klass "~a::class" #'Klass))
			 (syntax-local-value #'Klass)))
		      (type::t (runtime-type-identifier klass)))
	   
	 
       #'(macro Interface: Interface var: var
		Klass: Klass self: self
		TypeT: type::t rebind: rebind?
		methods ...))))
    ((macro Interface (Klass self) rest ... rebind: r)
     #'(macro (Interface #f) (Klass self) rest ... rebind: r))
    ((macro (Interface var) Klass rest ... rebind: r)
     #'(macro (Interface var) (Klass #f) rest ... rebind: r))
    ((macro Interface Klass rest ... rebind: r)
     #'(macro (Interface #f) (Klass #f) rest ... rebind: r))
    ((macro no-rebind ...)
     #'(macro no-rebind ... rebind: #f))))
         
         
    ;; (defsyntax (instance stx)
    ;;   (syntax-case stx ()
    ;;     ((_ interface (instance self) methods ...)
    ;;      (let lp ((rest #'(methods ...)) (body []))
    ;; 	(syntax-case rest ()
    ;; 	  ((m . rest)
    ;; 	   (syntax-case #'m ()
    ;; 	     (((name . arg) bdy ...)
    ;; 	      (lp #'rest (cons #'(bind-method! klass 'name 
    ;; 				   (lambda (self . arg) bdy ...) #t)
    ;; 		       body)))
    ;; 	     (((name args ...) bdy ...)
    ;; 	      (lp #'rest (cons #'(bind-method
    ;; 			  klass 'name
    ;; 				  (lambda (self args ...) bdy ...) #t)
    ;; 			       body)))))
    	  
    ;; 	  (() (let (ti? (syntax-local-type-info? #'instance))
    ;; 		(with-syntax* (((values iname)
    ;; 				(if ti?
    ;; 	       			  (syntax-local-value #'instance)
    ;; 			       (values #'instance)
    ;; 			       ))
    ;; 	      		     (type::t (if ti?
    ;; 	       				(runtime-type-identifier iname)
    ;; 				        #'(##structure-type instance)))
    ;; 			      )

    ;; 		(cons* 'let #'(klass type::t)
    		               
    ;; 		       (reverse body))))))))


    ;;     ((macro interface instance methods ...)
    ;;      #'(macro interface (instance instance) methods ...))))

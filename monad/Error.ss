(import :std/contract (for-syntax :std/stxutil)
	./syntax ./interface)
(export #t)

(defstruct Error (string irritants)
  transparent: #t
  constructor: :init!)
(defmethod {:init! Error}
  (lambda (err string . irritants)
    (set! (Error-string err) string)
    (set! (Error-irritants err) irritants)
    err))

(defstruct ErrorT (inner type is-error? make-error) constructor: :init!)
(defmethod {:init! ErrorT}
  (lambda (self inner type: (type::t Error::t) is-error?: (pred #f) make-error: (mk #f))
    (unless pred (set! pred (make-struct-predicate type::t)))
    (unless mk (set! mk (cut make-struct-instance type::t <...>)))
    (using (e self : ErrorT)
      (set! e.inner inner)
      (set! e.type type::t)
      (set! e.is-error? pred)
      (set! e.make-error mk)))
   rebind: #t)

(instance Monad (ErrorT et)
  ((return a) (du (i (ErrorT-inner et) : Monad) (i.return a)))
  ((bind ma f)
   (using (et : ErrorT)
     (if (et.is-error? ma) ma (du (i et.inner : Monad) (i.bind ma f))))))

(defsyntax (call-inner stx)
  (syntax-case stx ()
    ((_ (p Type Interface) id args ...)
     (with-syntax* ((inner (format-id #'p "~a.inner" #'p))
		    (iref (format-id #'p "%~a_~a%" #'p #'Interface))
		    (iid (format-id #'id "~a.~a" #'iref #'id)))
       #'(using ((p : Type) (iref inner : Interface)) (iid args ...))))))
 
(instance MonadError (ErrorT et)
  ((throw args) (apply (ErrorT-make-error et) args))
  ((catch ma handler)
   (if ((ErrorT-is-error? et) ma) (handler ma) ma)))

(instance MonadFail (ErrorT et)
  ((fail arg) (call-inner (et ErrorT MonadFail) fail arg)))

(instance MonadPlus (ErrorT et)
  ((zero) (call-inner (et ErrorT MonadPlus) zero))
  ((plus x y) (call-inner (et ErrorT MonadPlus) plus x y)))

(instance MonadState (ErrorT et)
  ((get) (call-inner (et ErrorT MonadState) get))
  ((set thing) (call-inner (et ErrorT MonadState) set thing)))

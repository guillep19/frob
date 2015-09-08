

-- UUAGC 0.9.42.3 (AttributeGrammar.ag)

{-# LINE 2 "./AttributeGrammar.ag" #-}

module AttributeGrammar where
{-# LINE 9 "AttributeGrammar.hs" #-}
-- Decl --------------------------------------------------------
data Decl = Decl_Function (String) (String) (String)
          | Decl_Const (String) (String)
          deriving ( Show)
-- cata
sem_Decl :: Decl ->
            T_Decl
sem_Decl (Decl_Function _name _args _body) =
    (sem_Decl_Function _name _args _body)
sem_Decl (Decl_Const _name _value) =
    (sem_Decl_Const _name _value)
-- semantic domain
type T_Decl = ( String)
sem_Decl_Function :: String ->
                     String ->
                     String ->
                     T_Decl
sem_Decl_Function name_ args_ body_ =
    (let _lhsOcode :: String
         _lhsOcode =
             ({-# LINE 31 "./AttributeGrammar.ag" #-}
              "fun: " ++ name_ ++ "body: TODO" ++ "\n"
              {-# LINE 32 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOcode))
sem_Decl_Const :: String ->
                  String ->
                  T_Decl
sem_Decl_Const name_ value_ =
    (let _lhsOcode :: String
         _lhsOcode =
             ({-# LINE 33 "./AttributeGrammar.ag" #-}
              "const: " ++ name_ ++ "value: \n"
              {-# LINE 43 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOcode))
-- Decls -------------------------------------------------------
data Decls = Decls_Cons (Decl) (Decls)
           | Decls_Nil
           deriving ( Show)
-- cata
sem_Decls :: Decls ->
             T_Decls
sem_Decls (Decls_Cons _hd _tl) =
    (sem_Decls_Cons (sem_Decl _hd) (sem_Decls _tl))
sem_Decls (Decls_Nil) =
    (sem_Decls_Nil)
-- semantic domain
type T_Decls = ( String)
sem_Decls_Cons :: T_Decl ->
                  T_Decls ->
                  T_Decls
sem_Decls_Cons hd_ tl_ =
    (let _lhsOcode :: String
         _hdIcode :: String
         _tlIcode :: String
         _lhsOcode =
             ({-# LINE 37 "./AttributeGrammar.ag" #-}
              _hdIcode ++ _tlIcode
              {-# LINE 69 "AttributeGrammar.hs" #-}
              )
         ( _hdIcode) =
             hd_
         ( _tlIcode) =
             tl_
     in  ( _lhsOcode))
sem_Decls_Nil :: T_Decls
sem_Decls_Nil =
    (let _lhsOcode :: String
         _lhsOcode =
             ({-# LINE 39 "./AttributeGrammar.ag" #-}
              ""
              {-# LINE 82 "AttributeGrammar.hs" #-}
              )
     in  ( _lhsOcode))
-- Root --------------------------------------------------------
data Root = Root_Root (Decls)
          deriving ( Show)
-- cata
sem_Root :: Root ->
            T_Root
sem_Root (Root_Root _decls) =
    (sem_Root_Root (sem_Decls _decls))
-- semantic domain
type T_Root = ( String)
data Inh_Root = Inh_Root {}
data Syn_Root = Syn_Root {code_Syn_Root :: String}
wrap_Root :: T_Root ->
             Inh_Root ->
             Syn_Root
wrap_Root sem (Inh_Root) =
    (let ( _lhsOcode) = sem
     in  (Syn_Root _lhsOcode))
sem_Root_Root :: T_Decls ->
                 T_Root
sem_Root_Root decls_ =
    (let _lhsOcode :: String
         _declsIcode :: String
         _lhsOcode =
             ({-# LINE 27 "./AttributeGrammar.ag" #-}
              _declsIcode
              {-# LINE 111 "AttributeGrammar.hs" #-}
              )
         ( _declsIcode) =
             decls_
     in  ( _lhsOcode))
:Namespace ns1

    ∇ _dont_call_me       
    ⍝ this function should be excluded by ExcludeFns
      ⎕←(⊃⎕XSI),' called'
    ∇ 
     
    ∇ niladic
    ⎕←(⊃⎕XSI),' called'
    ∇

    ∇ r←niladic_result
    ⎕←(⊃⎕XSI),' called'
    r←'niladic_result result'
    ∇

    ∇ monadic rarg
    ⎕←(⊃⎕XSI),' called'
    fld2←rarg
    ∇

    ∇ r←monadic_result rarg
    ⎕←(⊃⎕XSI),' called'
    r←'monadic_result result'
    fld2←rarg
    ∇

    ∇ larg dyadic rarg
    ⎕←(⊃⎕XSI),' called'
    (fld1 fld2)←larg rarg
    ∇

    ∇ r←larg dyadic_result rarg
    ⎕←(⊃⎕XSI),' called'
    (fld1 fld2)←larg rarg
    r←'dyadic_result result'
    ∇

:EndNamespace

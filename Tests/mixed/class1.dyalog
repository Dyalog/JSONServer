:Class class1

    :field public fld1
    :field public fld2←'initial fld2 value'
    :field _p1←'initial _p1 value'

    :property prop1
    :access public
        ∇ r←Get ipa
          r←_p1
        ∇
        ∇ Set ipa
          _p1←ipa.NewValue
        ∇
    :endproperty

    ∇ make
      :Access public
      :Implements constructor
    ∇

    ∇ make1 args
      :Access public
      :Implements constructor
      fld1←args
    ∇

    ∇ niladic
      :Access public
      ⎕←(⊃⎕XSI),' called'
    ∇

    ∇ r←niladic_result
      :Access public
      ⎕←(⊃⎕XSI),' called'
      r←'niladic_result result'
    ∇

    ∇ monadic rarg
      :Access public
      ⎕←(⊃⎕XSI),' called'
      fld2←rarg
    ∇

    ∇ r←monadic_result rarg
      :Access public
      ⎕←(⊃⎕XSI),' called'
      r←'monadic_result result'
      fld2←rarg
    ∇

    ∇ larg dyadic rarg
      :Access public
      ⎕←(⊃⎕XSI),' called'
      (fld1 fld2)←larg rarg
    ∇

    ∇ r←larg dyadic_result rarg
      :Access public
      ⎕←(⊃⎕XSI),' called'
      (fld1 fld2)←larg rarg
      r←'dyadic_result result'
    ∇

:EndClass

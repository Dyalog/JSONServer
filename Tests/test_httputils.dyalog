:Namespace test_httputils

    (⎕IO ⎕ML)←1 1
    
    check←{⍺≡⍵:'' ⋄ (2⊃⎕SI),': Expected [',(1↓,(⎕UCS 13),⍕⍺),'] got [',(1↓,(⎕UCS 13),⍕⍵),']'}

    _true←⊂'true'

:EndNamespace

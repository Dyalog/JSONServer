 ns←GetSignNamed ns
 ⍝ GetSign where input is an object containing month and day as integers
 ⍝         result is object with "sign" injected

 ns.sign←GetSign ns.(month day)

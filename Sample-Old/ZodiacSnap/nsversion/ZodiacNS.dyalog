 response←ZodiacNS ns
 response←⎕NS''
 response.sign←##.Signs⊃⍨1+##.Dates⍸100⊥2↑ns.date

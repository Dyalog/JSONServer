﻿ r←req GetSignWithRequest date;dates;signs
⍝ Compute sign of the Zodiac from a 2-element integer vector containing [Month,Day]
 r←⎕NS''
 signs←13⍴'Capricorn' 'Aquarius' 'Pisces' 'Aries' 'Taurus' 'Gemini' 'Cancer' 'Leo' 'Virgo' 'Libra' 'Scorpio' 'Sagittarius'
 dates←119 218 320 419 520 620 722 822 922 1022 1121 1221
 r.sign←signs⊃⍨1+dates⍸100⊥2↑date
 r.ipAddr←req.PeerAddr
 ∘∘∘
 :If ~0∊⍴req.PeerCert
     r.certSubj←req.PeerCert.Formatted.Subject
 :EndIf

 sign←Zodiac date;dates;signs
 dates←119 218 320 419 520 620 722 822 922 1022 1121 1221
 signs←'Capricorn' 'Aquarius' 'Pisces' 'Aries' 'Taurus' 'Gemini' 'Cancer' 'Leo' 'Virgo' 'Libra' 'Scorpio' 'Sagittarius' 'Capricorn'
 sign←signs⊃⍨1+dates⍸100⊥2↑date

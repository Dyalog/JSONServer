 MakeSALTns
⍝ This program recreates the #.SALT namespace from ⎕SE's namespaces.
⍝ The #.SALT namespace is used when running in a runtime environment
 #.⎕EX'SALT'
 'SALT'#.⎕NS''
 #.SALT.⎕FIX¨⎕SRC¨⎕SE.(SALTUtils SALT Parser UnicodeFile)
⍝ To avoid cross reference between ⎕SE and # we do not use ⎕NS∘⎕OR
 'Dyalog'#.SALT.⎕NS'⎕se.Dyalog.Callbacks'
 #.SALT.Dyalog.⎕FIX ⎕SRC ⎕SE.Dyalog.Utils
 #.SALT.⎕FX(fn+1)↓⎕CR 1⊃⎕SI
 ⎕←'#.SALT namespace recreated'
 →0

fn: ⍝ This code defined in the namespace
 Boot
⍝ Move a local copy of SALT into ⎕SE
 ⎕SE.⎕FIX¨#.SALT.(⎕SRC¨SALTUtils SALT Parser UnicodeFile)

⍝ The Dyalog namespace is a bit more complicated
 ⎕EX'⎕se.Dyalog' ⋄ '⎕SE.Dyalog'⎕NS ⍬

⍝ We cannot do
⍝   'Dyalog'⎕SE.⎕NS ⎕OR'SALT.Dyalog'
⍝ because this will keep a reference to # in ⎕SE
⍝ so we do this:
 ⎕SE.Dyalog.⎕FIX ⎕SRC Dyalog.Utils
 '⎕se.Dyalog.Callbacks'⎕NS ⎕OR'Dyalog.Callbacks'

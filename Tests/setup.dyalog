 r←setup dummy;home
⍝ Setup test
 ⎕IO←⎕ML←1
 r←''
 :Trap 0
     home←##.TESTSOURCE  ⍝ hopefully good enough...
     {}⎕SE.SALT.Load 'HttpCommand'
     {}⎕SE.SALT.Load 'HttpUtils'
     {}#.⎕FIX'file://',home,'test_httputils.dyalog'
 :Else
     r←,⍕⎕DM
 :EndTrap

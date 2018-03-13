 r←setup dummy;home
⍝ Setup test
 ⎕IO←⎕ML←1
 r←''
 :Trap 0
     home←##.TESTSOURCE  ⍝ hopefully good enough...
     {}#.⎕FIX'file://',home,'../../HttpCommand.dyalog'
     {}#.⎕FIX'file://',home,'../../HttpUtils.dyalog'
     {}#.⎕FIX'file://',home,'test_httputils.dyalog'
 :Else
     r←,⍕⎕DM
 :EndTrap

 Updates;t;n;commits
 ⍝ check up to last 5 updates to repository
 :Trap 0
     t←HttpCommand.Get'http://api.github.com/repos/Dyalog/JSONServer/commits'
     n←5⌊≢commits←⎕JSON t.Data ⍝ last commit should be for this workspace
     ⎕←'The last ',(⍕n),' commits to repository http://github.com/Dyalog/JSONServer are:'
     ⎕←1↓∊(⎕UCS 13),¨¨(n↑commits).commit.(((⎕UCS 13),author.date)message)
 :Else
     ⎕←'!! unable to check updates - ',⍕2↑⎕DM
 :EndTrap
